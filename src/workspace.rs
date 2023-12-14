use std::collections::hash_map;

use crate::file;

use super::parser::Parser;
use lsp_types::{SymbolInformation, Url};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub struct Workspace {
    proto_paths: Vec<std::path::PathBuf>,
    parser: Parser,
    files: std::collections::HashMap<Url, file::File>,
}

impl Workspace {
    pub fn new(proto_paths: Vec<std::path::PathBuf>) -> Workspace {
        Workspace {
            proto_paths: proto_paths.clone(),
            parser: Parser::new(proto_paths),
            files: hash_map::HashMap::new(),
        }
    }

    fn get(self: &Self, uri: &Url) -> Result<&file::File> {
        Ok(self.files.get(uri).ok_or("File not loaded: {uri}")?)
    }

    fn find_import(&self, name: &str) -> Option<std::path::PathBuf> {
        self.proto_paths
            .iter()
            .map(|dir| dir.join(name))
            .find(|path| path.exists())
    }

    // Open and parse an imported file if we haven't already
    fn open_import(&mut self, name: &str) -> Result<()> {
        let Some(path) = self.find_import(name) else {
            // TODO: Could generate not-found import diagnostic here, if we stop using protoc
            return Ok(());
        };

        let uri = Url::from_file_path(path.clone()).unwrap();
        if self.files.contains_key(&uri) {
            return Ok(()); // already parsed
        };

        let text = std::fs::read_to_string(path)?;
        let file = file::File::new(text)?;
        let mut qc = tree_sitter::QueryCursor::new();
        for import in file.imports(&mut qc) {
            self.open_import(import)?;
        }
        self.files.insert(uri, file);
        Ok(())
    }

    pub fn open(&mut self, uri: Url, text: String) -> Result<Vec<lsp_types::Diagnostic>> {
        let file = file::File::new(text)?;

        let mut qc = tree_sitter::QueryCursor::new();

        for import in file.imports(&mut qc) {
            self.open_import(import)?;
        }

        self.files.insert(uri.clone(), file);
        self.parser.reparse(uri)
    }

    pub fn save(&mut self, uri: Url, text: String) -> Result<Vec<lsp_types::Diagnostic>> {
        self.open(uri, text)
    }

    pub fn edit(&mut self, uri: &Url, text: String) -> Result<Vec<lsp_types::Diagnostic>> {
        self.open(uri.clone(), text)
    }

    pub fn symbols(&mut self, uri: Url) -> Result<Vec<SymbolInformation>> {
        Ok(self
            .get(&uri)?
            .symbols()
            .iter()
            .map(|s| to_lsp_symbol(uri.clone(), s))
            .collect())
    }

    pub fn all_symbols(&mut self) -> Result<Vec<SymbolInformation>> {
        let paths = self
            .proto_paths
            .iter()
            .filter_map(|p| std::fs::read_dir(p).ok())
            .flatten()
            .filter_map(|p| p.ok())
            .map(|f| f.path())
            .filter(|p| p.is_file() && p.extension().map_or(false, |e| e == "proto"))
            .map(|p| std::fs::canonicalize(p));
        let mut res = vec![];
        for path in paths {
            let path = path?;
            let uri = Url::from_file_path(&path).or(Err(format!("Invalid path: {path:?}")))?;
            let file = if let Some(file) = self.files.get(&uri) {
                file
            } else {
                let text = std::fs::read_to_string(uri.path())?;
                let file = file::File::new(text)?;
                self.files.insert(uri.clone(), file);
                self.files.get(&uri).unwrap()
            };
            let symbols = file.symbols();
            let syms = symbols.iter().map(|s| to_lsp_symbol(uri.clone(), s));
            res.extend(syms);
        }
        Ok(res)
    }

    pub fn completion_context(
        &self,
        uri: &Url,
        line: usize,
        character: usize,
    ) -> Result<Option<file::CompletionContext>> {
        let file = self
            .files
            .get(uri)
            .ok_or("Completion requested on file with no tree for {uri}")?;
        Ok(file.completion_context(line, character))
    }

    // Return all available imports for a given file.
    // Excludes the file itself and any files already imported.
    pub fn available_imports<'a>(
        &'a self,
        uri: &'a Url,
    ) -> Result<impl Iterator<Item = String> + 'a> {
        let name = std::path::Path::new(uri.path())
            .file_name()
            .ok_or("Invalid path: {uri}")?;
        let file = self.files.get(uri).ok_or("File not loaded: {uri}")?;
        let mut qc = tree_sitter::QueryCursor::new();
        let imports = file.imports(&mut qc).collect::<Vec<_>>();
        Ok(self
            .proto_paths
            .iter()
            .filter_map(|dir| std::fs::read_dir(dir).ok())
            .flatten()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.metadata().is_ok_and(|m| m.is_file()))
            .map(|entry| entry.file_name())
            .filter(move |fname| fname != name)
            .filter_map(|fname| fname.into_string().ok())
            .filter(|fname| fname.ends_with(".proto"))
            .filter(move |fname| !imports.contains(&fname.as_str())))
    }

    pub fn definition(
        &self,
        uri: Url,
        pos: lsp_types::Position,
    ) -> Result<Option<lsp_types::Location>> {
        let file = self.get(&uri)?;

        let Some(name) = file.type_at(pos.line.try_into()?, pos.character.try_into()?) else {
            return Ok(None);
        };

        eprintln!("Getting definition for {name}");

        // First look within the file.
        if let Some(sym) = file.symbols().iter().find(|s| s.full_name() == name) {
            return Ok(Some(lsp_types::Location {
                uri,
                range: to_lsp_range(sym.range),
            }));
        };

        // Next look within the file imports.
        let mut qc = tree_sitter::QueryCursor::new();
        let imports = file
            .imports(&mut qc)
            .filter_map(|name| self.find_import(name))
            .map(|path| Url::from_file_path(path).unwrap())
            .map(|uri| (uri.clone(), self.get(&uri).unwrap()));

        let local_package = file.package();
        for (uri, file) in imports {
            let package = file.package();
            let expected_name = if package == local_package {
                name.to_string()
            } else if let Some(package) = package {
                name.strip_prefix(package)
                    .unwrap_or(name)
                    .strip_prefix(".")
                    .unwrap_or(name)
                    .to_string()
            } else {
                name.to_string()
            };
            if let Some(sym) = file
                .symbols()
                .iter()
                .find(|sym| sym.full_name() == expected_name)
            {
                return Ok(Some(lsp_types::Location {
                    uri,
                    range: to_lsp_range(sym.range),
                }));
            }
        }
        Ok(None)
    }
}

fn to_lsp_pos(p: tree_sitter::Point) -> lsp_types::Position {
    lsp_types::Position {
        line: p.row.try_into().unwrap(),
        character: p.column.try_into().unwrap(),
    }
}

fn to_lsp_range(r: tree_sitter::Range) -> lsp_types::Range {
    lsp_types::Range {
        start: to_lsp_pos(r.start_point),
        end: to_lsp_pos(r.end_point),
    }
}

fn to_lsp_symbol(uri: Url, sym: &file::Symbol) -> lsp_types::SymbolInformation {
    // deprecated field is deprecated, but cannot be omitted
    #[allow(deprecated)]
    lsp_types::SymbolInformation {
        name: sym.full_name(),
        kind: match sym.kind {
            file::SymbolKind::Enum => lsp_types::SymbolKind::ENUM,
            file::SymbolKind::Message => lsp_types::SymbolKind::STRUCT,
        },
        tags: None,
        deprecated: None,
        location: lsp_types::Location {
            uri,
            range: lsp_types::Range {
                start: to_lsp_pos(sym.range.start_point),
                end: to_lsp_pos(sym.range.end_point),
            },
        },
        container_name: sym.parent.clone(),
    }
}
