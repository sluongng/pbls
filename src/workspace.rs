use std::collections::hash_map;

use crate::file;

use super::parser::Parser;
use lsp_types::{SymbolInformation, Url};
use tree_sitter::QueryCursor;

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
        Ok(self
            .files
            .get(uri)
            .ok_or(format!("File not loaded: {uri}"))?)
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

        let uri = Url::from_file_path(path.clone()).or(Err(format!("Invalid path {path:?}")))?;
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

    pub fn symbols(&self, uri: &Url) -> Result<Vec<SymbolInformation>> {
        let mut qc = tree_sitter::QueryCursor::new();
        Ok(self
            .get(uri)?
            .symbols(&mut qc)
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
        let mut qc = tree_sitter::QueryCursor::new();
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
            let symbols = file.symbols(&mut qc);
            let syms = symbols.map(|s| to_lsp_symbol(uri.clone(), s));
            res.extend(syms);
        }
        Ok(res)
    }

    pub fn complete(
        &self,
        uri: &Url,
        line: usize,
        character: usize,
    ) -> Result<Option<lsp_types::CompletionResponse>> {
        let file = self
            .files
            .get(uri)
            .ok_or("Completion requested on file with no tree for {uri}")?;
        match file.completion_context(line, character)? {
            Some(file::CompletionContext::Message(msg)) => self.complete_types(&msg, file),
            Some(file::CompletionContext::Enum(_)) => Ok(None), // TODO
            Some(file::CompletionContext::Keyword) => Ok(complete_keywords()),
            Some(file::CompletionContext::Import) => self.complete_imports(uri),
            None => Ok(None),
        }
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
        let ctx = file.type_at(pos.line.try_into()?, pos.character.try_into()?);
        eprintln!("Finding definition for {ctx:?}");
        match ctx {
            None => Ok(None),
            Some(file::GotoContext::Type(name)) => self.find_symbol(uri, file, name),
            Some(file::GotoContext::Import(name)) => {
                eprintln!("Looking up import {name:?}");
                Ok(self.find_import(name).map(|path| lsp_types::Location {
                    uri: Url::from_file_path(path).unwrap(),
                    range: lsp_types::Range::default(),
                }))
            }
        }
    }

    fn find_symbol(
        &self,
        uri: Url,
        file: &file::File,
        name: &str,
    ) -> Result<Option<lsp_types::Location>> {
        // First look within the file.
        let mut qc = tree_sitter::QueryCursor::new();
        if let Some(sym) = file.symbols(&mut qc).find(|s| s.name == name) {
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

        let mut qc = tree_sitter::QueryCursor::new();
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
            if let Some(sym) = file.symbols(&mut qc).find(|sym| sym.name == expected_name) {
                return Ok(Some(lsp_types::Location {
                    uri,
                    range: to_lsp_range(sym.range),
                }));
            }
        }
        Ok(None)
    }

    fn complete_types(
        &self,
        base_name: &str,
        file: &file::File,
    ) -> Result<Option<lsp_types::CompletionResponse>> {
        let current_package = file.package();
        let mut qc = QueryCursor::new();
        let mut items: Vec<_> = file
            .relative_symbols(base_name, &mut qc)
            .map(to_lsp_completion)
            .collect();

        let imports = file
            .imports(&mut qc)
            .filter_map(|name| self.find_import(name))
            .map(|path| Url::from_file_path(path).unwrap())
            .map(|uri| self.get(&uri).unwrap());

        for file in imports {
            let package = file.package();
            if package.is_none() || package == current_package {
                let mut qc = tree_sitter::QueryCursor::new();
                items.extend(file.symbols(&mut qc).map(to_lsp_completion));
            } else if let Some(package) = package {
                let mut qc = tree_sitter::QueryCursor::new();
                items.extend(
                    file.symbols(&mut qc)
                        .map(|s| file::Symbol {
                            name: package.to_owned() + "." + &s.name,
                            ..s
                        })
                        .map(to_lsp_completion),
                );
            }
        }

        let builtins = [
            "bool", "bytes", "double", "fixed32", "fixed64", "float", "int32", "int64", "sfixed32",
            "sfixed64", "sint32", "sint64", "string", "uint32", "uint64",
        ]
        .map(|s| lsp_types::CompletionItem {
            label: s.to_string(),
            kind: Some(lsp_types::CompletionItemKind::STRUCT),
            ..Default::default()
        });
        items.extend(builtins);

        let keywords = [
            "enum", "extend", "import", "message", "oneof", "option", "optional", "package",
            "repeated", "reserved", "returns", "rpc", "service", "stream", "syntax", "to", "map",
        ]
        .map(|s| lsp_types::CompletionItem {
            label: s.to_string(),
            kind: Some(lsp_types::CompletionItemKind::KEYWORD),
            ..Default::default()
        });
        items.extend(keywords);

        Ok(Some(lsp_types::CompletionResponse::Array(items)))
    }

    fn complete_imports(
        &self,
        url: &lsp_types::Url,
    ) -> Result<Option<lsp_types::CompletionResponse>> {
        let items = self
            .available_imports(&url)?
            .map(|s| lsp_types::CompletionItem {
                label: s.clone(),
                label_details: None,
                kind: Some(lsp_types::CompletionItemKind::FILE),
                insert_text: Some(format!("{}\";", s)),
                ..Default::default()
            });
        Ok(Some(lsp_types::CompletionResponse::Array(items.collect())))
    }
}

fn complete_keywords() -> Option<lsp_types::CompletionResponse> {
    let items = ["message", "enum", "import"]
        .iter()
        .map(|s| lsp_types::CompletionItem {
            label: s.to_string(),
            kind: Some(lsp_types::CompletionItemKind::KEYWORD),
            ..Default::default()
        });
    Some(lsp_types::CompletionResponse::Array(items.collect()))
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

fn to_lsp_symbol(uri: Url, sym: file::Symbol) -> lsp_types::SymbolInformation {
    // deprecated field is deprecated, but cannot be omitted
    #[allow(deprecated)]
    lsp_types::SymbolInformation {
        name: sym.name,
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
        container_name: None,
    }
}

fn to_lsp_completion(sym: file::Symbol) -> lsp_types::CompletionItem {
    lsp_types::CompletionItem {
        label: sym.name,
        kind: Some(match sym.kind {
            file::SymbolKind::Enum => lsp_types::CompletionItemKind::ENUM,
            file::SymbolKind::Message => lsp_types::CompletionItemKind::STRUCT,
        }),
        ..Default::default()
    }
}
