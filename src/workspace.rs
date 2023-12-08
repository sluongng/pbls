use crate::file;

use super::parser::{ParseResult, Parser};
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
            files: std::collections::HashMap::new(),
        }
    }

    fn get(self: &Self, uri: &Url) -> Result<&file::File> {
        Ok(self.files.get(uri).ok_or("File not loaded: {uri}")?)
    }

    pub fn open(&mut self, uri: Url, text: String) -> Result<ParseResult> {
        self.files.insert(uri.clone(), file::File::new(text)?);
        self.parser.reparse(uri)
    }

    pub fn save(&mut self, uri: Url, text: String) -> Result<ParseResult> {
        self.files.insert(uri.clone(), file::File::new(text)?);
        self.parser.reparse(uri)
    }

    pub fn edit(&mut self, uri: &Url, text: String) -> Result<ParseResult> {
        self.files.insert(uri.clone(), file::File::new(text)?);
        self.parser.reparse(uri.clone())
    }

    pub fn symbols(&mut self, uri: Url) -> Result<Vec<SymbolInformation>> {
        Ok(self
            .get(&uri)?
            .symbols
            .iter()
            .map(|s| to_lsp_symbol(uri.clone(), s))
            .collect())
    }

    pub fn all_symbols(&mut self) -> Result<Vec<SymbolInformation>> {
        Ok(self
            .parser
            .parse_all()
            .iter()
            .flatten()
            .filter_map(|r| match r {
                (_, ParseResult::Syms(syms)) => Some(syms.to_owned()),
                (_, ParseResult::Diags(_)) => None,
            })
            .flatten()
            .collect())
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
        Ok(self
            .proto_paths
            .iter()
            .filter_map(|dir| std::fs::read_dir(dir).ok())
            .flatten()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.metadata().is_ok_and(|m| m.is_file()))
            .filter_map(|entry| entry.file_name().into_string().ok())
            .filter(|fname| fname.ends_with(".proto"))
            .filter(move |fname| fname.as_str() != name)
            .filter(|fname| !file.imports.contains(fname)))
    }
}

fn to_lsp_pos(p: tree_sitter::Point) -> lsp_types::Position {
    lsp_types::Position {
        line: p.row.try_into().unwrap(),
        character: p.column.try_into().unwrap(),
    }
}

fn to_lsp_symbol(uri: Url, sym: &file::Symbol) -> lsp_types::SymbolInformation {
    // deprecated field is deprecated, but cannot be omitted
    #[allow(deprecated)]
    lsp_types::SymbolInformation {
        name: sym.name.clone(),
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
        container_name: if sym.ancestors.is_empty() {
            None
        } else {
            Some(sym.ancestors.join("."))
        },
    }
}
