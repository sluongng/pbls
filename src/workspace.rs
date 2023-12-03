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
        match self.parser.parse(uri)? {
            ParseResult::Syms(syms) => Ok(syms),
            ParseResult::Diags(_) => Ok(vec![]),
        }
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
        let tree = self
            .files
            .get(uri)
            .ok_or("Completion requested on file with no tree for {uri}")?;
        Ok(tree.completion_context(line, character))
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
        let tree = self.files.get(uri).ok_or("File not loaded: {uri}")?;
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
            .filter(|fname| !tree.imports.contains(fname)))
    }
}
