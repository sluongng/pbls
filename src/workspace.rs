use crate::syntax;

use super::parser::{ParseResult, Parser};
use lsp_types::{SymbolInformation, Url};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub struct Workspace {
    parser: Parser,
    trees: std::collections::HashMap<Url, syntax::Tree>,
}

impl Workspace {
    pub fn new(proto_paths: Vec<std::path::PathBuf>) -> Workspace {
        Workspace {
            parser: Parser::new(proto_paths),
            trees: std::collections::HashMap::new(),
        }
    }

    pub fn open(&mut self, uri: Url, text: String) -> Result<ParseResult> {
        self.trees
            .insert(uri.clone(), syntax::Tree::new(text.as_bytes())?);
        self.parser.reparse(uri)
    }

    pub fn save(&mut self, uri: Url, text: String) -> Result<ParseResult> {
        self.trees
            .insert(uri.clone(), syntax::Tree::new(text.as_bytes())?);
        self.parser.reparse(uri)
    }

    pub fn edit(&mut self, uri: &Url, text: String) -> Result<ParseResult> {
        eprintln!("Text: {text}");
        self.trees
            .insert(uri.clone(), syntax::Tree::new(text.as_bytes())?);
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
    ) -> Result<Option<syntax::CompletionContext>> {
        let tree = self
            .trees
            .get(uri)
            .ok_or("Completion requested on file with no tree for {uri}")?;
        Ok(tree.completion_context(line, character))
    }
}
