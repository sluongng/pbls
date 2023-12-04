type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug, PartialEq)]
pub enum SymbolKind {
    Message,
    Enum,
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum CompletionContext {
    Message(String),
    Enum(String),
    Import,
    Keyword,
}

pub struct File {
    tree: tree_sitter::Tree,
    text: String,
    pub package: Option<String>,
    pub imports: Vec<String>,
    pub symbols: Vec<Symbol>,
}

impl File {
    pub fn new(text: String) -> Result<File> {
        // TODO: cache parser/language?
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_proto::language())
            .expect("Error loading proto language");

        let tree = parser.parse(&text, None).ok_or("Parse failed")?;
        eprintln!("Parsed: {}", tree.root_node().to_sexp());

        // TODO: Use node kind IDs

        let bytes = text.as_bytes();

        // Locate imports.
        let query = tree_sitter::Query::new(
            tree_sitter_proto::language(),
            "(package (full_ident (identifier) @ident))",
        )?;
        let mut qc = tree_sitter::QueryCursor::new();
        let imports = qc
            .matches(&query, tree.root_node(), |n| {
                n.utf8_text(bytes).unwrap().as_bytes()
            })
            .filter_map(|m| m.captures.first())
            .filter_map(|c| c.node.utf8_text(text.as_bytes()).ok())
            .map(|s| s.to_string())
            .collect();

        let query = tree_sitter::Query::new(
            tree_sitter_proto::language(),
            "(message (message_name (identifier) @name))",
        )?;
        let mut qc = tree_sitter::QueryCursor::new();
        let syms: Vec<_> = qc
            .matches(&query, tree.root_node(), |n| {
                n.utf8_text(bytes).unwrap().as_bytes()
            })
            .filter_map(|m| m.captures.first())
            .filter_map(|c| c.node.utf8_text(text.as_bytes()).ok())
            .map(|s| s.to_string())
            .collect();

        eprintln!("syms: {syms:?}");

        // Locate all imports
        let mut cursor = tree.walk();

        // Look for (package (full_ident (identifier)))
        let package = tree
            .root_node()
            .named_children(&mut cursor)
            .find(|c| c.kind() == "package")
            .and_then(|c| c.child(1)) // identifier
            .and_then(|c| c.utf8_text(text.as_bytes()).ok())
            .map(|t| t.to_string());

        let symbols = tree
            .root_node()
            .named_children(&mut cursor)
            .filter(|c| c.kind() == "message" || c.kind() == "enum")
            .filter_map(|c| type_name(c, text.as_bytes()).map(|name| (c, name)))
            .map(|(node, name)| Symbol {
                kind: match node.kind() {
                    "message" => SymbolKind::Message,
                    _ => SymbolKind::Enum,
                },
                name,
            })
            .collect();

        Ok(File {
            tree: tree.to_owned(),
            text,
            imports,
            package,
            symbols,
        })
    }

    fn is_import_completion(self: &Self, row: usize, col: usize) -> bool {
        col > "import ".len()
            && self
                .text
                .lines()
                .skip(row)
                .next()
                .map_or(false, |line| line.starts_with("import "))
    }

    fn parent_context(&self, node: Option<tree_sitter::Node>) -> Option<CompletionContext> {
        eprintln!("Check parent {node:?}");
        match node {
            None => None,
            Some(n) if n.kind() == "enum_body" => n
                .parent() // enum
                .and_then(|p| type_name(p, self.text.as_bytes()))
                .and_then(|n| Some(CompletionContext::Enum(n))),
            Some(n) if n.kind() == "message_body" => n
                .parent() // message
                .and_then(|p| type_name(p, self.text.as_bytes()))
                .and_then(|n| Some(CompletionContext::Message(n))),
            Some(n) => self.parent_context(n.parent()),
        }
    }

    pub fn completion_context(self: &Self, row: usize, col: usize) -> Option<CompletionContext> {
        let pos = tree_sitter::Point {
            row: row.try_into().unwrap(),
            column: col.try_into().unwrap(),
        };
        let node = self
            .tree
            .root_node()
            .named_descendant_for_point_range(pos, pos)?;

        eprintln!("Completion {node:?}: {}", node.to_sexp());

        if let Some(parent) = self.parent_context(Some(node)) {
            Some(parent)
        } else if self.is_import_completion(row, col) {
            Some(CompletionContext::Import)
        } else {
            Some(CompletionContext::Keyword)
        }
    }
}

// Get the name of a Enum or Message node.
fn type_name(node: tree_sitter::Node, text: &[u8]) -> Option<String> {
    debug_assert!(node.kind() == "enum" || node.kind() == "message");
    let mut cursor = node.walk();
    let child = node
        .named_children(&mut cursor)
        .find(|c| c.kind() == "message_name" || c.kind() == "enum_name");
    child
        .and_then(|c| c.utf8_text(text).ok())
        .map(|s| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    // Takes a string with '|' characters representing cursors.
    // Returns the string with '|' removed, and the positions of the cursors.
    fn cursors(text: &str) -> (String, Vec<tree_sitter::Point>) {
        let res = text.replace('|', "");
        let cursors = text
            .lines()
            .enumerate()
            .flat_map(|(row, line)| {
                line.match_indices('|')
                    .enumerate()
                    .map(move |(i, (column, _))| tree_sitter::Point {
                        row,
                        // subtract 1 for each '|' before in this row,
                        // as those offset the position of following '|'
                        column: column - i,
                    })
            })
            .collect();
        (res, cursors)
    }

    #[test]
    fn test_package() {
        let text = r#"syntax="proto3"; package main;"#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(file.package, Some("main".into()));

        let text = r#"syntax="proto3"; package main; package other"#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(file.package, Some("main".into()));

        let text = r#"syntax="proto3";"#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(file.package, None);
    }

    #[test]
    fn test_symbols() {
        let text = r#"
            syntax="proto3"; 
            package main;
            message Foo{}
            enum Bar{}
            message Baz{
                int i = 1;
            }
        "#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(
            file.symbols,
            vec![
                Symbol {
                    kind: SymbolKind::Message,
                    name: "Foo".into(),
                },
                Symbol {
                    kind: SymbolKind::Enum,
                    name: "Bar".into(),
                },
                Symbol {
                    kind: SymbolKind::Message,
                    name: "Baz".into(),
                }
            ]
        );
    }

    #[test]
    fn test_wip() {
        let text = r#"
            syntax="proto3"; 
            package main;
            message Foo{message Bar{message Baz{}}}
        "#;
        let file = File::new(text.to_string()).unwrap();
        assert!(false);
    }

    #[test]
    fn test_completion_context() {
        let (source, points) = cursors(
            r#"
            synt|ax = "proto3";

            import "other|.proto";

            message |Foo {
                uint32 i = 1;
                st|ring s = 2;
            }

            message Bar {
                message Buz {
                    |
                }

                Buz b = 1;
                |
            }

            |

            enum Enum {
                ENUM_ONE_TWO = |
                ENUM_TWO_|
                |
            }
            "#,
        );

        assert_eq!(
            points
                .iter()
                .map(|p| File::new(source.clone())
                    .unwrap()
                    .completion_context(p.row, p.column))
                .collect::<Vec<Option<CompletionContext>>>(),
            vec![
                Some(CompletionContext::Keyword),
                Some(CompletionContext::Keyword),
                Some(CompletionContext::Keyword),
                Some(CompletionContext::Message("Foo".into())),
                Some(CompletionContext::Message("Buz".into())),
                Some(CompletionContext::Message("Bar".into())),
                Some(CompletionContext::Keyword),
                Some(CompletionContext::Enum("Enum".into())),
                Some(CompletionContext::Enum("Enum".into())),
                Some(CompletionContext::Enum("Enum".into())),
            ]
        );
    }
}
