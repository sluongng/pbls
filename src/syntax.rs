type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug, PartialEq)]
pub enum CompletionContext {
    Message(String),
    Enum(String),
    Import,
    Keyword,
}

pub struct Tree {
    tree: tree_sitter::Tree,
    text: String,
    pub imports: Vec<String>,
}

impl Tree {
    pub fn new(text: String) -> Result<Tree> {
        // TODO: cache parser/language?
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_proto::language())
            .expect("Error loading proto language");

        let tree = parser.parse(&text, None).ok_or("Parse failed")?;
        eprintln!("Parsed: {}", tree.root_node().to_sexp());
        let mut cursor = tree.walk();
        let imports = tree
            .root_node()
            .named_children(&mut cursor)
            .filter(|c| c.kind() == "import")
            .filter_map(|c| c.child_by_field_name("path"))
            .filter_map(|c| c.utf8_text(text.as_bytes()).ok())
            .map(|s| s.trim_matches('"').to_string());

        let imports = imports.collect();
        eprintln!("Updating imports to {imports:?}");

        Ok(Tree {
            tree: tree.to_owned(),
            text,
            imports,
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

    fn node_name(&self, node: tree_sitter::Node) -> Option<String> {
        debug_assert!(node.kind() == "enum" || node.kind() == "message");
        let mut cursor = node.walk();
        let child = node
            .named_children(&mut cursor)
            .find(|c| c.kind() == "message_name" || c.kind() == "enum_name");
        child
            .and_then(|c| c.utf8_text(self.text.as_bytes()).ok())
            .map(|s| s.to_string())
    }

    fn parent_context(&self, node: Option<tree_sitter::Node>) -> Option<CompletionContext> {
        eprintln!("Check parent {node:?}");
        match node {
            None => None,
            Some(n) if n.kind() == "enum_body" => n
                .parent() // enum
                .and_then(|p| self.node_name(p))
                .and_then(|n| Some(CompletionContext::Enum(n))),
            Some(n) if n.kind() == "message_body" => n
                .parent() // message
                .and_then(|p| self.node_name(p))
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
    fn test_enclosing_type() {
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
                .map(|p| Tree::new(source.clone())
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
