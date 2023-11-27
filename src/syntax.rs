type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn node_name<'a>(source: &'a [u8], node: tree_sitter::Node) -> Option<&'a str> {
    let mut cursor = node.walk();
    let name = node
        .named_children(&mut cursor)
        .find(|c| c.kind() == "message_name" || c.kind() == "enum_name");
    name.and_then(|c| c.utf8_text(source).ok())
}

fn find_parent<'a>(
    node: Option<tree_sitter::Node<'a>>,
    kind: &str,
) -> Option<tree_sitter::Node<'a>> {
    match node {
        None => None,
        Some(n) if n.kind() == kind => Some(n),
        Some(n) => find_parent(n.parent(), kind),
    }
}

#[derive(Debug, PartialEq)]
pub enum CompletionContext {
    Message(String),
    Enum(String),
    Import,
}

pub struct Tree {
    tree: tree_sitter::Tree,
    source: Vec<u8>,
}

impl Tree {
    pub fn new(source: &[u8]) -> Result<Tree> {
        // TODO: cache parser/language?
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_proto::language())
            .expect("Error loading proto language");

        Ok(Tree {
            tree: parser.parse(source, None).ok_or("Parse failed")?,
            source: source.into(),
        })
    }

    pub fn completion_context(self: Self, row: usize, col: usize) -> Option<CompletionContext> {
        let pos = tree_sitter::Point {
            row: row.try_into().unwrap(),
            column: col.try_into().unwrap(),
        };
        let node = self
            .tree
            .root_node()
            .named_descendant_for_point_range(pos, pos)?;

        match node.kind() {
            "source_file" => Some(CompletionContext::Import),
            "string" if node.parent().is_some_and(|p| p.kind() == "import") => {
                Some(CompletionContext::Import)
            }
            "type" | "message_body" => find_parent(Some(node), "message")
                .and_then(|n| node_name(self.source.as_slice(), n))
                .map(|name| CompletionContext::Message(name.into())),
            "enum_body" => find_parent(Some(node), "enum")
                .and_then(|n| node_name(self.source.as_slice(), n))
                .map(|name| CompletionContext::Enum(name.into())),
            _ => None,
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

    fn scope<'a>(text: &str) {
        let (source, points) = cursors(text);
        let pos = points.first().unwrap();
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_proto::language())
            .expect("Error loading proto language");
        let tree = parser.parse(source.clone(), None).unwrap();

        let mut node = tree
            .root_node()
            .named_descendant_for_point_range(*pos, *pos);
        // let mut cursor = node.walk();
        let prev = node.unwrap().prev_named_sibling();
        let mut res = vec![];
        while let Some(n) = node {
            res.push(n.kind());
            node = n.parent();
        }
        res.reverse();
        println!("{}: {:?} <- {prev:?}", text, res);
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
                .map(|p| Tree::new(source.as_bytes())
                    .unwrap()
                    .completion_context(p.row, p.column,))
                .collect::<Vec<Option<CompletionContext>>>(),
            vec![
                None,
                Some(CompletionContext::Import),
                None,
                Some(CompletionContext::Message("Foo".into())),
                Some(CompletionContext::Message("Buz".into())),
                Some(CompletionContext::Message("Bar".into())),
                None,
                Some(CompletionContext::Enum("Enum".into())),
                Some(CompletionContext::Enum("Enum".into())),
            ]
        );
    }

    #[test]
    fn test_scope() {
        scope("syntax = \"proto3\"; message Foo{ | }");
        scope("syntax = \"proto3\"; message Foo{ int| }");
        scope("syntax = \"proto3\"; message Foo{ Bar| }");
        scope("syntax = \"proto3\"; message Foo{ Ba|r }");
        scope("syntax = \"proto3\"; message Foo{ Bar | }");
        scope("syntax = \"proto3\"; message Foo{ Ba|r }");
        scope("syntax = \"proto3\"; message Foo{ Bar b| }");
        scope("syntax = \"proto3\"; message Foo{ Bar |b }");
        scope("syntax = \"proto3\"; message Foo{ Bar b | }");
        scope("syntax = \"proto3\"; message Foo{ Bar b =| }");
        scope("syntax = \"proto3\"; message Foo{ Bar b = | }");
        scope("syntax = \"proto3\"; message Foo{ Bar b = |1 }");
        scope("syntax = \"proto3\"; message Foo{ Bar b = 1| }");
        scope("syntax = \"proto3\"; message Foo{ Bar b = 1;| }");
        scope("syntax = \"proto3\"; message Foo{ Bar b = |1; }");
        scope("syntax = \"proto3\"; message Foo{ Bar |b = 1; }");
        scope("syntax = \"proto3\"; message Foo{ Bar b| = 1; }");
        scope("syntax = \"proto3\"; message Foo{ Bar| b = 1; }");
        scope("syntax = \"proto3\"; message Foo{ Ba|r b = 1; }");
        scope("syntax = \"proto3\"; enum Foo{ | }");
        scope("syntax = \"proto3\"; enum Foo{ FOO| }");
        scope("syntax = \"proto3\"; enum Foo{ FOO_ONE = 1|; }");
        scope("syntax = \"proto3\"; |");
        scope("syntax = \"proto3\"; import |");
        scope("syntax = \"proto3\"; impo|rt ");
        scope("syntax = \"proto3\"; import \"|");
        scope("syntax = \"proto3\"; import \"a|");
        scope("syntax = \"proto3\"; import \"|a");
        scope("syntax = \"proto3\"; import |\"");
        scope("syntax = \"proto3\"; import \"t|h");
        scope("syntax = \"proto3\"; import \"th|ing\"");
    }
}
