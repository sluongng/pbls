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

fn enclosing_type<'a>(pos: tree_sitter::Point, source: &'a [u8]) -> Option<&'a str> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_proto::language())
        .expect("Error loading proto language");
    let tree = parser.parse(source, None)?;

    let node = tree.root_node().named_descendant_for_point_range(pos, pos);
    let parent = match node {
        Some(n) if n.kind() == "type" || n.kind() == "message_body" => {
            find_parent(Some(n), "message")
        }
        Some(n)
            if n.kind() == "identifier" && n.parent().map(|p| p.kind()) == Some("enum_field") =>
        {
            find_parent(Some(n), "enum")
        }
        Some(n) if n.kind() == "enum_body" => find_parent(Some(n), "enum"),
        _ => None,
    };

    parent.and_then(|c| node_name(source, c))
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

    // #[test]
    // fn test_cursors() {
    //     assert_eq!(
    //         cursors(
    //             vec!["|foo|bar|", "bizbuz|baz", "bluh|",]
    //                 .join("\n")
    //                 .as_str()
    //         ),
    //         (
    //             "foobar\nbizbuzbaz\nbluh".into(),
    //             vec![
    //                 tree_sitter::Point { row: 0, column: 0 },
    //                 tree_sitter::Point { row: 0, column: 3 },
    //                 tree_sitter::Point { row: 0, column: 6 },
    //                 tree_sitter::Point { row: 1, column: 6 },
    //                 tree_sitter::Point { row: 2, column: 4 }
    //             ]
    //         )
    //     );
    // }

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
                .map(|p| enclosing_type(*p, source.as_bytes()))
                .collect::<Vec<Option<&str>>>(),
            vec![
                None,
                None,
                None,
                Some("Foo"),
                Some("Buz"),
                Some("Bar"),
                None,
                Some("Enum"),
                Some("Enum"),
            ]
        );
    }
}
