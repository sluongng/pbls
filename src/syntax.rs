fn type_name<'a>(source: &'a [u8], node: tree_sitter::Node) -> Option<&'a str> {
    let mut cursor = node.walk();
    let name = node
        .named_children(&mut cursor)
        .find(|c| c.kind() == "message_name");
    name.and_then(|c| c.utf8_text(source).ok())
}

fn enclosing_type<'a>(
    row: usize,
    column: usize,
    source: &'a [u8],
    tree: &tree_sitter::Tree,
) -> Option<&'a str> {
    let mut child = tree.root_node().named_descendant_for_point_range(
        tree_sitter::Point { row, column },
        tree_sitter::Point { row, column },
    );

    while child.is_some_and(|c| c.kind() != "message") {
        child = child.unwrap().parent();
    }

    child.and_then(|c| type_name(source, c))
}

fn text_and_positions(text: String) -> (String, Vec<tree_sitter::Point>) {
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
fn test_text_and_position() {
    assert_eq!(
        text_and_positions(vec!["|foo|bar|", "bizbuz|baz", "bluh|",].join("\n")),
        (
            "foobar\nbizbuzbaz\nbluh".into(),
            vec![
                tree_sitter::Point { row: 0, column: 0 },
                tree_sitter::Point { row: 0, column: 3 },
                tree_sitter::Point { row: 0, column: 6 },
                tree_sitter::Point { row: 1, column: 6 },
                tree_sitter::Point { row: 2, column: 4 }
            ]
        )
    );
}
