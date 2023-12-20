use std::sync::OnceLock;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn language() -> tree_sitter::Language {
    static LANGUAGE: OnceLock<tree_sitter::Language> = OnceLock::new();
    *LANGUAGE.get_or_init(|| tree_sitter_protobuf::language())
}

#[derive(Debug, PartialEq)]
pub enum SymbolKind {
    Message,
    Enum,
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name: String,
    pub parent: Option<String>,
    pub range: tree_sitter::Range,
}

impl Symbol {
    pub fn full_name(&self) -> String {
        self.parent.as_ref().map_or(self.name.clone(), |p| {
            p.to_string() + "." + self.name.as_str()
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum CompletionContext {
    Message(String),
    Enum(String),
    Import,
    Keyword,
}

#[derive(Debug, PartialEq)]
pub enum GotoContext<'a> {
    Type(&'a str),
    Import(&'a str),
}

pub struct File {
    tree: tree_sitter::Tree,
    text: String,
}

impl File {
    pub fn new(text: String) -> Result<File> {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(language())
            .expect("Error loading proto language");

        let tree = parser.parse(&text, None).ok_or("Parse failed")?;
        log::trace!("Parsed: {}", tree.root_node().to_sexp());
        Ok(File { tree, text })
    }

    fn get_text(&self, node: tree_sitter::Node) -> &str {
        node.utf8_text(self.text.as_bytes()).unwrap()
    }

    pub fn package(&self) -> Option<&str> {
        static QUERY: OnceLock<tree_sitter::Query> = OnceLock::new();
        let query = QUERY.get_or_init(|| {
            tree_sitter::Query::new(language(), "(package (fullIdent (ident) @id))").unwrap()
        });

        let mut qc = tree_sitter::QueryCursor::new();
        let res = qc
            .matches(&query, self.tree.root_node(), self.text.as_bytes())
            .next()
            .map(|m| m.captures[0].node)
            .map(|n| self.get_text(n))
            .map(|s| s.trim_matches('"'));
        res
    }

    pub fn imports<'this: 'cursor, 'cursor>(
        &'this self,
        qc: &'cursor mut tree_sitter::QueryCursor,
    ) -> impl Iterator<Item = &'this str> + 'cursor {
        static QUERY: OnceLock<tree_sitter::Query> = OnceLock::new();
        let query = QUERY.get_or_init(|| {
            tree_sitter::Query::new(language(), "(import (strLit) @path)").unwrap()
        });

        qc.matches(&query, self.tree.root_node(), self.text.as_bytes())
            .map(|m| m.captures[0].node)
            .map(|n| self.get_text(n))
            .map(|s| s.trim_matches('"'))
    }

    pub fn symbols<'this: 'cursor, 'cursor>(
        &'this self,
        qc: &'cursor mut tree_sitter::QueryCursor,
    ) -> impl Iterator<Item = Symbol> + 'cursor {
        static QUERY: OnceLock<tree_sitter::Query> = OnceLock::new();
        let query = QUERY.get_or_init(|| {
            tree_sitter::Query::new(
                language(),
                "[
                     (message (messageName (ident) @id))
                     (enum (enumName (ident) @id))
                 ] @def",
            )
            .unwrap()
        });

        qc.matches(&query, self.tree.root_node(), self.text.as_bytes())
            .map(|m| (m.captures[0].node, m.captures[1].node))
            .map(|(def, id)| Symbol {
                kind: match def.kind() {
                    "message" => SymbolKind::Message,
                    _ => SymbolKind::Enum,
                },
                name: self.get_text(id).into(),
                parent: parent_name(def, self.text.as_bytes()),
                range: def.range(),
            })
    }

    // Given an "ident" or "enumMessageType", node representing a type, find the name of the type.
    fn field_type(&self, node: Option<tree_sitter::Node>) -> Option<&str> {
        log::trace!("Finding type of {node:?}");
        match node {
            None => None,
            Some(n) if n.kind() == "type" => Some(self.get_text(n)),
            Some(n) => self.field_type(n.parent()),
        }
    }

    fn parent_context(&self, node: Option<tree_sitter::Node>) -> Option<CompletionContext> {
        log::trace!(
            "Checking parent context: {node:?} - {}",
            node.map_or("".into(), |n| n.to_sexp())
        );
        match node {
            // Hit the document root
            None => None,
            // Don't complete if we're typing a field name or number
            Some(n) if n.kind() == "fieldName" => None,
            Some(n) if n.kind() == "enumBody" => n
                .parent() // enum
                .and_then(|p| type_name(p, self.text.as_bytes()))
                .and_then(|n| Some(CompletionContext::Enum(n))),
            Some(n) if n.kind() == "messageBody" => n
                .parent() // message
                .and_then(|p| type_name(p, self.text.as_bytes()))
                .and_then(|n| Some(CompletionContext::Message(n))),
            Some(n) => self.parent_context(n.parent()),
        }
    }

    pub fn completion_context(
        self: &Self,
        row: usize,
        col: usize,
    ) -> Result<Option<CompletionContext>> {
        let pos = tree_sitter::Point {
            row: row.try_into().unwrap(),
            // Generally, the node before the cursor is more interesting for context.
            column: (col.checked_sub(1).unwrap_or(0)).try_into()?,
        };
        let node = self
            .tree
            .root_node()
            .named_descendant_for_point_range(pos, pos)
            .ok_or(format!("No descendant for range {pos:?}"))?;

        log::debug!(
            "Getting completion context for node={node:?} prev={:?} parent={:?}",
            node.prev_sibling(),
            node.parent(),
        );

        if node.prev_sibling().is_some_and(|s| s.kind() == "import") {
            Ok(Some(CompletionContext::Import))
        } else if node.kind() == "ident" || node.kind() == "type" {
            Ok(self.parent_context(Some(node)))
        } else if is_top_level_error(node) {
            // typically means we're typing the first word of a line
            Ok(Some(CompletionContext::Keyword))
        } else if node.kind() == "source_file" {
            let line: String = self
                .text
                .lines()
                .skip(row)
                .next()
                .ok_or(format!("Line {row} out of range"))?
                .chars()
                .take(col)
                .collect();

            log::trace!("Checking keyword completion for line {line}");

            if line.trim_start().split(char::is_whitespace).count() <= 1 {
                // first word of the line
                Ok(Some(CompletionContext::Keyword))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    pub fn type_at(self: &Self, row: usize, col: usize) -> Option<GotoContext> {
        log::trace!("Getting type at row: {row} col: {col}");

        let pos = tree_sitter::Point {
            row: row.try_into().unwrap(),
            column: col.try_into().unwrap(),
        };
        let node = self
            .tree
            .root_node()
            .named_descendant_for_point_range(pos, pos)?;

        log::debug!("Getting type at node: {node:?} parent: {:?}", node.parent());

        if node.kind() == "strLit" && node.parent().is_some_and(|p| p.kind() == "import") {
            return Some(GotoContext::Import(self.get_text(node).trim_matches('"')));
        }

        if node.kind() == "ident" || node.kind() == "enumMessageType" {
            if let Some(name) = self.field_type(Some(node)) {
                return Some(GotoContext::Type(name));
            }
        }

        None
    }
}

fn parent_name(node: tree_sitter::Node, text: &[u8]) -> Option<String> {
    let mut node = node;
    let mut res = Vec::<String>::new();
    loop {
        if let Some(parent) = node.parent() {
            if parent.kind() == "message" {
                type_name(parent, text).map(|n| res.push(n));
            }
            node = parent;
        } else {
            break;
        }
    }
    if res.is_empty() {
        None
    } else {
        Some(res.join("."))
    }
}

// Get the name of a Enum or Message node.
fn type_name(node: tree_sitter::Node, text: &[u8]) -> Option<String> {
    debug_assert!(
        node.kind() == "enum" || node.kind() == "message",
        "{node:?}"
    );
    let mut cursor = node.walk();
    let child = node
        .named_children(&mut cursor)
        .find(|c| c.kind() == "messageName" || c.kind() == "enumName");
    child
        .and_then(|c| c.utf8_text(text).ok())
        .map(|s| s.to_string())
}

fn is_top_level_error(node: tree_sitter::Node) -> bool {
    if node.is_error() || node.is_missing() {
        match node.parent() {
            None => true,
            Some(n) if n.kind() == "source_file" => true,
            Some(n) if n.is_error() || n.is_missing() => is_top_level_error(n),
            _ => false,
        }
    } else {
        false
    }
}

// Find the shortest form of a type name relative to a message
// relative_name("Foo", "Foo.Bar.Baz") -> "Bar.Baz"
// relative_name("Foo.Bar", "Foo.Bar.Baz") -> "Baz"
// relative_name("Foo.Bar", "Foo.Bar") -> "Bar"
// relative_name("Foo.Bar", "Foo") -> "Foo"
fn relative_name<'a>(message: &str, name: &'a str) -> String {
    let prefix = name
        .split(".")
        .zip(message.split("."))
        .take_while(|(a, b)| a == b)
        .map(|(a, _)| a)
        .collect::<Vec<_>>()
        .join(".");

    eprintln!("{message}, {name}, {prefix}");

    if prefix.len() == name.len() {
        let Some((_, name)) = name.rsplit_once(".") else {
            return name.to_string();
        };
        name.to_string()
    } else {
        name.strip_prefix(prefix.as_str())
            .unwrap_or(name)
            .strip_prefix(".")
            .unwrap_or(name)
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logger;

    // Takes a string with '|' characters representing cursors.
    // Builds a file from the string with '|' removed, and returns the positions of the cursors.
    fn cursors(text: &str) -> (File, Vec<tree_sitter::Point>) {
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
        (File::new(text.replace('|', "")).unwrap(), cursors)
    }

    // Like cursors, but expect exactly one |
    fn cursor(text: &str) -> (File, tree_sitter::Point) {
        let cursor = text
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
            .next()
            .unwrap();
        (File::new(text.replace('|', "")).unwrap(), cursor)
    }

    #[test]
    fn test_package() {
        logger::init(log::Level::Trace);
        let text = r#"syntax="proto3"; package main;"#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(file.package(), Some("main".into()));

        let text = r#"syntax="proto3"; package main; package other"#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(file.package(), Some("main".into()));

        let text = r#"syntax="proto3";"#;
        let file = File::new(text.to_string()).unwrap();
        assert_eq!(file.package(), None);
    }

    #[test]
    fn test_imports() {
        logger::init(log::Level::Trace);
        let text = r#"
            syntax="proto3";
            package main;
            import "foo.proto";
            import "bar.proto";
            import "ba
        "#;
        let file = File::new(text.to_string()).unwrap();
        let mut qc = tree_sitter::QueryCursor::new();
        assert_eq!(
            file.imports(&mut qc).collect::<Vec<_>>(),
            vec!["foo.proto", "bar.proto"]
        );
    }

    #[test]
    fn test_symbols() {
        logger::init(log::Level::Trace);
        let text = r#"
            syntax="proto3"; 
            package main;
            message Foo{}
            enum Bar{}
            message Baz{
                message Biz{}
            }
        "#;
        let file = File::new(text.to_string()).unwrap();
        let mut qc = tree_sitter::QueryCursor::new();
        assert_eq!(
            file.symbols(&mut qc).collect::<Vec<_>>(),
            vec![
                Symbol {
                    kind: SymbolKind::Message,
                    name: "Foo".into(),
                    parent: None,
                    range: tree_sitter::Range {
                        start_byte: 69,
                        end_byte: 82,
                        start_point: tree_sitter::Point { row: 3, column: 12 },
                        end_point: tree_sitter::Point { row: 3, column: 25 },
                    },
                },
                Symbol {
                    kind: SymbolKind::Enum,
                    name: "Bar".into(),
                    parent: None,
                    range: tree_sitter::Range {
                        start_byte: 95,
                        end_byte: 105,
                        start_point: tree_sitter::Point { row: 4, column: 12 },
                        end_point: tree_sitter::Point { row: 4, column: 22 },
                    },
                },
                Symbol {
                    kind: SymbolKind::Message,
                    name: "Baz".into(),
                    parent: None,
                    range: tree_sitter::Range {
                        start_byte: 118,
                        end_byte: 174,
                        start_point: tree_sitter::Point { row: 5, column: 12 },
                        end_point: tree_sitter::Point { row: 7, column: 13 },
                    },
                },
                Symbol {
                    kind: SymbolKind::Message,
                    name: "Biz".into(),
                    parent: Some("Baz".into()),
                    range: tree_sitter::Range {
                        start_byte: 147,
                        end_byte: 160,
                        start_point: tree_sitter::Point { row: 6, column: 16 },
                        end_point: tree_sitter::Point { row: 6, column: 29 },
                    },
                }
            ]
        );
    }

    #[test]
    fn test_completion_context() {
        logger::init(log::Level::Trace);
        let (file, points) = cursors(
            r#"|
            synt|ax = "proto3";

            import "other|.proto";

            message Foo {
                uint32 i = 1;
                st|ring s = 2;
            }

            message Bar {
                message Buz {
                    b|
                }

                Buz b = 1;
                Ba|
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
                .map(|p| file.completion_context(p.row, p.column).unwrap())
                .collect::<Vec<Option<CompletionContext>>>(),
            vec![
                Some(CompletionContext::Keyword),
                None,
                Some(CompletionContext::Import),
                Some(CompletionContext::Message("Foo".into())),
                Some(CompletionContext::Message("Buz".into())),
                Some(CompletionContext::Message("Bar".into())),
                Some(CompletionContext::Keyword),
                None,
                Some(CompletionContext::Enum("Enum".into())),
                None,
            ]
        );
    }

    #[test]
    fn test_completion_context_import() {
        logger::init(log::Level::Trace);
        let (file, points) = cursors(
            r#"
            syntax = "proto3";
            import "fo|o";
            import "|
            message Foo{}
            "#,
        );

        assert_eq!(
            points
                .iter()
                .map(|p| file.completion_context(p.row, p.column).unwrap())
                .collect::<Vec<_>>(),
            vec![
                Some(CompletionContext::Import),
                Some(CompletionContext::Import),
            ]
        );
    }

    #[test]
    fn test_completion_context_keyword() {
        logger::init(log::Level::Trace);

        fn test(lines: &[&str], expected: Option<CompletionContext>) {
            let text = lines.join("\n");
            let (file, point) = cursor(text.as_str());
            assert_eq!(
                file.completion_context(point.row, point.column).unwrap(),
                expected,
                "text:\n{}",
                text
            );
        }

        // first word of the line
        test(
            &[r#"syntax = "proto3";"#, "|", ""],
            Some(CompletionContext::Keyword),
        );
        test(
            &[r#"syntax = "proto3";"#, "mes|", ""],
            Some(CompletionContext::Keyword),
        );

        // following words of the line
        test(&[r#"syntax = "proto3";"#, "message |", ""], None);
        test(&[r#"syntax = "proto3";"#, "message Fo|", ""], None);
        test(&[r#"syntax = "proto3";"#, "message Foo |", ""], None);

        // new line
        test(
            &[r#"syntax = "proto3";"#, "message Foo{}", "mes|"],
            Some(CompletionContext::Keyword),
        );
    }

    #[test]
    fn test_completion_context_message() {
        logger::init(log::Level::Trace);

        fn test(lines: &[&str], expected: Option<CompletionContext>) {
            let text = format!("syntax = \"proto3\";\n{}\n", lines.join("\n"));
            let (file, point) = cursor(text.as_str());
            assert_eq!(
                file.completion_context(point.row, point.column).unwrap(),
                expected,
                "text:\n{}",
                text
            );
        }

        test(&["message Foo{ | }", ""], None);
        test(
            &["message Foo{ B| }", ""],
            Some(CompletionContext::Message("Foo".into())),
        );
        test(
            &["message Foo{ B|ar }", ""],
            Some(CompletionContext::Message("Foo".into())),
        );
        test(
            &["message Foo{ s|tring }", ""],
            Some(CompletionContext::Message("Foo".into())),
        );
        test(&["message Foo{ Bar | }"], None);
        test(&["message Foo{ Bar b| }"], None);
        test(&["message Foo{ Bar b|ar }"], None);
        test(&["message Foo{ Bar bar |}"], None);
        test(&["message Foo{ Bar bar | }"], None);
        test(&["message Foo{ Bar bar |= }"], None);
        test(&["message Foo{ Bar bar =| }"], None);
        test(&["message Foo{ Bar bar = | }"], None);
        test(&["message Foo{ Bar bar = |1 }"], None);
        test(&["message Foo{ Bar bar = 1| }"], None);
        test(&["message Foo{ Bar bar = 1|; }"], None);
        test(&["message Foo{ Bar bar = 1;| }"], None);
    }

    #[test]
    fn test_type_at() {
        logger::init(log::Level::Trace);
        let (file, points) = cursors(
            r#"
            synt|ax = "proto3";

            import "other|.proto";

            message |Foo {
                st|ring s = 1;
                int|32 i = 2;
                B|ar |b = 3;
                Baz.|Buz b = |4;
                foo.bar|.|Buz.Boz g = 5|;
            }
            "#,
        );

        assert_eq!(
            points
                .iter()
                .map(|p| file.type_at(p.row, p.column))
                .collect::<Vec<Option<GotoContext>>>(),
            vec![
                None,
                Some(GotoContext::Import("other.proto")),
                None,
                None,
                None,
                Some(GotoContext::Type("Bar")),
                None,
                Some(GotoContext::Type("Baz.Buz")),
                None,
                Some(GotoContext::Type("foo.bar.Buz.Boz")),
                Some(GotoContext::Type("foo.bar.Buz.Boz")),
                None,
            ]
        );
    }

    #[test]
    fn test_relative_name() {
        assert_eq!(relative_name("Foo", "Biz.Bar.Baz"), "Biz.Bar.Baz");
        assert_eq!(relative_name("Foo.Bar", "Biz.Bar.Baz"), "Biz.Bar.Baz");
        assert_eq!(relative_name("Foo.Bar.Baz", "Biz.Bar.Baz"), "Biz.Bar.Baz");

        assert_eq!(relative_name("Foo.Bar.Baz", "Foo.Bar.Baz"), "Baz");
        assert_eq!(relative_name("Foo.Bar.Baz", "Foo.Bar"), "Bar");
        assert_eq!(relative_name("Foo.Bar.Baz", "Foo"), "Foo");

        assert_eq!(relative_name("Foo.Bar", "Foo.Bar.Baz"), "Baz");
        assert_eq!(relative_name("Foo.Bar", "Foo.Bar"), "Bar");
        assert_eq!(relative_name("Foo.Bar", "Foo"), "Foo");

        assert_eq!(relative_name("Foo", "Foo.Bar.Baz"), "Bar.Baz");
        assert_eq!(relative_name("Foo", "Foo.Bar"), "Bar");
        assert_eq!(relative_name("Foo", "Foo"), "Foo");
    }
}
