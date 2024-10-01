use std::collections::hash_map;
use std::collections::HashSet;
use std::str::FromStr;

use crate::file::{self};

use super::protoc;
use lsp_types::{SymbolInformation, Uri};
use regex::RegexBuilder;
use tree_sitter::QueryCursor;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const OPTIONS: &[&str] = &[
    "cc_enable_arenas",
    "cc_generic_services",
    "csharp_namespace",
    "deprecated",
    "features",
    "go_package",
    "java_generate_equals_and_hash",
    "java_generic_services",
    "java_multiple_files",
    "java_outer_classname",
    "java_package",
    "java_string_check_utf8",
    "objc_class_prefix",
    "optimize_for",
    "php_class_prefix",
    "php_metadata_namespace",
    "php_namespace",
    "py_generic_services",
    "ruby_package",
    "swift_prefix",
];

pub struct Workspace {
    proto_paths: Vec<std::path::PathBuf>,
    files: std::collections::HashMap<Uri, file::File>,
}

impl Workspace {
    pub fn new(proto_paths: Vec<std::path::PathBuf>) -> Workspace {
        let mut sorted_proto_paths = proto_paths.clone();
        sorted_proto_paths.sort();
        Workspace {
            proto_paths: sorted_proto_paths,
            files: hash_map::HashMap::new(),
        }
    }

    fn get(self: &Self, uri: &Uri) -> Result<&file::File> {
        Ok(self
            .files
            .get(uri)
            .ok_or(format!("File not loaded: {uri:?}"))?)
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

        let uri = Uri::from_str(format!("file://{}", path.to_str().unwrap()).as_str())
            .or(Err(format!("Invalid path {path:?}")))?;
        if self.files.contains_key(&uri) {
            return Ok(()); // already parsed
        };

        let text = std::fs::read_to_string(path)?;
        let file = file::File::new(text)?;
        let mut qc = tree_sitter::QueryCursor::new();
        let imports = Vec::from_iter(file.imports(&mut qc).map(str::to_string));
        self.files.insert(uri, file);
        for import in imports {
            self.open_import(import.as_str())?;
        }
        Ok(())
    }

    pub fn open(&mut self, uri: Uri, text: String) -> Result<Vec<lsp_types::Diagnostic>> {
        let diags = protoc::diags(&uri, &text, &self.proto_paths);
        let file = file::File::new(text)?;

        let mut qc = tree_sitter::QueryCursor::new();
        let imports = Vec::from_iter(file.imports(&mut qc).map(str::to_string));

        self.files.insert(uri.clone(), file);

        for import in imports {
            self.open_import(import.as_str())?;
        }

        diags
    }

    pub fn save(&mut self, uri: Uri) -> Result<Vec<lsp_types::Diagnostic>> {
        let file = self.get(&uri)?;
        protoc::diags(&uri, &file.text(), &self.proto_paths)
    }

    pub fn edit(
        &mut self,
        uri: &Uri,
        changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        log::trace!("edit");
        self.files
            .get_mut(uri)
            .ok_or(format!("File not loaded: {uri:?}"))?
            .edit(changes)
            .into()
    }

    pub fn symbols(&self, uri: &Uri) -> Result<Vec<SymbolInformation>> {
        let mut qc = tree_sitter::QueryCursor::new();
        Ok(self
            .get(uri)?
            .symbols(&mut qc)
            .map(|s| to_lsp_symbol(uri.clone(), s))
            .collect())
    }

    pub fn all_symbols(&mut self, query: &str) -> Result<Vec<SymbolInformation>> {
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

        let regexes: std::result::Result<Vec<_>, _> = query
            .split_whitespace()
            .map(|s| {
                RegexBuilder::new(
                    &s.chars()
                        .map(|c| c.to_string())
                        .collect::<Vec<_>>()
                        .join(".*"),
                )
                .case_insensitive(query.chars().all(|c| !c.is_uppercase()))
                .build()
            })
            .collect();
        let regexes = regexes?;
        log::debug!("Searching workspace symbols with patterns: {regexes:?}");

        for path in paths {
            let path = path?;
            let uri = Uri::from_str(format!("file://{}", path.to_str().unwrap()).as_str())
                .or(Err(format!("Invalid path: {path:?}")))?;
            let file = if let Some(file) = self.files.get(&uri) {
                file
            } else {
                let text = std::fs::read_to_string(uri.path().to_string()).expect(format!("Failed to read file {:?}", uri).as_str());
                let file = file::File::new(text)?;
                self.files.insert(uri.clone(), file);
                self.files.get(&uri).unwrap()
            };
            let symbols = file.symbols(&mut qc);
            let syms = symbols
                .filter(|s| regexes.iter().all(|r| r.is_match(&s.name)))
                .map(|s| to_lsp_symbol(uri.clone(), s));
            res.extend(syms);
        }
        Ok(res)
    }

    pub fn complete(
        &self,
        uri: &Uri,
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
            Some(file::CompletionContext::Option) => {
                Ok(Some(lsp_types::CompletionResponse::Array(
                    OPTIONS
                        .iter()
                        .map(|name| lsp_types::CompletionItem {
                            label: name.to_string(),
                            kind: Some(lsp_types::CompletionItemKind::TEXT),
                            ..Default::default()
                        })
                        .collect(),
                )))
            }
            Some(file::CompletionContext::Syntax) => {
                Ok(Some(lsp_types::CompletionResponse::Array(
                    [3, 2]
                        .iter()
                        .map(|n| lsp_types::CompletionItem {
                            label: format!("syntax = \"proto{n}\";"),
                            kind: Some(lsp_types::CompletionItemKind::TEXT),
                            ..Default::default()
                        })
                        .collect(),
                )))
            }
            None => Ok(None),
        }
    }

    // Return the relative paths of proto files under the given dir.

    pub fn goto(&self, uri: Uri, pos: lsp_types::Position) -> Result<Option<lsp_types::Location>> {
        let file = self.get(&uri)?;
        let ctx = file.type_at(pos.line.try_into()?, pos.character.try_into()?);
        log::debug!("Finding definition for {ctx:?}");
        match ctx {
            None => Ok(None),
            Some(file::GotoContext::Type(typ)) => self.find_symbol(uri, file, &typ),
            Some(file::GotoContext::Import(name)) => {
                log::debug!("Looking up import {name:?}");
                Ok(self.find_import(name).map(|path| lsp_types::Location {
                    uri: Uri::from_str(format!("file://{}", path.to_str().unwrap()).as_str())
                        .unwrap(),
                    range: lsp_types::Range::default(),
                }))
            }
        }
    }

    pub fn references(
        &self,
        params: lsp_types::ReferenceParams,
    ) -> Result<Option<Vec<lsp_types::Location>>> {
        let doc = params.text_document_position;
        let file = self.get(&doc.text_document.uri)?;

        let Some(item) = file.type_at(
            doc.position.line.try_into()?,
            doc.position.character.try_into()?,
        ) else {
            return Ok(None);
        };

        let mut res = Vec::new();
        for (uri, file) in self.files.iter() {
            res.extend(
                file.references(&item)
                    .iter()
                    .map(|range| lsp_types::Location {
                        uri: uri.clone(),
                        range: to_lsp_range(*range),
                    }),
            );
        }
        Ok(Some(res))
    }

    fn find_symbol(
        &self,
        uri: Uri,
        file: &file::File,
        typ: &file::GotoTypeContext,
    ) -> Result<Option<lsp_types::Location>> {
        let mut qc = tree_sitter::QueryCursor::new();

        // First look within the file, qualifying the name if it is nested.
        if let Some(sym) = typ.parent.as_ref().and_then(|p| {
            let qualified = format!("{p}.{}", typ.name);
            log::trace!("Searching for {qualified} in {uri:?}");
            file.symbols(&mut qc).find(|sym| sym.name == qualified)
        }) {
            return Ok(Some(lsp_types::Location {
                uri,
                range: to_lsp_range(sym.range),
            }));
        }

        log::trace!("Searching for {} in {uri:?}", typ.name);
        // Next look within the file for the unqualified name.
        if let Some(sym) = file.symbols(&mut qc).find(|s| s.name == typ.name) {
            return Ok(Some(lsp_types::Location {
                uri,
                range: to_lsp_range(sym.range),
            }));
        };

        // If the type is nested, try the fully qualified name
        log::trace!("Searching for {} in {uri:?}", typ.name);
        let mut qc = tree_sitter::QueryCursor::new();
        if let Some(sym) = file.symbols(&mut qc).find(|s| s.name == typ.name) {
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
            .map(|path| {
                Uri::from_str(format!("file://{}", path.to_str().unwrap()).as_str()).unwrap()
            })
            .map(|uri| (uri.clone(), self.get(&uri).unwrap()));

        let mut qc = tree_sitter::QueryCursor::new();
        let local_package = file.package();
        for (uri, file) in imports {
            let package = file.package();
            if let Some(sym) = if package == local_package {
                log::trace!("Searching for {} in {uri:?}", typ.name);
                // same package, match the name without the package prefix
                file.symbols(&mut qc).find(|sym| sym.name == typ.name)
            } else if let Some(package) = package {
                // different package, fully qualify the name
                log::trace!("Stripping {package} from {}", typ.name);
                let qualified = typ
                    .name
                    .strip_prefix(package)
                    .unwrap_or(typ.name)
                    .strip_prefix(".")
                    .unwrap_or(typ.name)
                    .to_string();
                log::trace!("Searching for {} in {uri:?} (different package)", qualified);
                file.symbols(&mut qc).find(|sym| sym.name == qualified)
            } else {
                // target file has no package
                log::trace!("Searching for {} in {uri:?}", typ.name);
                file.symbols(&mut qc).find(|sym| sym.name == typ.name)
            } {
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
            .map(|path| {
                Uri::from_str(format!("file://{}", path.to_str().unwrap()).as_str()).unwrap()
            })
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
            "repeated", "reserved", "returns", "rpc", "service", "stream", "map",
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
        url: &lsp_types::Uri,
    ) -> Result<Option<lsp_types::CompletionResponse>> {
        log::debug!("Completing imports for {url:?}");

        let current = std::path::Path::new(url.path().as_str())
            .file_name()
            .ok_or("Invalid path: {uri}")?
            .to_str()
            .ok_or("Invalid path: {uri}")?;

        let file = self.files.get(url).ok_or("File not loaded: {uri}")?;
        let mut qc = tree_sitter::QueryCursor::new();
        let existing: HashSet<_> =
            HashSet::from_iter(file.imports(&mut qc).chain(std::iter::once(current)));

        log::trace!("Excluding existing imports: {existing:?}");

        let mut last_path = None;
        let items = self
            .proto_paths
            .iter()
            .filter(|pb| {
                if let Some(last_path) = last_path {
                    !pb.starts_with(last_path)
                } else {
                    last_path = Some(pb.to_str().unwrap());
                    true
                }
            })
            .map(|p| find_protos(p.as_path()))
            .flat_map(|p| {
                p.iter()
                    .filter(|p| !existing.contains(&p.as_str()))
                    .map(|s| lsp_types::CompletionItem {
                        insert_text: Some(format!("{}\";", s)),
                        label: s.to_owned(),
                        label_details: None,
                        kind: Some(lsp_types::CompletionItemKind::FILE),
                        ..Default::default()
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        Ok(Some(lsp_types::CompletionResponse::Array(items)))
    }
}

fn find_protos(dir: &std::path::Path) -> Vec<String> {
    let mut res = vec![];
    let entries = match std::fs::read_dir(dir) {
        Ok(ok) => ok,
        Err(err) => {
            log::warn!("Failed to read dir {dir:?}: {err:?}");
            return res;
        }
    };
    log::trace!("Finding imports under {dir:?}");
    for path in entries {
        let path = match path {
            Ok(ok) => ok,
            Err(err) => {
                log::warn!("Failed to read dir {dir:?}: {err:?}");
                continue;
            }
        };

        let meta = match path.metadata() {
            Ok(ok) => ok,
            Err(err) => {
                log::warn!("Failed to read dir {dir:?}: {err:?}");
                continue;
            }
        };

        if meta.is_dir() {
            let dir = dir.join(path.path());
            let protos = find_protos(dir.as_path());
            let root = &path.file_name();
            let root = std::path::PathBuf::from(root);
            res.extend(
                protos
                    .iter()
                    .filter_map(|p| root.join(p).to_str().map(str::to_string)),
            );
            continue;
        }

        if !meta.is_file() {
            continue;
        }

        let name = &path.file_name();
        let Some(name) = name.to_str() else {
            continue;
        };

        if !name.ends_with(".proto") {
            continue;
        }

        log::trace!("Found import {name:?}");
        res.push(name.to_string())
    }
    res
}

fn complete_keywords() -> Option<lsp_types::CompletionResponse> {
    let items = ["message", "enum", "import", "option"]
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

fn to_lsp_symbol(uri: Uri, sym: file::Symbol) -> lsp_types::SymbolInformation {
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn setup() -> (Workspace, tempfile::TempDir) {
        let _ = env_logger::builder().is_test(true).try_init();
        let tmp = tempfile::tempdir().unwrap();
        (Workspace::new(vec![tmp.path().into()]), tmp)
    }

    fn proto(dir: impl AsRef<std::path::Path>, path: &str, lines: &[&str]) -> (Uri, String) {
        let path = dir.as_ref().join(path);
        let text = lines.join("\n") + "\n";
        std::fs::write(&path, &text).unwrap();
        (
            Uri::from_str(format!("file://{}", path.to_str().unwrap()).as_str()).unwrap(),
            text,
        )
    }

    #[test]
    fn test_open_loop() {
        let (mut ws, tmp) = setup();
        let (uri, text) = proto(
            &tmp,
            "foo.proto",
            &["syntax = \"proto3\";", "import \"bar.proto\";"],
        );
        proto(
            &tmp,
            "bar.proto",
            &["syntax = \"proto3\";", "import \"bar.proto\";"],
        );

        ws.open(uri.clone(), text).unwrap();
    }

    #[test]
    fn test_complete_syntax() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut ws = Workspace::new(vec![]);
        let uri = Uri::from_str(
            format!(
                "file://{}",
                std::env::temp_dir().join("foo.proto").to_str().unwrap()
            )
            .as_str(),
        )
        .unwrap();
        // TODO: This returns Error because protoc needs a file on disk.
        // Replace with unwrap after errors are based on treesitter.
        _ = ws.open(uri.clone(), "".into());
        assert_eq!(
            ws.complete(&uri, 0, 0).unwrap().unwrap(),
            lsp_types::CompletionResponse::Array(vec![
                lsp_types::CompletionItem {
                    label: "syntax = \"proto3\";".into(),
                    kind: Some(lsp_types::CompletionItemKind::TEXT),
                    ..Default::default()
                },
                lsp_types::CompletionItem {
                    label: "syntax = \"proto2\";".into(),
                    kind: Some(lsp_types::CompletionItemKind::TEXT),
                    ..Default::default()
                }
            ])
        );
    }

    #[test]
    fn test_complete_import() {
        let (mut ws, tmp) = setup();
        let (uri, text) = proto(
            &tmp,
            "foo.proto",
            &["syntax = \"proto3\";", "import \"bar.proto\";", "import \""],
        );
        proto(&tmp, "bar.proto", &["syntax = \"proto3\";"]);
        proto(&tmp, "baz.proto", &["syntax = \"proto3\";"]);

        ws.open(uri.clone(), text).unwrap();
        assert_eq!(
            ws.complete(&uri, 2, "import \"".len()).unwrap().unwrap(),
            lsp_types::CompletionResponse::Array(vec![lsp_types::CompletionItem {
                label: "baz.proto".into(),
                kind: Some(lsp_types::CompletionItemKind::FILE),
                insert_text: Some("baz.proto\";".into()),
                ..Default::default()
            },])
        );
    }

    #[test]
    fn test_complete_nested_import() {
        let (mut ws, tmp) = setup();
        let (uri, text) = proto(&tmp, "foo.proto", &["syntax = \"proto3\";", "import \""]);
        proto(&tmp, "bar.proto", &["syntax = \"proto3\";"]);

        let subdir = tmp.path().join("subdir");
        let subdir = subdir.as_path();
        std::fs::create_dir(subdir).unwrap();
        proto(subdir, "baz.proto", &["syntax = \"proto3\";"]);

        ws.open(uri.clone(), text).unwrap();
        assert_eq!(
            ws.complete(&uri, 1, "import \"".len()).unwrap().unwrap(),
            lsp_types::CompletionResponse::Array(vec![
                lsp_types::CompletionItem {
                    label: "subdir/baz.proto".into(),
                    kind: Some(lsp_types::CompletionItemKind::FILE),
                    insert_text: Some("subdir/baz.proto\";".into()),
                    ..Default::default()
                },
                lsp_types::CompletionItem {
                    label: "bar.proto".into(),
                    kind: Some(lsp_types::CompletionItemKind::FILE),
                    insert_text: Some("bar.proto\";".into()),
                    ..Default::default()
                },
            ])
        );
    }

    #[test]
    fn test_complete_options() {
        let (mut ws, tmp) = setup();
        let (uri, text) = proto(
            &tmp,
            "foo.proto",
            &["syntax = \"proto3\";", "import \"bar.proto\";", "option j"],
        );
        proto(&tmp, "bar.proto", &["syntax = \"proto3\";"]);
        proto(&tmp, "baz.proto", &["syntax = \"proto3\";"]);

        ws.open(uri.clone(), text).unwrap();
        assert_eq!(
            ws.complete(&uri, 2, "option j".len()).unwrap().unwrap(),
            lsp_types::CompletionResponse::Array(
                OPTIONS
                    .iter()
                    .map(|name| {
                        lsp_types::CompletionItem {
                            label: name.to_string(),
                            kind: Some(lsp_types::CompletionItemKind::TEXT),
                            ..Default::default()
                        }
                    })
                    .collect()
            )
        );
    }

    #[test]
    fn test_goto_import() {
        let (mut ws, tmp) = setup();
        let (foo_uri, text) = proto(
            &tmp,
            "foo.proto",
            &[
                "syntax = \"proto3\";",
                "import \"bar.proto\";",
                "import \"baz.proto\";",
                "import \"biz.proto\";",
            ],
        );
        let (bar_uri, _) = proto(&tmp, "bar.proto", &["syntax = \"proto3\";"]);
        let (baz_uri, _) = proto(&tmp, "baz.proto", &["syntax = \"proto3\";"]);

        ws.open(foo_uri.clone(), text).unwrap();

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 1,
                    character: "import \"bar.".len().try_into().unwrap(),
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: bar_uri,
                range: lsp_types::Range::default(),
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 2,
                    character: "import \"baz".len().try_into().unwrap(),
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: baz_uri,
                range: lsp_types::Range::default(),
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri,
                lsp_types::Position {
                    line: 3,
                    character: "import \"biz".len().try_into().unwrap(),
                }
            )
            .unwrap(),
            None,
        );
    }

    #[test]
    fn test_goto_type() {
        let (mut ws, tmp) = setup();
        let (foo_uri, text) = proto(
            &tmp,
            "foo.proto",
            &[
                "syntax = \"proto3\";",        // 0
                "package main;",               // 1
                "import \"bar.proto\";",       // 2
                "import \"baz.proto\";",       // 3
                "message One {",               // 4
                "message Two {",               // 5
                "enum Three {}",               // 6
                "}",                           // 7
                "Two.Three tt = 1;",           // 8
                "}",                           // 9
                "message Stuff {",             // 10
                "One one = 1;",                // 11
                "One.Two two = 2;",            // 12
                "One.Two.Three three = 3;",    // 13
                "Two nope = 4;",               // 14
                "bar.One bar_one = 5;",        // 15
                "bar.One.Two b1 = 6;",         // 16
                "bar.One.Two.Three b123 = 7;", // 17
                "baz.buz.Baz bazbuz = 8;",     // 18
                "}",                           // 19
            ],
        );
        let (bar_uri, _) = proto(
            &tmp,
            "bar.proto",
            &[
                "syntax = \"proto3\";", // 0
                "package bar;",         // 1
                "message One",          // 2
                "message One {",        // 3
                "message Two {",        // 4
                "enum Three {",         // 5
                "}",                    // 6
                "}",                    // 7
                "}",                    // 8
            ],
        );
        let (baz_uri, _) = proto(
            &tmp,
            "baz.proto",
            &[
                "syntax = \"proto3\";", // 0
                "package baz.buz;",     // 1
                "message Baz{}",        // 2
            ],
        );

        ws.open(foo_uri.clone(), text).unwrap();

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 8,
                    character: "Two.Th".len().try_into().unwrap(),
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: foo_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 6,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 6,
                        character: 13,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 11,
                    character: 0,
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: foo_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 4,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 9,
                        character: 1,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 12,
                    character: "One.".len().try_into().unwrap(),
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: foo_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 5,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 7,
                        character: 1,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 13,
                    character: "One.Two.T".len().try_into().unwrap(),
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: foo_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 6,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 6,
                        character: 13,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 15,
                    character: 0,
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: bar_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 3,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 8,
                        character: 1,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 16,
                    character: 0,
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: bar_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 4,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 7,
                        character: 1,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 17,
                    character: 0,
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: bar_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 5,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 6,
                        character: 1,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 18,
                    character: 2,
                }
            )
            .unwrap(),
            Some(lsp_types::Location {
                uri: baz_uri.clone(),
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 2,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 2,
                        character: 13,
                    },
                },
            })
        );

        assert_eq!(
            ws.goto(
                foo_uri.clone(),
                lsp_types::Position {
                    line: 14,
                    character: 0,
                }
            )
            .unwrap(),
            None,
        );
    }
}
