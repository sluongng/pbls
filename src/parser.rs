use std::{collections::HashMap, error::Error};

use lsp_types::{
    Diagnostic, DiagnosticSeverity, Location, Position, Range, SymbolInformation, SymbolKind, Url,
};
use protobuf::descriptor::{source_code_info, DescriptorProto, FileDescriptorProto};

// Field numbers from https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L100-L101
const MESSAGE_TYPE: i32 = 4;
const ENUM_TYPE: i32 = 5;
const NESTED_MESSAGE_TYPE: i32 = 3;
const NESTED_ENUM_TYPE: i32 = 4;

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Clone)]
pub enum ParseResult {
    Syms(Vec<SymbolInformation>),
    Diags(Vec<Diagnostic>),
}

pub struct Parser {
    proto_paths: Vec<String>,
    files: HashMap<Url, ParseResult>,
}

impl Parser {
    pub fn new(proto_paths: Vec<String>) -> Parser {
        Parser {
            proto_paths,
            files: HashMap::new(),
        }
    }

    // Parse a single proto file.
    // Returns a list of symbols if the file parsed.
    // Returns a list of diagnostics if the file failed to parse.
    // Either way, the result is cached for quick access next time.
    pub fn parse(&mut self, uri: Url) -> Result<ParseResult> {
        if uri.scheme() != "file" {
            Err(format!("Unsupported URI scheme {uri}"))?;
        }

        let mut parser = protobuf_parse::Parser::new();
        // The protoc parser gives more useful and consistent error messages
        parser.protoc();
        parser.protoc_extra_args(vec!["--include_source_info"]);
        parser.capture_stderr();
        parser.input(
            uri.to_file_path()
                .or(Err(format!("Failed to normalize URI path: {uri}")))?,
        );
        parser.includes(
            self.proto_paths
                .iter()
                .map(|p| std::fs::canonicalize(p).unwrap()),
        );

        let result = match parser.file_descriptor_set() {
            Ok(fds) => ParseResult::Syms(get_symbols(
                uri.clone(),
                fds.file
                    .first()
                    .ok_or(format!("No results parsing {uri}"))?,
            )),
            Err(err) => ParseResult::Diags(get_diagnostics(
                err.source()
                    .ok_or(format!("Parse error missing source: {err}"))?,
            )),
        };

        self.files.insert(uri, result.clone());

        Ok(result)
    }

    // Parse all proto files found in the proto path, caching all results.
    // Returns a list of symbols across all files that parsed.
    // Parsing failures are ignored.
    pub fn parse_all(&mut self) -> Result<Vec<SymbolInformation>> {
        Ok(self
            .proto_paths
            .to_owned()
            .iter()
            .filter_map(|p| std::fs::read_dir(p).ok())
            .flatten()
            .filter_map(|p| p.ok())
            .map(|f| f.path())
            .filter(|p| p.is_file() && p.extension().map_or(false, |e| e == "proto"))
            .filter_map(|p| std::fs::canonicalize(p).ok())
            .filter_map(|p| Url::from_file_path(p).ok())
            .filter_map(|u| self.parse(u).ok())
            .filter_map(|r| match r {
                ParseResult::Syms(syms) => Some(syms),
                ParseResult::Diags(_) => None,
            })
            .flatten()
            .collect())
    }
}

fn get_diagnostics(err: impl Error) -> Vec<Diagnostic> {
    let mut vec = Vec::<Diagnostic>::new();
    // Errors are delineated by literal \n.
    for diag in err.to_string().split("\\n").filter_map(|l| parse_diag(l)) {
        vec.push(diag);
    }
    vec
}

fn get_symbols(uri: Url, fd: &FileDescriptorProto) -> Vec<SymbolInformation> {
    fd.source_code_info
        .location
        .iter()
        .filter_map(|loc| location_to_symbol(uri.clone(), loc, fd))
        .collect()
}

// Parse a single error line from the protoc parser into a diagnostic.
// Usually each error has a line containing a location, like:
// foo.proto:4:13: "int" is not defined
// Other lines do not contain location info.
// We'll return None to skip these, as usually another line contains the location.
fn parse_diag(line: &str) -> Option<lsp_types::Diagnostic> {
    eprintln!("Parsing diag {line}");
    let (_, rest) = line.split_once(".proto:")?;
    let (linestr, rest) = rest.split_once(':')?;
    let (_, msg) = rest.split_once(':')?;
    let msg = msg.strip_suffix(".\"").unwrap_or(msg).replace("\\\"", "\"");

    let lineno = linestr.parse::<u32>().unwrap();

    Some(lsp_types::Diagnostic {
        range: Range {
            start: lsp_types::Position {
                line: lineno - 1,
                character: 0,
            },
            end: lsp_types::Position {
                line: lineno - 1,
                character: line.len().try_into().unwrap(),
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(String::from("pbls")),
        message: msg.trim().into(),
        ..Default::default()
    })
}

fn location_to_name_kind_nested(
    path: &[i32],
    proto: &DescriptorProto,
) -> Option<(String, SymbolKind)> {
    match path {
        [NESTED_ENUM_TYPE, idx] => Some((
            proto
                .enum_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<EnumMissingName>".into()),
            SymbolKind::ENUM,
        )),
        [NESTED_MESSAGE_TYPE, idx] => Some((
            proto
                .nested_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()?,
            SymbolKind::STRUCT,
        )),
        [NESTED_MESSAGE_TYPE, idx, tail @ ..] => {
            let parent_name = proto
                .nested_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<MessageMissingName>".into());
            let (name, kind) = location_to_name_kind_nested(
                tail,
                proto.nested_type.get(usize::try_from(*idx).ok()?)?,
            )?;
            Some((parent_name + "." + &name, kind))
        }
        _ => None,
    }
}
fn location_to_name_kind(path: &[i32], fd: &FileDescriptorProto) -> Option<(String, SymbolKind)> {
    match path {
        // top-level enum
        [ENUM_TYPE, idx] => Some((
            fd.enum_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<EnumMissingName>".into()),
            SymbolKind::ENUM,
        )),
        // top-level message
        [MESSAGE_TYPE, idx] => Some((
            fd.message_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<MessageMissingName>".into()),
            SymbolKind::STRUCT,
        )),
        // nested type
        [MESSAGE_TYPE, idx, tail @ ..] => {
            let parent_name = fd
                .message_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<MessageMissingName>".into());
            let (name, kind) = location_to_name_kind_nested(
                &tail,
                fd.message_type.get(usize::try_from(*idx).ok()?)?,
            )?;
            Some((parent_name + "." + &name, kind))
        }
        _ => None,
    }
}

fn location_to_symbol(
    uri: Url,
    loc: &source_code_info::Location,
    fd: &FileDescriptorProto,
) -> Option<SymbolInformation> {
    // See https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L1097-L1120
    // The first element is the type, followed by the index of that type in the descriptor.
    let (name, kind) = location_to_name_kind(&loc.path[..], fd)?;

    // Always has exactly three or four elements: start line, start column,
    // end line (optional, otherwise assumed same as start line), end column.
    let (start_line, start_col, end_line, end_col) = match loc.span[..] {
        [start_line, start_col, end_line, end_col] => (start_line, start_col, end_line, end_col),
        [start_line, start_col, end_col] => (start_line, start_col, start_line, end_col),
        _ => None?,
    };

    let start = Position {
        line: start_line.try_into().unwrap(),
        character: start_col.try_into().unwrap(),
    };
    let end = Position {
        line: end_line.try_into().unwrap(),
        character: end_col.try_into().unwrap(),
    };

    // deprecated field is deprecated, but cannot be omitted
    #[allow(deprecated)]
    Some(SymbolInformation {
        name,
        kind,
        location: Location {
            uri: uri.clone(),
            range: Range { start, end },
        },
        container_name: fd.package.clone(),
        tags: None,
        deprecated: None,
    })
}
