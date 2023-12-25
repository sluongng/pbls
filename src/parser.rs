use std::{collections::HashMap, error::Error};

use lsp_types::{Diagnostic, DiagnosticSeverity, Range, Url};

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub struct Parser {
    proto_paths: Vec<std::path::PathBuf>,
    files: HashMap<Url, Vec<Diagnostic>>,
}

impl Parser {
    pub fn new(proto_paths: Vec<std::path::PathBuf>) -> Parser {
        Parser {
            proto_paths,
            files: HashMap::new(),
        }
    }

    // Parse a single proto file.
    // Returns a list of symbols if the file parsed.
    // Returns a list of diagnostics if the file failed to parse.
    // Either way, the result is cached for re-access via `parse`.
    pub fn reparse(&mut self, uri: Url) -> Result<Vec<Diagnostic>> {
        if uri.scheme() != "file" {
            Err(format!("Unsupported URI scheme {uri}"))?;
        }

        let path = uri
            .to_file_path()
            .or(Err(format!("Failed to normalize URI path: {uri}")))?;

        let mut parser = protobuf_parse::Parser::new();
        // The protoc parser gives more useful and consistent error messages
        parser.protoc();
        parser.capture_stderr();
        parser.input(&path);
        parser.includes(
            self.proto_paths
                .iter()
                .map(|p| std::fs::canonicalize(p).unwrap()),
        );

        let result = match parser.file_descriptor_set() {
            Ok(_) => vec![],
            Err(err) => get_diagnostics(
                err.source()
                    .ok_or(format!("Parse error missing source: {err}"))?,
                std::fs::read_to_string(path)?,
            ),
        };

        self.files.insert(uri, result.clone());

        Ok(result)
    }
}

fn get_diagnostics(err: impl Error, file_contents: String) -> Vec<Diagnostic> {
    // Errors are delineated by literal \n.
    err.to_string()
        .split("\\n")
        .filter_map(|l| parse_diag(l, &file_contents))
        .collect()
}

// Parse a single error line from the protoc parser into a diagnostic.
// Usually each error has a line containing a location, like:
// foo.proto:4:13: "int" is not defined
// Other lines do not contain location info.
// We'll return None to skip these, as usually another line contains the location.
fn parse_diag(diag: &str, file_contents: &String) -> Option<lsp_types::Diagnostic> {
    log::debug!("Parsing diagnostic {diag}");
    let (_, rest) = diag.split_once(".proto:")?;
    let (linestr, rest) = rest.split_once(':')?;
    let (_, msg) = rest.split_once(':')?;
    let msg = msg.strip_suffix(".\"").unwrap_or(msg).replace("\\\"", "\"");

    // Lines from protoc stderr are 1-indexed.
    let lineno = linestr.parse::<u32>().unwrap() - 1;
    let line = file_contents.lines().skip(lineno.try_into().ok()?).next()?;
    let start = line.find(|c: char| !c.is_whitespace()).unwrap_or(0);
    let end = line
        .rfind(|c: char| !c.is_whitespace())
        .map(|c| c + 1) // include the final character
        .unwrap_or(line.len());

    Some(lsp_types::Diagnostic {
        range: Range {
            start: lsp_types::Position {
                line: lineno,
                character: start.try_into().ok()?,
            },
            end: lsp_types::Position {
                line: lineno,
                character: end.try_into().ok()?,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(String::from("pbls")),
        message: msg.trim().into(),
        ..Default::default()
    })
}
