# Protobuf Language Server

`pbls` is a [Language Server](https://microsoft.github.io/language-server-protocol/) for [protobuf](https://protobuf.dev/).

# Features

- [x] Diagnostics
- [x] Goto Definition
  - [x] Field Type
  - [x] Import
- [x] Document Symbols
- [x] Workspace Symbols
- [ ] Completion (WIP, still has some bugs)
  - [x] Keyword
  - [x] Import
  - [x] Field Type
- [ ] Find References

# Prerequisites

Ensure [`protoc`](https://github.com/protocolbuffers/protobuf#protobuf-compiler-installation) is on your `$PATH`.

# Installation

```
cargo install --git https://git.sr.ht/~rrc/pbls
```

Ensure the cargo binary path (usually `~/.cargo/bin`) is on `$PATH`.
Finally, [configure pbls in your editor](#editor-setup).

# Configuration

Create a file named ".pbls.toml" at your workspace root, and specify the proto import paths that are passed to the `-I`/`--proto_path` flag of `protoc`.
These can be absolute, or local to the workspace.
Make sure to include the "well known" types ("google/protobuf/*.proto").
This is often "/usr/include" on a unix system.

```toml
proto_paths=["some/workspace/path", "/usr/include"]
```

If this is omitted, `pbls` will make a best-effort attempt to add local include paths.
In general, prefer explicitly specifying paths.

## Logging

Set the environment variable `RUST_LOG` to one of ERROR, WARN, INFO, DEBUG, or TRACE.
See [env_logger](https://docs.rs/env_logger/latest/env_logger/#enabling-logging) for more details.

# Editor Setup

This assumes that `pbls` and `protoc` are on your `$PATH`.

## Helix

```toml
# ~/.config/helix/languages.toml

[language-server.pbls]
command = "pbls"

[[language]]
name = "protobuf"
language-servers = ['pbls']
# Unrelated to pbls, you may want to use clang-format as a formatter
formatter = { command = "clang-format" , args = ["--assume-filename=a.proto"]}
```

# Similar Projects

- [buf-language-server](https://github.com/bufbuild/buf-language-server)
- [protocol-buffers-language-server](https://github.com/micnncim/protocol-buffers-language-server)
- [protobuf-language-server](https://github.com/lasorda/protobuf-language-server)
- [pbkit](https://github.com/pbkit/pbkit)
