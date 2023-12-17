# Protobuf Language Server

`pbls` is a [Language Server](https://microsoft.github.io/language-server-protocol/) for [protobuf](https://protobuf.dev/).

# Features

- [x] Diagnostics
- [x] Goto Definition
  - [x] Field Type
  - [x] Import
- [x] Document Symbols
- [x] Workspace Symbols
- [ ] Completion (WIP)
  - [x] Keyword
  - [x] Import
  - [x] Field Type
- [ ] Find References

# Prerequisites

Ensure [`protoc`](https://github.com/protocolbuffers/protobuf#protobuf-compiler-installation) is on your `$PATH`.

# Installation

## From Source

1. Run `cargo install --path .` from the repository root.
2. Optionally, add the cargo binary path (`~/.cargo/bin` by default) to your `$PATH`.
3. Finally, [configure pbls in your editor](#editor-setup).

## Packages

None yet. If you package `pbls` for the distro of your choice, let me know!

# Configuration

Create a file named ".pbls.toml" at your workspace root, and specify the proto import paths that are passed to the `-I`/`--proto_path` flag of `protoc`.

```toml
proto_paths=["one", "two/three"]
```

If this is omitted, `pbls` will automatically add every folder in the workspace containing a protobuf file to the import path. This will not work properly when proto files specify imports containing directories, like `import "foo/bar.proto"`.

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
formatter = { command = "clang-format" } # optional
```

# Similar Projects

- [buf-language-server](https://github.com/bufbuild/buf-language-server)
- [protocol-buffers-language-server](https://github.com/micnncim/protocol-buffers-language-server)
- [protobuf-language-server](https://github.com/lasorda/protobuf-language-server)
- [pbkit](https://github.com/pbkit/pbkit)
