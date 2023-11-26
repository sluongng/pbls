# Protobuf Language Server

`pbls` is a [Language Server](https://microsoft.github.io/language-server-protocol/) for [protobuf](https://protobuf.dev/).

# Features

- [x] Diagnostics
- [x] Goto Definition
- [x] Document Symbols
- [x] Workspace Symbols
- [ ] Completion (WIP)
- [ ] Hover
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

`pbls` does not require any configuration.
`pbls` automatically adds every folder in the workspace containing a protobuf file to the import path.
If you need specific import paths for a project, create a ".pbls.toml" file at the workspace root:

```toml
# .pbls.toml
proto_paths=["one", "two/three"]
```

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
