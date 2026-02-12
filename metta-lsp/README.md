# MeTTa Language Support for VS Code

Full-featured language support for the [MeTTa](https://metta-lang.dev/) programming language, powered by a Tree-sitter parser and the Language Server Protocol.

## Features

| Feature | Description |
|---------|-------------|
| **Diagnostics** | Real-time syntax error detection and arity validation via Tree-sitter AST analysis |
| **Completions** | Context-aware completions for builtins, keywords, and user-defined symbols |
| **Hover** | Documentation on hover for all MeTTa builtins and user definitions |
| **Go-to-Definition** | Jump to the definition of any user-defined function or variable |
| **Document Symbols** | Outline view of all definitions in the current file |
| **Workspace Symbols** | Search for symbols across all open MeTTa files |
| **Folding** | Code folding for multi-line S-expressions |
| **Code Actions** | Quick-fixes for mismatched parentheses and brackets |

## Supported File Types

- `.metta`

## Requirements

- VS Code 1.75.0 or later
- Node.js 18 or later

## Extension Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `metta.server.debug` | boolean | `false` | Enable debug logging |
| `metta.server.trace` | string | `"off"` | Trace level: `off`, `messages`, or `verbose` |

## Commands

| Command | Description |
|---------|-------------|
| `MeTTa: Restart MeTTa Language Server` | Restart the language server |

## Architecture

This extension uses a client/server architecture:

- **Client**: VS Code extension that launches and communicates with the server
- **Server**: LSP server using Tree-sitter for parsing and AST-based analysis
- **Grammar**: Tree-sitter grammar for MeTTa (`tree-sitter-metta`)

All analysis (diagnostics, symbol extraction, folding) is driven by the Tree-sitter AST — no regex-based parsing.

## Development

```bash
# Install dependencies
npm install

# Build both client and server
npm run build

# Watch mode
npm run watch
```

## License

MIT
