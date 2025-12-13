# MeTTa Language Server Protocol (LSP) for VS Code

A complete Language Server implementation for the **MeTTa** programming language, built with **Node.js**, **vscode-languageserver**, and **Tree-sitter** grammar support.

## Table of Contents

1. [Features](#features)
2. [Project Structure](#project-structure)
3. [Prerequisites](#prerequisites)
4. [Installation & Setup](#installation--setup)
5. [Building the Project](#building-the-project)
6. [Testing Locally](#testing-locally)
7. [Publishing to VS Code Marketplace](#publishing-to-vs-code-marketplace)
8. [Architecture Overview](#architecture-overview)
9. [Advanced Features](#advanced-features)
10. [Best Practices](#best-practices)
11. [Troubleshooting](#troubleshooting)

---

## Features

### Core LSP Features

**Syntax Highlighting** - TextMate grammar + Tree-sitter semantic awareness
**Diagnostics** - Real-time syntax error detection and unmatched paren warnings
**Hover Information** - Symbol type and location hover tooltips
**Go to Definition** - Jump to symbol definitions
**Auto-Completion** - Context-aware completions for keywords and built-in functions
**Document Symbols** - Outline view with function and variable definitions
**Workspace Symbols** - Search symbols across all open files

### Advanced Features (Included)

**Semantic Tokens** - Color code tokens by type (keywords, functions, variables)
**Folding Ranges** - Collapse/expand list structures
**Rename Symbols** - Rename definitions with cross-reference updates
**Incremental Parsing** - Efficient document updates via Tree-sitter caching

---

## Project Structure

```
metta-lsp/
├── client/                     # VS Code extension client
│   ├── src/
│   │   └── extension.ts       # Extension entry point
│   ├── syntaxes/
│   │   └── metta.tmLanguage.json
│   ├── language-configuration.json
│   ├── package.json
│   ├── tsconfig.json
│   └── .vscodeignore
├── server/                     # Language server backend
│   ├── src/
│   │   └── server.ts          # LSP server implementation
│   ├── package.json
│   └── tsconfig.json
├── package.json               # Root monorepo config
├── tsconfig.json             # Root TypeScript config
├── webpack.config.js         # Bundling configuration
├── .eslintrc.json            # Linting rules
└── README.md                 # This file
```

---

## Prerequisites

- **Node.js** 18.0.0 or higher
- **npm** 8.0.0 or higher
- **Visual Studio Code** 1.75.0 or higher
- **Git** for cloning repositories



## Installation & Setup

### 1. Clone and Navigate to Project

```bash
# Project structure is already created at metta-lsp/
cd metta-lsp
```

### 2. Install Dependencies

```bash
# Install root dependencies
npm install

# Install workspace dependencies (client + server)
npm install --workspaces
```

### 3. Build the Project

```bash
npm run build
```

This compiles TypeScript in both client and server directories to `out/` folders.

---

## Building the Project

### Development Build

```bash
# Watch mode for active development
npm run watch

# Builds client and server in parallel, watching for changes
```
---

## Testing Locally

### Setup Test Environment

1. **Open the extension in VS Code (Debug Mode)**

   ```bash
   # From the metta-lsp directory
   code .
   ```

   This opens VS Code with the workspace.

2. **Launch Debug Configuration**

   - Press `F5` to start the extension in debug mode
   - A new VS Code window opens with the extension loaded

  

### Test Features

1. **Open a MeTTa File**

   ```bash
   code test-fixtures/example.metta
   ```

---

## Architecture Overview

### Client-Server Communication

```
┌─────────────────────────────────────────┐
│       VS Code Extension (Client)        │
│  - extension.ts (activates LSP)         │
│  - Manages lifecycle & UI                │
└─────────────────────────────────────────┘
                   ↕ stdio
        LanguageClientOptions
┌─────────────────────────────────────────┐
│     Language Server (Node.js Backend)   │
│  - server.ts (LSP implementation)       │
│  - Tree-sitter parser integration       │
│  - Document management & analysis       │
└─────────────────────────────────────────┘
                   ↕
        ┌─────────────────────┐
        │  Tree-sitter MeTTa  │
        │  Grammar (WASM/.so) │
        └─────────────────────┘
```


## Contributing

Contributions welcome! Please:

1. Fork and create a feature branch
2. Make changes with tests
3. Submit a pull request

---

## License

MIT License - see LICENSE file

---


**Happy coding with MeTTa!** 
