/**
 * MeTTa Language Server
 *
 * Production LSP server for the MeTTa programming language.
 * Uses Tree-sitter for parsing and AST-based analysis throughout:
 *  - Diagnostics (syntax errors + arity validation)
 *  - Completions (builtins + user-defined symbols)
 *  - Hover (builtin docs + definition locations)
 *  - Go-to-definition
 *  - Document / workspace symbols
 *  - Folding ranges
 *  - Code actions (quick-fixes for bracket errors)
 */

import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  InitializeResult,
  TextDocumentSyncKind,
  Range,
  CompletionItem,
  CompletionItemKind,
  Hover,
  Location,
  SymbolKind,
  SymbolInformation,
  DocumentSymbolParams,
  WorkspaceSymbolParams,
  HoverParams,
  CompletionParams,
  DefinitionParams,
  FoldingRangeParams,
  FoldingRange,
  CodeActionParams,
  CodeAction,
  CodeActionKind,
  TextEdit,
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

// Internal modules ----------------------------------------------------------
import { parser } from './parser';
import { getOrParseDocument, invalidateDocument } from './document-cache';
import { analyzeTree } from './diagnostics';
import { isKeyword } from './metta-builtins';
import {
  BUILTIN_COMPLETIONS_DATA,
  getAllBuiltinCompletionItems,
} from './builtins-completion';
import {
  extractFunctionDefs,
  extractVariables,
  extractDollarVariables,
  computeFoldingRanges,
  FunctionDef,
  Variable,
} from './tree-sitter-utils';

// ============================================================================
// Connection & Documents
// ============================================================================

const connection = createConnection();
const documents = new TextDocuments(TextDocument);

// Pre-compute builtin completions once at startup
const builtinCompletionItems = getAllBuiltinCompletionItems();

// ============================================================================
// Server Initialization
// ============================================================================

connection.onInitialize((params) => {
  const hasWorkspaceFolders = !!(
    params.capabilities?.workspace?.workspaceFolders
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      completionProvider: {
        resolveProvider: false,
        triggerCharacters: ['(', ' '],
      },
      hoverProvider: true,
      definitionProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      foldingRangeProvider: true,
      codeActionProvider: true,
    },
  };

  if (hasWorkspaceFolders) {
    result.capabilities.workspace = {
      workspaceFolders: { supported: true, changeNotifications: true },
    };
  }

  return result;
});

connection.onInitialized(() => {
  connection.console.log('MeTTa Language Server initialized.');
});

// ============================================================================
// Helpers
// ============================================================================

/**
 * Get the word under the cursor.
 */
function getWordAtPosition(
  text: string,
  line: number,
  character: number,
): { word: string; range: Range } | null {
  const lines = text.split('\n');
  if (line >= lines.length) return null;

  const lineText = lines[line];
  if (character > lineText.length) character = lineText.length;

  let start = character;
  let end = character;

  while (start > 0 && /[\w\-?!*+/=<>%]/.test(lineText[start - 1])) start--;
  while (end < lineText.length && /[\w\-?!*+/=<>%]/.test(lineText[end])) end++;

  const word = lineText.substring(start, end);
  if (!word) return null;

  return {
    word,
    range: {
      start: { line, character: start },
      end: { line, character: end },
    },
  };
}

/**
 * Build SymbolInformation[] from the AST of a document.
 */
function extractSymbols(doc: TextDocument): SymbolInformation[] {
  const tree = getOrParseDocument(doc, parser);
  const symbols: SymbolInformation[] = [];

  for (const fn of extractFunctionDefs(tree)) {
    symbols.push({
      name: fn.name,
      kind: SymbolKind.Function,
      location: {
        uri: doc.uri,
        range: {
          start: { line: fn.line, character: fn.column },
          end: { line: fn.line, character: fn.column + fn.name.length },
        },
      },
    });
  }

  for (const v of extractVariables(tree)) {
    symbols.push({
      name: v.name,
      kind: SymbolKind.Variable,
      location: {
        uri: doc.uri,
        range: {
          start: { line: v.line, character: v.column },
          end: { line: v.line, character: v.column + v.name.length },
        },
      },
    });
  }

  return symbols;
}

/**
 * Convert diagnostics from the analyser into LSP Diagnostic[].
 */
function computeDiagnostics(doc: TextDocument): Diagnostic[] {
  try {
    const tree = getOrParseDocument(doc, parser);
    return analyzeTree(tree).map((d) => ({
      severity: DiagnosticSeverity.Error,
      range: {
        start: { line: d.start.row, character: d.start.column },
        end: { line: d.end.row, character: d.end.column },
      },
      message: d.message,
      source: 'metta',
    }));
  } catch (err) {
    connection.console.error(`Diagnostics error: ${err}`);
    return [];
  }
}

// ============================================================================
// Document Events
// ============================================================================

documents.onDidOpen((event) => {
  const diagnostics = computeDiagnostics(event.document);
  connection.sendDiagnostics({ uri: event.document.uri, diagnostics });
});

documents.onDidChangeContent((change) => {
  const diagnostics = computeDiagnostics(change.document);
  connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

documents.onDidClose((event) => {
  invalidateDocument(event.document.uri);
  connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] });
});

// ============================================================================
// Completion
// ============================================================================

connection.onCompletion((params: CompletionParams): CompletionItem[] => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const text = document.getText();
  const wordInfo = getWordAtPosition(text, params.position.line, params.position.character);
  const prefix = wordInfo ? wordInfo.word.toLowerCase() : '';

  const completions: CompletionItem[] = [];

  // 1. Builtins (from the single source of truth)
  for (const item of builtinCompletionItems) {
    if (item.label.toLowerCase().startsWith(prefix)) {
      completions.push({ ...item, sortText: `0_${item.label}` });
    }
  }

  // 2. User-defined symbols from current document
  const symbols = extractSymbols(document);
  for (const sym of symbols) {
    if (sym.name.toLowerCase().startsWith(prefix)) {
      completions.push({
        label: sym.name,
        kind: sym.kind === SymbolKind.Function
          ? CompletionItemKind.Function
          : CompletionItemKind.Variable,
        insertText: sym.name,
        detail: sym.kind === SymbolKind.Function
          ? 'User-defined function'
          : 'User-defined variable',
        documentation: `Defined at line ${sym.location.range.start.line + 1}`,
        sortText: `1_${sym.name}`,
      });
    }
  }

  // De-duplicate
  const seen = new Set<string>();
  return completions.filter((c) => {
    if (seen.has(c.label)) return false;
    seen.add(c.label);
    return true;
  });
});

// ============================================================================
// Hover
// ============================================================================

connection.onHover((params: HoverParams): Hover | null => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const wordInfo = getWordAtPosition(
    document.getText(),
    params.position.line,
    params.position.character,
  );
  if (!wordInfo) return null;

  const word = wordInfo.word;

  // Check builtins first
  const builtin = BUILTIN_COMPLETIONS_DATA[word];
  if (builtin) {
    return {
      contents: {
        kind: 'markdown',
        value: `### ${word}\n\n${builtin.detail}\n\n${builtin.documentation}`,
      },
    };
  }

  // Check user-defined symbols
  const symbols = extractSymbols(document);
  const sym = symbols.find((s) => s.name === word)
    ?? symbols.find((s) => s.name.toLowerCase() === word.toLowerCase());

  if (sym) {
    const kind = sym.kind === SymbolKind.Function ? 'function' : 'variable';
    return {
      contents: {
        kind: 'markdown',
        value: `### ${sym.name}\n\nUser-defined ${kind} (line ${sym.location.range.start.line + 1})`,
      },
    };
  }

  return null;
});

// ============================================================================
// Go-to-Definition
// ============================================================================

connection.onDefinition((params: DefinitionParams): Location | null => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const wordInfo = getWordAtPosition(
    document.getText(),
    params.position.line,
    params.position.character,
  );
  if (!wordInfo) return null;

  const word = wordInfo.word;
  const symbols = extractSymbols(document);

  const sym = symbols.find((s) => s.name === word)
    ?? symbols.find((s) => s.name.toLowerCase() === word.toLowerCase());

  return sym ? sym.location : null;
});

// ============================================================================
// Document Symbols
// ============================================================================

connection.onDocumentSymbol((params: DocumentSymbolParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];
  return extractSymbols(document);
});

// ============================================================================
// Workspace Symbols
// ============================================================================

connection.onWorkspaceSymbol((params: WorkspaceSymbolParams): SymbolInformation[] => {
  const query = params.query.toLowerCase();
  const result: SymbolInformation[] = [];

  for (const doc of documents.all()) {
    for (const sym of extractSymbols(doc)) {
      if (sym.name.toLowerCase().includes(query)) {
        result.push(sym);
      }
    }
  }

  return result;
});

// ============================================================================
// Folding Ranges
// ============================================================================

connection.onFoldingRanges((params: FoldingRangeParams): FoldingRange[] => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const tree = getOrParseDocument(document, parser);
  return computeFoldingRanges(tree).map((r) => ({
    startLine: r.startLine,
    endLine: r.endLine,
    kind: 'region',
  }));
});

// ============================================================================
// Code Actions
// ============================================================================

connection.onCodeAction((params: CodeActionParams): CodeAction[] => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const actions: CodeAction[] = [];
  const text = document.getText();
  const lines = text.split('\n');

  for (const diagnostic of params.context.diagnostics) {
    if (diagnostic.message.includes('Unmatched closing')) {
      actions.push({
        title: 'Remove unmatched character',
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        edit: {
          changes: {
            [params.textDocument.uri]: [
              TextEdit.del(Range.create(diagnostic.range.start, {
                line: diagnostic.range.start.line,
                character: diagnostic.range.start.character + 1,
              })),
            ],
          },
        },
      });
    }

    if (diagnostic.message.includes('nclosed') || diagnostic.message.includes('Missing')) {
      const line = lines[diagnostic.range.start.line] || '';
      let closingChar = ')';
      if (diagnostic.message.includes('bracket')) closingChar = ']';
      else if (diagnostic.message.includes('brace')) closingChar = '}';

      actions.push({
        title: `Add missing '${closingChar}'`,
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        edit: {
          changes: {
            [params.textDocument.uri]: [
              TextEdit.insert(
                { line: diagnostic.range.start.line, character: line.length },
                closingChar,
              ),
            ],
          },
        },
      });
    }
  }

  return actions;
});

// ============================================================================
// Configuration
// ============================================================================

connection.onDidChangeConfiguration(() => {
  for (const doc of documents.all()) {
    const diagnostics = computeDiagnostics(doc);
    connection.sendDiagnostics({ uri: doc.uri, diagnostics });
  }
});

// ============================================================================
// Lifecycle
// ============================================================================

documents.listen(connection);
connection.listen();
