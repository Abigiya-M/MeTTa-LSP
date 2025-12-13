import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  InitializeResult,
  ServerCapabilities,
  TextDocumentSyncKind,
  Position,
  Range,
  CompletionItem,
  CompletionItemKind,
  Hover,
  Location,
  SymbolKind,
  SymbolInformation,
  DocumentSymbolParams,
  WorkspaceSymbolParams,
  TextDocumentPositionParams,
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
import * as fs from 'fs';
import * as path from 'path';
import { 
  extractFunctionDefs, 
  extractVariables, 
  extractQuotedSymbols,
  analyzeScope,
  extractListHeads,
  detectErrors,
} from './tree-sitter-utils';

// ============================================================================
// Connection & Documents
// ============================================================================

const connection = createConnection();
const documents = new TextDocuments(TextDocument);
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

// ============================================================================
// Server Initialization
// ============================================================================

connection.onInitialize((params) => {
  hasWorkspaceFolderCapability = !!(params.capabilities && params.capabilities.workspace && !!params.capabilities.workspace.workspaceFolders);
  hasDiagnosticRelatedInformationCapability = !!(params.capabilities && params.capabilities.textDocument && params.capabilities.textDocument.publishDiagnostics && !!params.capabilities.textDocument.publishDiagnostics.relatedInformation);

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      completionProvider: {
        resolveProvider: false,
        triggerCharacters: ['(', ' ', 'd', 'l', 'i', 'm', 'q'],
      },
      hoverProvider: true,
      definitionProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      foldingRangeProvider: true,
      codeActionProvider: true,
    },
  };

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
        changeNotifications: true,
      },
    };
  }

  return result;
});

connection.onInitialized(() => {
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log('Workspace folder change event received.');
    });
  }
});

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Check if a symbol is a keyword
 */
function isKeyword(text: string): boolean {
  const keywords = ['defn', 'match', 'if', 'then', 'else', 'let', 'in', 'atom', 'type', 'import', 'define', 'defun', 'lambda', 'quote', 'eval', 'module', 'fn'];
  return keywords.includes(text.toLowerCase());
}

/**
 * Extract all symbols (atoms, definitions, etc.) from text using tree-sitter utils
 */
function extractSymbols(text: string, uri: string): SymbolInformation[] {
  const symbols: SymbolInformation[] = [];
  const functions = extractFunctionDefs(text);
  const variables = extractVariables(text);

  functions.forEach((func) => {
    symbols.push({
      name: func.name,
      kind: SymbolKind.Function,
      location: {
        uri: uri,
        range: {
          start: { line: func.line, character: func.column },
          end: { line: func.line, character: func.column + func.name.length },
        },
      },
    });
  });

  variables.forEach((variable) => {
    symbols.push({
      name: variable.name,
      kind: SymbolKind.Variable,
      location: {
        uri: uri,
        range: {
          start: { line: variable.line, character: variable.column },
          end: { line: variable.line, character: variable.column + variable.name.length },
        },
      },
    });
  });

  return symbols;
}

/**
 * Get word at position
 */
function getWordAtPosition(text: string, line: number, character: number): { word: string; range: Range } | null {
  const lines = text.split('\n');
  if (line >= lines.length) return null;

  const lineText = lines[line];
  if (character > lineText.length) character = lineText.length;

  let start = character;
  let end = character;

  // Find word boundaries (include both alphanumerics and hyphens for Lisp identifiers)
  while (start > 0 && /[\w-]/.test(lineText[start - 1])) start--;
  while (end < lineText.length && /[\w-]/.test(lineText[end])) end++;

  const word = lineText.substring(start, end);

  return word ? {
    word,
    range: {
      start: { line, character: start },
      end: { line, character: end },
    },
  } : null;
}

/**
 * Parse MeTTa code for diagnostics using AST-based analysis
 */
function parseMeTTa(text: string): Diagnostic[] {
  const errors = detectErrors(text);
  
  // Convert syntax errors to diagnostics
  const diagnostics: Diagnostic[] = errors.map((error) => ({
    severity: error.type === 'error' ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
    range: {
      start: { line: error.line, character: error.column },
      end: { line: error.line, character: error.column + 1 },
    },
    message: error.message,
    source: 'metta',
  }));

  // Note: Semantic diagnostics for undefined references are disabled because MeTTa is a dynamic language
  // and symbols can be provided at runtime. Full scope analysis would require more sophisticated parsing.

  return diagnostics;
}

/**
 * Compute folding ranges for code sections
 */
function computeFoldingRanges(text: string): FoldingRange[] {
  const ranges: FoldingRange[] = [];
  const lines = text.split('\n');
  const stack: { startLine: number; depth: number }[] = [];

  lines.forEach((line, lineIndex) => {
    let depth = 0;
    for (let i = 0; i < line.length; i++) {
      const char = line[i];
      if (char === '(') {
        depth++;
        stack.push({ startLine: lineIndex, depth });
      } else if (char === ')' && stack.length > 0) {
        const start = stack.pop();
        if (start && start.startLine < lineIndex && lineIndex - start.startLine > 1) {
          // Only fold multi-line expressions
          ranges.push({
            startLine: start.startLine,
            endLine: lineIndex,
            kind: 'region',
          });
        }
      }
    }
  });

  return ranges;
}

// ============================================================================
// Document Events
// ============================================================================

documents.onDidOpen((event) => {
  connection.console.log(`Document opened: ${event.document.uri}`);
  const diagnostics = parseMeTTa(event.document.getText());
  connection.sendDiagnostics({ uri: event.document.uri, diagnostics });
});

documents.onDidChangeContent((change) => {
  const diagnostics = parseMeTTa(change.document.getText());
  connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

// ============================================================================
// Completion
// ============================================================================

const mettaBuiltins = [
  { label: 'defn', detail: 'Define a function', documentation: 'Syntax: (defn name (params...) body...)\nDefines a named function with parameters and a body that gets executed when called.' },
  { label: 'defun', detail: 'Define a function (alias for defn)', documentation: 'Syntax: (defun name (params...) body...)\nAlternative syntax for defining functions. Equivalent to defn.' },
  { label: 'define', detail: 'Define a variable', documentation: 'Syntax: (define name value)\nBinds a name to a value in the current scope.' },
  { label: 'match', detail: 'Pattern matching construct', documentation: 'Syntax: (match expr (pattern body)...)\nMatches an expression against multiple patterns and executes the body of the first matching pattern.' },
  { label: 'if', detail: 'Conditional expression', documentation: 'Syntax: (if condition then-expr else-expr)\nEvaluates condition and returns then-expr if true, else-expr if false.' },
  { label: 'then', detail: 'Then branch of conditional', documentation: 'Used in if expressions to denote the true branch: (if cond then-expr else-expr)' },
  { label: 'else', detail: 'Else branch of conditional', documentation: 'Used in if expressions to denote the false branch: (if cond then-expr else-expr)' },
  { label: 'let', detail: 'Local binding construct', documentation: 'Syntax: (let ((var val)...) body...)\nCreates local variable bindings available only within the body.' },
  { label: 'let*', detail: 'Sequential local bindings', documentation: 'Syntax: (let* ((var val)...) body...)\nLike let, but bindings are sequential so later bindings can reference earlier ones.' },
  { label: 'in', detail: 'In expression continuation', documentation: 'Used with let bindings: (let ((x 10)) (in (+ x 1)))' },
  { label: 'atom', detail: 'Atom literal or type check', documentation: 'Syntax: (atom value)\nCreates an atom literal or checks if a value is an atom.' },
  { label: 'type', detail: 'Type annotation', documentation: 'Syntax: (type expr type-spec)\nAnnotates the type of an expression.' },
  { label: 'import', detail: 'Import a module', documentation: 'Syntax: (import module-name)\nImports definitions from a module.' },
  { label: 'module', detail: 'Module definition', documentation: 'Syntax: (module name exports...)\nDefines a module with exported symbols.' },
  { label: 'fn', detail: 'Function (lambda shorthand)', documentation: 'Syntax: (fn (params...) body...)\nDefines an anonymous function. Equivalent to lambda.' },
  { label: 'lambda', detail: 'Anonymous function', documentation: 'Syntax: (lambda (params...) body...)\nCreates an anonymous function that can be passed as a value or assigned to variables.' },
  { label: 'quote', detail: 'Quote expression (prevent evaluation)', documentation: 'Syntax: (quote expr) or \'expr\nPrevents the quoted expression from being evaluated, treating it as data.' },
  { label: 'quasiquote', detail: 'Quasiquote (controlled evaluation)', documentation: 'Syntax: (quasiquote expr)\nLike quote but allows selective evaluation of unquoted parts.' },
  { label: 'unquote', detail: 'Unquote within quasiquote', documentation: 'Syntax: (unquote expr)\nWithin a quasiquote, evaluates the expression.' },
  { label: 'eval', detail: 'Evaluate expression', documentation: 'Syntax: (eval expr)\nEvaluates an expression at runtime.' },
  { label: 'cond', detail: 'Multiple conditional branches', documentation: 'Syntax: (cond (test body)...)\nEvaluates test expressions in sequence and executes body of first true test.' },
  { label: 'case', detail: 'Switch-like pattern matching', documentation: 'Syntax: (case expr (pattern body)...)\nMatches expr against patterns similar to match.' },
  { label: 'and', detail: 'Logical AND', documentation: 'Syntax: (and expr...)\nReturns true if all expressions are true, short-circuits on false.' },
  { label: 'or', detail: 'Logical OR', documentation: 'Syntax: (or expr...)\nReturns true if any expression is true, short-circuits on true.' },
  { label: 'not', detail: 'Logical NOT', documentation: 'Syntax: (not expr)\nReturns the boolean negation of expr.' },
  { label: '=', detail: 'Equality comparison', documentation: 'Syntax: (= expr1 expr2 ...)\nReturns true if all arguments are equal.' },
  { label: '==', detail: 'Strict equality check', documentation: 'Syntax: (== expr1 expr2)\nChecks if two expressions are strictly equal.' },
  { label: '!=', detail: 'Not equal', documentation: 'Syntax: (!= expr1 expr2)\nReturns true if expressions are not equal.' },
  { label: '+', detail: 'Addition', documentation: 'Syntax: (+ num1 num2 ...)\nAdds all numeric arguments.' },
  { label: '-', detail: 'Subtraction or negation', documentation: 'Syntax: (- num1 num2 ...)\nSubtracts subsequent numbers from the first, or negates if single argument.' },
  { label: '*', detail: 'Multiplication', documentation: 'Syntax: (* num1 num2 ...)\nMultiplies all numeric arguments.' },
  { label: '/', detail: 'Division', documentation: 'Syntax: (/ num1 num2)\nDivides num1 by num2.' },
  { label: '%', detail: 'Modulo (remainder)', documentation: 'Syntax: (% num1 num2)\nReturns the remainder of num1 divided by num2.' },
  { label: '<', detail: 'Less than', documentation: 'Syntax: (< num1 num2 ...)\nReturns true if arguments are in strictly increasing order.' },
  { label: '>', detail: 'Greater than', documentation: 'Syntax: (> num1 num2 ...)\nReturns true if arguments are in strictly decreasing order.' },
  { label: '<=', detail: 'Less than or equal', documentation: 'Syntax: (<= num1 num2 ...)\nReturns true if arguments are in non-decreasing order.' },
  { label: '>=', detail: 'Greater than or equal', documentation: 'Syntax: (>= num1 num2 ...)\nReturns true if arguments are in non-increasing order.' },
  { label: 'car', detail: 'Get first element of list', documentation: 'Syntax: (car list)\nReturns the first element (head) of a list.' },
  { label: 'cdr', detail: 'Get rest of list', documentation: 'Syntax: (cdr list)\nReturns the rest (tail) of a list, everything after the first element.' },
  { label: 'cons', detail: 'Construct list', documentation: 'Syntax: (cons elem list)\nConstructs a new list by prepending elem to the front of list.' },
  { label: 'list', detail: 'Create a list', documentation: 'Syntax: (list elem...)\nCreates a list containing the given elements.' },
  { label: 'append', detail: 'Concatenate lists', documentation: 'Syntax: (append list1 list2 ...)\nConcatenates multiple lists into one.' },
  { label: 'length', detail: 'Get list length', documentation: 'Syntax: (length list)\nReturns the number of elements in a list.' },
  { label: 'null?', detail: 'Check if empty list', documentation: 'Syntax: (null? expr)\nReturns true if expr is an empty list ().' },
  { label: 'atom?', detail: 'Check if atom', documentation: 'Syntax: (atom? expr)\nReturns true if expr is an atom (not a list).' },
  { label: 'number?', detail: 'Check if number', documentation: 'Syntax: (number? expr)\nReturns true if expr is a number.' },
  { label: 'string?', detail: 'Check if string', documentation: 'Syntax: (string? expr)\nReturns true if expr is a string.' },
  { label: 'symbol?', detail: 'Check if symbol', documentation: 'Syntax: (symbol? expr)\nReturns true if expr is a symbol.' },
  { label: 'list?', detail: 'Check if list', documentation: 'Syntax: (list? expr)\nReturns true if expr is a list.' },
  { label: 'print', detail: 'Print to output', documentation: 'Syntax: (print expr)\nPrints the expression to standard output.' },
  { label: 'display', detail: 'Display value', documentation: 'Syntax: (display expr)\nDisplays the value in a human-readable format.' },
  { label: 'newline', detail: 'Print newline', documentation: 'Syntax: (newline)\nPrints a newline character.' },
];

connection.onCompletion((params: CompletionParams): CompletionItem[] => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const position = params.position;
  const text = document.getText();
  const wordInfo = getWordAtPosition(text, position.line, position.character);

  if (!wordInfo) return [];

  const word = wordInfo.word.toLowerCase();

  const completions: CompletionItem[] = [];

  // Add built-in keywords and functions
  const builtinCompletions = mettaBuiltins
    .filter((builtin) => builtin.label.toLowerCase().startsWith(word))
    .map((builtin) => ({
      label: builtin.label,
      kind: isKeyword(builtin.label) ? CompletionItemKind.Keyword : CompletionItemKind.Function,
      insertText: builtin.label,
      detail: builtin.detail,
      documentation: builtin.documentation,
      sortText: `1_${builtin.label}`, // Sort builtins first
    }));

  completions.push(...builtinCompletions);

  // Add user-defined symbols from the current document
  const symbols = extractSymbols(text, document.uri);
  const userCompletions = symbols
    .filter((symbol) => symbol.name.toLowerCase().startsWith(word))
    .map((symbol) => ({
      label: symbol.name,
      kind: symbol.kind === SymbolKind.Function ? CompletionItemKind.Function : CompletionItemKind.Variable,
      insertText: symbol.name,
      detail: symbol.kind === SymbolKind.Function ? 'User-defined function' : 'User-defined variable',
      documentation: `Defined at line ${symbol.location.range.start.line + 1}`,
      sortText: `2_${symbol.name}`, // Sort user symbols after builtins
    }));

  completions.push(...userCompletions);

  // Remove duplicates (builtins might override user-defined names)
  const seen = new Set<string>();
  return completions.filter((item) => {
    if (seen.has(item.label)) return false;
    seen.add(item.label);
    return true;
  });
});

// ============================================================================
// Hover
// ============================================================================

connection.onHover((params: HoverParams): Hover | null => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const position = params.position;
  const text = document.getText();
  const wordInfo = getWordAtPosition(text, position.line, position.character);

  if (!wordInfo) return null;

  const word = wordInfo.word;

  // Check if it's a builtin
  const builtin = mettaBuiltins.find((b) => b.label === word);
  if (builtin) {
    return {
      contents: {
        language: 'markdown',
        value: `### ${word}\n\n${builtin.detail}\n\n${builtin.documentation || ''}`,
      },
    };
  }

  // Check if it's a defined symbol
  const symbols = extractSymbols(text, document.uri);
  const symbol = symbols.find((s) => s.name === word);

  if (symbol) {
    return {
      contents: {
        language: 'markdown',
        value: `### ${symbol.name}\n\nDefined function (line ${symbol.location.range.start.line + 1})`,
      },
    };
  }

  return null;
});

// ============================================================================
// Go-to-Definition
// ============================================================================

connection.onDefinition((params: DefinitionParams): Location | Location[] | null => {
  try {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
      connection.console.log(`[Definition] Document not found: ${params.textDocument.uri}`);
      return null;
    }

    const position = params.position;
    const text = document.getText();
    
    connection.console.log(`[Definition] Request at line ${position.line}, char ${position.character}`);

    const wordInfo = getWordAtPosition(text, position.line, position.character);

    if (!wordInfo) {
      connection.console.log(`[Definition] Could not extract word at position`);
      return null;
    }

    const word = wordInfo.word;
    connection.console.log(`[Definition] Extracted word: "${word}"`);
    
    const symbols = extractSymbols(text, document.uri);
    connection.console.log(`[Definition] Extracted ${symbols.length} symbols from document`);
    
    // Try exact match first
    let symbol = symbols.find((s) => s.name === word);
    
    // Try case-insensitive match as fallback
    if (!symbol) {
      symbol = symbols.find((s) => s.name.toLowerCase() === word.toLowerCase());
    }
    
    if (symbol) {
      connection.console.log(`[Definition] ✓ Found "${word}" at line ${symbol.location.range.start.line}`);
      return symbol.location;
    }
    
    connection.console.log(`[Definition] ✗ Symbol "${word}" not found. Available: ${symbols.map(s => s.name).join(', ')}`);
    return null;
  } catch (error) {
    connection.console.log(`[Definition] ERROR: ${error}`);
    return null;
  }
});

// ============================================================================
// Document Symbols
// ============================================================================

connection.onDocumentSymbol((params: DocumentSymbolParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  return extractSymbols(document.getText(), document.uri);
});

// ============================================================================
// Workspace Symbols
// ============================================================================

connection.onWorkspaceSymbol((params: WorkspaceSymbolParams): SymbolInformation[] => {
  const symbols: SymbolInformation[] = [];

  documents.all().forEach((doc) => {
    const docSymbols = extractSymbols(doc.getText(), doc.uri);
    const filtered = docSymbols.filter((s) => s.name.toLowerCase().includes(params.query.toLowerCase()));
    symbols.push(...filtered);
  });

  return symbols;
});

// ============================================================================
// Code Folding
// ============================================================================

connection.onFoldingRanges((params: FoldingRangeParams): FoldingRange[] => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  return computeFoldingRanges(document.getText());
});

// ============================================================================
// Code Actions
// ============================================================================

connection.onCodeAction((params: CodeActionParams): CodeAction[] => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const actions: CodeAction[] = [];
  const text = document.getText();
  const diagnostics = params.context.diagnostics;

  // Provide quick fixes for unmatched brackets/parentheses
  diagnostics.forEach((diagnostic) => {
    if (diagnostic.message.includes('Unmatched closing')) {
      const action: CodeAction = {
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
      };
      actions.push(action);
    }
  });

  // Provide auto-fix for unmatched opening brackets
  const lines = text.split('\n');
  if (diagnostics.length > 0) {
    diagnostics.forEach((diagnostic) => {
      if (diagnostic.message.includes('unclosed')) {
        const line = lines[diagnostic.range.start.line] || '';
        const endOfLine = {
          line: diagnostic.range.start.line,
          character: line.length,
        };

        let closingChar = ')';
        if (diagnostic.message.includes('bracket')) closingChar = ']';
        else if (diagnostic.message.includes('brace')) closingChar = '}';

        const action: CodeAction = {
          title: `Add missing ${closingChar} at end of line`,
          kind: CodeActionKind.QuickFix,
          diagnostics: [diagnostic],
          edit: {
            changes: {
              [params.textDocument.uri]: [
                TextEdit.insert(endOfLine, closingChar),
              ],
            },
          },
        };
        actions.push(action);
      }
    });
  }

  return actions;
});

// ============================================================================
// Configuration
// ============================================================================

connection.onDidChangeConfiguration((_change) => {
  // Workspace configuration changed
  documents.all().forEach((doc) => {
    const diagnostics = parseMeTTa(doc.getText());
    connection.sendDiagnostics({ uri: doc.uri, diagnostics });
  });
});

// ============================================================================
// Setup & Lifecycle
// ============================================================================

documents.listen(connection);
connection.listen();
