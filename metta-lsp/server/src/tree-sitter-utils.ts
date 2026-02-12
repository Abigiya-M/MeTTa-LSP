/**
 * Tree-Sitter Utilities for MeTTa Language Analysis
 * Provides AST-based symbol extraction and analysis using Tree-sitter parse trees.
 *
 * All functions accept a Tree-sitter Tree (obtained via the document cache)
 * so that the same parse tree is reused across diagnostics, completions,
 * hover, go-to-definition, and document symbols.
 */

import { SyntaxNode, Tree } from 'tree-sitter';

// ============================================================================
// Type Definitions
// ============================================================================

export interface FunctionDef {
  name: string;
  type: 'function';
  line: number;
  column: number;
}

export interface Variable {
  name: string;
  type: 'variable';
  line: number;
  column: number;
}

export type Definition = FunctionDef | Variable;

// ============================================================================
// AST-based extraction helpers
// ============================================================================

/** Definition keywords that introduce a named binding */
const DEF_KEYWORDS = new Set(['defn', 'defun', 'define', 'def']);

/** Let-form keywords that introduce variable bindings */
const LET_KEYWORDS = new Set(['let', 'let*']);

/**
 * Extract function/definition names from the AST.
 *
 * Walks every `list` node and checks whether its `head` field is a
 * definition keyword.  If so, the first argument is the defined name.
 */
export function extractFunctionDefs(tree: Tree): FunctionDef[] {
  const functions: FunctionDef[] = [];
  const lists = tree.rootNode.descendantsOfType('list');

  for (const list of lists) {
    const head = list.childForFieldName('head');
    if (!head) continue;

    const headText = head.type === 'symbol' ? head.text : resolveSymbolText(head);
    if (!headText || !DEF_KEYWORDS.has(headText)) continue;

    // The name is the first argument after the head
    const args = list.childrenForFieldName('argument');
    if (args.length === 0) continue;

    const nameNode = args[0];
    const name = resolveSymbolText(nameNode);
    if (!name) continue;

    functions.push({
      name,
      type: 'function',
      line: nameNode.startPosition.row,
      column: nameNode.startPosition.column,
    });
  }

  return functions;
}

/**
 * Extract variable bindings from let/let* forms.
 *
 * Pattern: (let ((var val) ...) body)
 *          (let* ((var val) ...) body)
 */
export function extractVariables(tree: Tree): Variable[] {
  const variables: Variable[] = [];
  const lists = tree.rootNode.descendantsOfType('list');

  for (const list of lists) {
    const head = list.childForFieldName('head');
    if (!head) continue;

    const headText = head.type === 'symbol' ? head.text : resolveSymbolText(head);
    if (!headText || !LET_KEYWORDS.has(headText)) continue;

    const args = list.childrenForFieldName('argument');
    if (args.length === 0) continue;

    // First argument should be the bindings list: ((var val) ...)
    const bindingsList = args[0];
    if (bindingsList.type !== 'list') continue;

    for (const binding of bindingsList.namedChildren) {
      if (binding.type !== 'list') continue;
      const varHead = binding.childForFieldName('head');
      if (!varHead) continue;
      const name = resolveSymbolText(varHead);
      if (!name) continue;

      variables.push({
        name,
        type: 'variable',
        line: varHead.startPosition.row,
        column: varHead.startPosition.column,
      });
    }
  }

  return variables;
}

/**
 * Extract $-prefixed variable references from the AST.
 */
export function extractDollarVariables(tree: Tree): Variable[] {
  const variables: Variable[] = [];
  const varNodes = tree.rootNode.descendantsOfType('variable');

  for (const node of varNodes) {
    variables.push({
      name: node.text,
      type: 'variable',
      line: node.startPosition.row,
      column: node.startPosition.column,
    });
  }

  return variables;
}

/**
 * Compute folding ranges from the AST.
 * Returns multi-line list nodes as foldable regions.
 */
export function computeFoldingRanges(tree: Tree): { startLine: number; endLine: number }[] {
  const ranges: { startLine: number; endLine: number }[] = [];
  const lists = tree.rootNode.descendantsOfType('list');

  for (const list of lists) {
    const startLine = list.startPosition.row;
    const endLine = list.endPosition.row;
    if (endLine - startLine > 1) {
      ranges.push({ startLine, endLine });
    }
  }

  return ranges;
}

// ============================================================================
// Internal helpers
// ============================================================================

/**
 * Resolve the text of a node that is expected to be a symbol.
 * Handles both raw `symbol` nodes and `atom` nodes wrapping a symbol.
 */
function resolveSymbolText(node: SyntaxNode): string | null {
  if (node.type === 'symbol') return node.text;
  if (node.type === 'atom') {
    const child = node.namedChildren.find((c) => c.type === 'symbol');
    return child ? child.text : null;
  }
  return null;
}
