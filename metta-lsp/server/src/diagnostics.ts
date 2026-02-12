/**
 * MeTTa Diagnostics Engine (Tree-sitter driven)
 *
 * Provides syntax and semantic diagnostics by walking the Tree-sitter AST.
 * - Syntax diagnostics: ERROR / MISSING nodes from the parser
 * - Semantic diagnostics: arity validation against known forms
 *
 * All functions accept a pre-parsed Tree so the document cache can be used.
 */

import Parser, { SyntaxNode, Tree } from 'tree-sitter';
import { ARITY_RULES } from './metta-builtins';

export interface AnalyzerDiagnostic {
  message: string;
  start: { row: number; column: number };
  end: { row: number; column: number };
}

// ============================================================================
// Syntax diagnostics
// ============================================================================

function isErrorNode(node: SyntaxNode): boolean {
  return node.type === 'ERROR' || node.isMissing;
}

function syntaxMessage(node: SyntaxNode): string {
  if (node.isMissing) {
    return `Missing ${node.type}`;
  }
  return 'Syntax error';
}

function collectSyntaxDiagnostics(tree: Tree): AnalyzerDiagnostic[] {
  const diagnostics: AnalyzerDiagnostic[] = [];
  const stack: SyntaxNode[] = [tree.rootNode];

  while (stack.length) {
    const node = stack.pop() as SyntaxNode;

    if (isErrorNode(node)) {
      let message = syntaxMessage(node);
      const startPos = node.startPosition;
      let endPos = { row: startPos.row, column: startPos.column + 1 };

      const nodeText = node.text;

      // Detect specific unclosed delimiter and provide a targeted message
      if (nodeText.includes('(') && !nodeText.includes(')')) {
        message = 'Unclosed parenthesis';
        const offset = nodeText.indexOf('(');
        endPos = { row: startPos.row, column: startPos.column + offset + 1 };
      } else if (nodeText.includes('[') && !nodeText.includes(']')) {
        message = 'Unclosed bracket';
        const offset = nodeText.indexOf('[');
        endPos = { row: startPos.row, column: startPos.column + offset + 1 };
      } else if (nodeText.includes('{') && !nodeText.includes('}')) {
        message = 'Unclosed brace';
        const offset = nodeText.indexOf('{');
        endPos = { row: startPos.row, column: startPos.column + offset + 1 };
      }

      diagnostics.push({ message, start: startPos, end: endPos });
    }

    // Only recurse into children that carry errors to avoid full-tree walk
    for (const child of node.children) {
      if (child.hasError || child.isMissing || child.type === 'ERROR') {
        stack.push(child);
      }
    }
  }

  return diagnostics;
}

// ============================================================================
// Semantic diagnostics (arity validation)
// ============================================================================

function collectArityDiagnostics(tree: Tree): AnalyzerDiagnostic[] {
  const diagnostics: AnalyzerDiagnostic[] = [];
  const lists = tree.rootNode.descendantsOfType('list');

  for (const list of lists) {
    const head = list.childForFieldName('head');
    if (!head || head.type !== 'symbol') continue;

    const rule = ARITY_RULES[head.text];
    if (!rule) continue;

    // Try field-based argument extraction first
    let count = list.childrenForFieldName('argument').length;

    // Fallback: count non-punctuation children after the head
    if (count === 0 && list.children.length > 2) {
      count = list.children.filter((c: SyntaxNode, i: number) =>
        i > 1 && c.type !== '(' && c.type !== ')'
      ).length;
    }

    if (count < rule.min || count > rule.max) {
      const expected = rule.min === rule.max
        ? `${rule.min}`
        : `${rule.min}-${rule.max}`;
      diagnostics.push({
        message: `${head.text} expects ${expected} argument(s); got ${count}`,
        start: head.startPosition,
        end: head.endPosition,
      });
    }
  }

  return diagnostics;
}

// ============================================================================
// Public API
// ============================================================================

/**
 * Run all diagnostics against a pre-parsed Tree-sitter tree.
 */
export function analyzeTree(tree: Tree): AnalyzerDiagnostic[] {
  const syntax = collectSyntaxDiagnostics(tree);
  const arity = collectArityDiagnostics(tree);
  return [...syntax, ...arity];
}
