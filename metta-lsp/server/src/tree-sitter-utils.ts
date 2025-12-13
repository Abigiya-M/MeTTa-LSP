/**
 * Tree-Sitter Utilities for MeTTa Language Analysis
 * Provides AST-based parsing and analysis capabilities
 */

/**
 * Extract function definitions from source code
 * Matches patterns like: (defn name ...) (define name ...) (defun name ...)
 */
export function extractFunctionDefs(text: string): FunctionDef[] {
  const functions: FunctionDef[] = [];
  
  // Pattern: (def... name args...)
  const defRegex = /\((def[a-z]*|defn|defun|define)\s+(\w[\w\-]*)/g;
  let match;

  while ((match = defRegex.exec(text)) !== null) {
    const name = match[2];
    const position = text.substring(0, match.index).split('\n').length - 1;
    // Calculate column position of the function name, not the opening paren
    const nameStartIndex = match.index + match[0].indexOf(name);
    
    functions.push({
      name,
      type: 'function',
      line: position,
      column: nameStartIndex - text.substring(0, nameStartIndex).lastIndexOf('\n') - 1,
    });
  }

  return functions;
}

/**
 * Find all variable definitions in let bindings
 */
export function extractVariables(text: string): Variable[] {
  const variables: Variable[] = [];
  
  // Pattern: (let ((var val) ...))
  const letRegex = /\(\s*let\*?\s*\(\s*\((\w[\w\-]*)/g;
  let match;

  while ((match = letRegex.exec(text)) !== null) {
    const name = match[1];
    const position = text.substring(0, match.index).split('\n').length - 1;
    const nameStartIndex = match.index + match[0].indexOf(name);
    
    variables.push({
      name,
      type: 'variable',
      line: position,
      column: nameStartIndex - text.substring(0, nameStartIndex).lastIndexOf('\n') - 1,
    });
  }

  return variables;
}

/**
 * Find all quoted symbols (starting with $)
 */
export function extractQuotedSymbols(text: string): Variable[] {
  const symbols: Variable[] = [];
  
  const varRegex = /\$(\w[\w\-]*)/g;
  let match;

  while ((match = varRegex.exec(text)) !== null) {
    const name = match[1];
    const position = text.substring(0, match.index).split('\n').length - 1;
    
    symbols.push({
      name,
      type: 'variable',
      line: position,
      column: match.index,
    });
  }

  return symbols;
}

/**
 * Analyze scope and find undefined references
 */
export function analyzeScope(text: string): ScopeAnalysis {
  const functions = extractFunctionDefs(text);
  const variables = extractVariables(text);
  const symbols = extractQuotedSymbols(text);
  
  // Combine all definitions
  const defined: Definition[] = [];
  defined.push(...functions);
  defined.push(...variables);
  defined.push(...symbols);

  // Find potential undefined references
  const allDefined = new Set([
    ...functions.map((f) => f.name),
    ...variables.map((v) => v.name),
    ...symbols.map((s) => s.name),
  ]);

  // Extract all identifiers used in the code
  const usageRegex = /\b([a-zA-Z_\-][\w\-]*)\b/g;
  const used = new Set<string>();
  
  let match;
  while ((match = usageRegex.exec(text)) !== null) {
    const identifier = match[1];
    // Skip keywords and built-ins
    if (!isKeyword(identifier) && !isBuiltin(identifier)) {
      used.add(identifier);
    }
  }

  const undefined = Array.from(used).filter((u) => !allDefined.has(u));

  return {
    defined,
    undefined,
  };
}

/**
 * Extract all list heads (potential function calls)
 */
export function extractListHeads(text: string): ListHead[] {
  const heads: ListHead[] = [];
  
  // Pattern: (symbol ...
  const headRegex = /\(\s*(\w[\w\-]*)/g;
  let match;

  while ((match = headRegex.exec(text)) !== null) {
    const name = match[1];
    const position = text.substring(0, match.index).split('\n').length - 1;
    
    heads.push({
      name,
      line: position,
      column: match.index,
    });
  }

  return heads;
}

/**
 * Check if text is a MeTTa keyword
 */
function isKeyword(text: string): boolean {
  const keywords = [
    'defn', 'defun', 'define', 'def',
    'match', 'case',
    'if', 'then', 'else',
    'let', 'let*', 'in',
    'lambda', 'fn', 'quote',
    'eval', 'apply',
    'atom', 'type', 'module',
    'import', 'export',
  ];
  return keywords.includes(text.toLowerCase());
}

/**
 * Check if text is a built-in function
 */
function isBuiltin(text: string): boolean {
  const builtins = [
    '+', '-', '*', '/', '=', '==', '!=', '<', '>', '<=', '>=',
    'car', 'cdr', 'cons', 'list', 'append', 'length',
    'not', 'and', 'or',
    'null?', 'number?', 'symbol?', 'list?',
    'print', 'display', 'newline',
  ];
  return builtins.includes(text.toLowerCase());
}

/**
 * Detect potential errors in the code
 */
export function detectErrors(text: string): ParseError[] {
  const errors: ParseError[] = [];
  const lines = text.split('\n');

  let parenDepth = 0;
  let bracketDepth = 0;
  let braceDepth = 0;

  lines.forEach((line, lineIndex) => {
    for (let i = 0; i < line.length; i++) {
      const char = line[i];

      if (char === '(') parenDepth++;
      else if (char === ')') {
        parenDepth--;
        if (parenDepth < 0) {
          errors.push({
            type: 'error',
            message: 'Unmatched closing parenthesis',
            line: lineIndex,
            column: i,
          });
          parenDepth = 0;
        }
      } else if (char === '[') bracketDepth++;
      else if (char === ']') {
        bracketDepth--;
        if (bracketDepth < 0) {
          errors.push({
            type: 'error',
            message: 'Unmatched closing bracket',
            line: lineIndex,
            column: i,
          });
          bracketDepth = 0;
        }
      } else if (char === '{') braceDepth++;
      else if (char === '}') {
        braceDepth--;
        if (braceDepth < 0) {
          errors.push({
            type: 'error',
            message: 'Unmatched closing brace',
            line: lineIndex,
            column: i,
          });
          braceDepth = 0;
        }
      }
    }
  });

  if (parenDepth > 0) {
    errors.push({
      type: 'error',
      message: `${parenDepth} unclosed parenthesis(es)`,
      line: lines.length - 1,
      column: lines[lines.length - 1]?.length || 0,
    });
  }

  if (bracketDepth > 0) {
    errors.push({
      type: 'error',
      message: `${bracketDepth} unclosed bracket(s)`,
      line: lines.length - 1,
      column: lines[lines.length - 1]?.length || 0,
    });
  }

  if (braceDepth > 0) {
    errors.push({
      type: 'error',
      message: `${braceDepth} unclosed brace(s)`,
      line: lines.length - 1,
      column: lines[lines.length - 1]?.length || 0,
    });
  }

  return errors;
}

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

export interface ListHead {
  name: string;
  line: number;
  column: number;
}

export interface ScopeAnalysis {
  defined: Definition[];
  undefined: string[];
}

export interface ParseError {
  type: 'error' | 'warning';
  message: string;
  line: number;
  column: number;
}
