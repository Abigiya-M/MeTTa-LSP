/**
 * MeTTa Language Builtins & Keywords Configuration
 * Single source of truth for:
 * - Keywords (defn, match, if, etc.)
 * - Builtins (operators, list ops, predicates, I/O)
 * - Arity rules for validation
 * 
 * Update this file once → used everywhere
 */

/**
 * MeTTa language keywords
 * Used for syntax highlighting, symbol extraction, completion
 */
export const METTA_KEYWORDS = [
  'defn', 'defun', 'define', 'def',
  'match', 'case',
  'if', 'then', 'else',
  'let', 'let*', 'in',
  'lambda', 'fn', 'quote', 'quasiquote', 'unquote',
  'eval', 'apply',
  'atom', 'type', 'module',
  'import', 'export',
  'cond',
];

/**
 * MeTTa builtin functions organized by category
 */
export const METTA_BUILTINS = {
  operators: ['+', '-', '*', '/', '=', '==', '!=', '<', '>', '<=', '>=', '%'],
  list: ['car', 'cdr', 'cons', 'list', 'append', 'length'],
  logical: ['not', 'and', 'or'],
  predicates: ['null?', 'number?', 'symbol?', 'list?', 'atom?', 'string?'],
  io: ['print', 'display', 'newline'],
};

/**
 * Flattened list of all builtins (for quick lookup)
 */
export const ALL_BUILTINS = [
  ...METTA_BUILTINS.operators,
  ...METTA_BUILTINS.list,
  ...METTA_BUILTINS.logical,
  ...METTA_BUILTINS.predicates,
  ...METTA_BUILTINS.io,
];

/**
 * Arity rules for MeTTa forms
 * Used for arity checking and validation
 */
export const ARITY_RULES: Record<string, { min: number; max: number }> = {
  'match': { min: 2, max: 2 },
  'if': { min: 3, max: 3 },
  'cond': { min: 1, max: Infinity },
  'case': { min: 2, max: Infinity },
  'define': { min: 2, max: 2 },
  'defn': { min: 2, max: Infinity },
  'defun': { min: 2, max: Infinity },
  'lambda': { min: 2, max: Infinity },
  'fn': { min: 2, max: Infinity },
  'let': { min: 2, max: Infinity },
  'let*': { min: 2, max: Infinity },
  'quote': { min: 1, max: 1 },
  'quasiquote': { min: 1, max: 1 },
  'unquote': { min: 1, max: 1 },
  'and': { min: 1, max: Infinity },
  'or': { min: 1, max: Infinity },
  'not': { min: 1, max: 1 },
  'atom': { min: 1, max: 1 },
  'type': { min: 1, max: 1 },
  'module': { min: 1, max: Infinity },
  'import': { min: 1, max: Infinity },
  'export': { min: 1, max: Infinity },
};

/**
 * Check if a symbol is a keyword
 */
export function isKeyword(text: string): boolean {
  return METTA_KEYWORDS.includes(text.toLowerCase());
}

/**
 * Check if a symbol is a builtin function
 */
export function isBuiltin(text: string): boolean {
  return ALL_BUILTINS.includes(text.toLowerCase());
}

/**
 * Get arity rules for a form
 */
export function getArityRule(formName: string): { min: number; max: number } | undefined {
  return ARITY_RULES[formName.toLowerCase()];
}
