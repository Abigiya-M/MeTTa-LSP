/**
 * MeTTa Builtin Completion Item Definitions
 * Maps builtin symbols to their completion documentation
 * Separated from core builtins config for better code organization
 */

import { CompletionItem, CompletionItemKind } from 'vscode-languageserver/node';
import { isKeyword } from './metta-builtins';

export const BUILTIN_COMPLETIONS_DATA: Record<string, { detail: string; documentation: string }> = {
  'defn': { detail: 'Define a function', documentation: 'Syntax: (defn name (params...) body...)\nDefines a named function with parameters and a body that gets executed when called.' },
  'defun': { detail: 'Define a function (alias for defn)', documentation: 'Syntax: (defun name (params...) body...)\nAlternative syntax for defining functions. Equivalent to defn.' },
  'define': { detail: 'Define a variable', documentation: 'Syntax: (define name value)\nBinds a name to a value in the current scope.' },
  'match': { detail: 'Pattern matching construct', documentation: 'Syntax: (match expr (pattern body)...)\nMatches an expression against multiple patterns and executes the body of the first matching pattern.' },
  'if': { detail: 'Conditional expression', documentation: 'Syntax: (if condition then-expr else-expr)\nEvaluates condition and returns then-expr if true, else-expr if false.' },
  'then': { detail: 'Then branch of conditional', documentation: 'Used in if expressions to denote the true branch: (if cond then-expr else-expr)' },
  'else': { detail: 'Else branch of conditional', documentation: 'Used in if expressions to denote the false branch: (if cond then-expr else-expr)' },
  'let': { detail: 'Local binding construct', documentation: 'Syntax: (let ((var val)...) body...)\nCreates local variable bindings available only within the body.' },
  'let*': { detail: 'Sequential local bindings', documentation: 'Syntax: (let* ((var val)...) body...)\nLike let, but bindings are sequential so later bindings can reference earlier ones.' },
  'in': { detail: 'In expression continuation', documentation: 'Used with let bindings: (let ((x 10)) (in (+ x 1)))' },
  'atom': { detail: 'Atom literal or type check', documentation: 'Syntax: (atom value)\nCreates an atom literal or checks if a value is an atom.' },
  'type': { detail: 'Type annotation', documentation: 'Syntax: (type expr type-spec)\nAnnotates the type of an expression.' },
  'import': { detail: 'Import a module', documentation: 'Syntax: (import module-name)\nImports definitions from a module.' },
  'module': { detail: 'Module definition', documentation: 'Syntax: (module name exports...)\nDefines a module with exported symbols.' },
  'fn': { detail: 'Function (lambda shorthand)', documentation: 'Syntax: (fn (params...) body...)\nDefines an anonymous function. Equivalent to lambda.' },
  'lambda': { detail: 'Anonymous function', documentation: 'Syntax: (lambda (params...) body...)\nCreates an anonymous function that can be passed as a value or assigned to variables.' },
  'quote': { detail: 'Quote expression (prevent evaluation)', documentation: 'Syntax: (quote expr) or \'expr\nPrevents the quoted expression from being evaluated, treating it as data.' },
  'quasiquote': { detail: 'Quasiquote (controlled evaluation)', documentation: 'Syntax: (quasiquote expr)\nLike quote but allows selective evaluation of unquoted parts.' },
  'unquote': { detail: 'Unquote within quasiquote', documentation: 'Syntax: (unquote expr)\nWithin a quasiquote, evaluates the expression.' },
  'eval': { detail: 'Evaluate expression', documentation: 'Syntax: (eval expr)\nEvaluates an expression at runtime.' },
  'cond': { detail: 'Multiple conditional branches', documentation: 'Syntax: (cond (test body)...)\nEvaluates test expressions in sequence and executes body of first true test.' },
  'case': { detail: 'Switch-like pattern matching', documentation: 'Syntax: (case expr (pattern body)...)\nMatches expr against patterns similar to match.' },
  'and': { detail: 'Logical AND', documentation: 'Syntax: (and expr...)\nReturns true if all expressions are true, short-circuits on false.' },
  'or': { detail: 'Logical OR', documentation: 'Syntax: (or expr...)\nReturns true if any expression is true, short-circuits on true.' },
  'not': { detail: 'Logical NOT', documentation: 'Syntax: (not expr)\nReturns the boolean negation of expr.' },
  '=': { detail: 'Equality comparison', documentation: 'Syntax: (= expr1 expr2 ...)\nReturns true if all arguments are equal.' },
  '==': { detail: 'Strict equality check', documentation: 'Syntax: (== expr1 expr2)\nChecks if two expressions are strictly equal.' },
  '!=': { detail: 'Not equal', documentation: 'Syntax: (!= expr1 expr2)\nReturns true if expressions are not equal.' },
  '+': { detail: 'Addition', documentation: 'Syntax: (+ num1 num2 ...)\nAdds all numeric arguments.' },
  '-': { detail: 'Subtraction or negation', documentation: 'Syntax: (- num1 num2 ...)\nSubtracts subsequent numbers from the first, or negates if single argument.' },
  '*': { detail: 'Multiplication', documentation: 'Syntax: (* num1 num2 ...)\nMultiplies all numeric arguments.' },
  '/': { detail: 'Division', documentation: 'Syntax: (/ num1 num2)\nDivides num1 by num2.' },
  '%': { detail: 'Modulo (remainder)', documentation: 'Syntax: (% num1 num2)\nReturns the remainder of num1 divided by num2.' },
  '<': { detail: 'Less than', documentation: 'Syntax: (< num1 num2 ...)\nReturns true if arguments are in strictly increasing order.' },
  '>': { detail: 'Greater than', documentation: 'Syntax: (> num1 num2 ...)\nReturns true if arguments are in strictly decreasing order.' },
  '<=': { detail: 'Less than or equal', documentation: 'Syntax: (<= num1 num2 ...)\nReturns true if arguments are in non-decreasing order.' },
  '>=': { detail: 'Greater than or equal', documentation: 'Syntax: (>= num1 num2 ...)\nReturns true if arguments are in non-increasing order.' },
  'car': { detail: 'Get first element of list', documentation: 'Syntax: (car list)\nReturns the first element (head) of a list.' },
  'cdr': { detail: 'Get rest of list', documentation: 'Syntax: (cdr list)\nReturns the rest (tail) of a list, everything after the first element.' },
  'cons': { detail: 'Construct list', documentation: 'Syntax: (cons elem list)\nConstructs a new list by prepending elem to the front of list.' },
  'list': { detail: 'Create a list', documentation: 'Syntax: (list elem...)\nCreates a list containing the given elements.' },
  'append': { detail: 'Concatenate lists', documentation: 'Syntax: (append list1 list2 ...)\nConcatenates multiple lists into one.' },
  'length': { detail: 'Get list length', documentation: 'Syntax: (length list)\nReturns the number of elements in a list.' },
  'null?': { detail: 'Check if empty list', documentation: 'Syntax: (null? expr)\nReturns true if expr is an empty list ().' },
  'atom?': { detail: 'Check if atom', documentation: 'Syntax: (atom? expr)\nReturns true if expr is an atom (not a list).' },
  'number?': { detail: 'Check if number', documentation: 'Syntax: (number? expr)\nReturns true if expr is a number.' },
  'string?': { detail: 'Check if string', documentation: 'Syntax: (string? expr)\nReturns true if expr is a string.' },
  'symbol?': { detail: 'Check if symbol', documentation: 'Syntax: (symbol? expr)\nReturns true if expr is a symbol.' },
  'list?': { detail: 'Check if list', documentation: 'Syntax: (list? expr)\nReturns true if expr is a list.' },
  'print': { detail: 'Print to output', documentation: 'Syntax: (print expr)\nPrints the expression to standard output.' },
  'display': { detail: 'Display value', documentation: 'Syntax: (display expr)\nDisplays the value in a human-readable format.' },
  'newline': { detail: 'Print newline', documentation: 'Syntax: (newline)\nPrints a newline character.' },
  'def': { detail: 'Define (shorthand)', documentation: 'Syntax: (def name value)\nShorthand for define.' },
  'apply': { detail: 'Apply function to arguments', documentation: 'Syntax: (apply fn args)\nApplies function fn to a list of arguments.' },
  'export': { detail: 'Export from module', documentation: 'Syntax: (export name)\nExports a symbol from the current module.' },
};

/**
 * Get completion item for a builtin symbol
 */
export function getBuiltinCompletionItem(label: string): CompletionItem {
  const info = BUILTIN_COMPLETIONS_DATA[label] || {
    detail: label,
    documentation: '',
  };

  return {
    label,
    kind: isKeyword(label) ? CompletionItemKind.Keyword : CompletionItemKind.Function,
    insertText: label,
    detail: info.detail,
    documentation: info.documentation,
  };
}

/**
 * Generate completion items for all builtins
 */
export function getAllBuiltinCompletionItems(): CompletionItem[] {
  return Object.keys(BUILTIN_COMPLETIONS_DATA).map((label) =>
    getBuiltinCompletionItem(label)
  );
}
