/**
 * Centralized Tree-Sitter parser instance
 * Single parser + language binding shared across the entire LSP
 *
 * This prevents memory leaks and ensures consistent parsing behavior.
 */

import Parser from 'tree-sitter';

// Load the native Tree-Sitter Metta grammar binding
const Metta = require('tree-sitter-metta/bindings/node');

// Create single parser instance
const parser = new Parser();

// Configure with Metta language
parser.setLanguage(Metta);

export { parser, Metta };
