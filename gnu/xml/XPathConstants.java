// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;

/**
 * @deprecated  Use the XQuery implementation instead.
 */

public interface XPathConstants
{
  /**
   * An encoding of the token type:
   * '\uffff': end of string.
   * '\001': invalid character.
   * '\002': lexer error, such as non-terminated string literal.
   * '(', ')', '[', ']', '.', '@', ',', '$':  That character token.
   * '/', '|', '*':  That operator.
   * '2': '..'
   * 'X': '::'
   * '"': String literal
   * '0': number literal
   * 'D': '//'
   * 'A': identifier (unresolved)
   * 'F': FuncName (as well as following '(') (intern'ed name is curName)
   * 'Q': QName (intern'ed name is curName)
   * 'R': NCName ':' '*'
   * OP_AXIS_FIRST: 'ancestor' followed by '::'
   * ...
   * OP_AXIS_FIRST+AXIS_SELF: 'self' followed by '::'
   */

  /** Represents a LocationPath:
   * Word-code usage:
   * (code to evaluate step1 - in forward direction, not RPN))
   * ...
   * (code to evaluate stepN - in forward direction, not RPN))
   * [start of step1, relative to OP_LOCATIONPATH pc]
   * ...
   * [start of stepN, relative to OP_LOCATIONPATH pc]
   * [number of steps (i.e. N)]
   * [OP_LOCATIONPATH]
   */
  static final byte OP_LOCATIONPATH = 86;

  /** Represents a FilterExpr followed by a '/' or '//' and RelativeLocationPath:
   * Word-code usage:
   * (code for FilterExpr)
   * (code to evaluate step1 - in forward direction, not RPN))
   * ...
   * (code to evaluate stepn - in forward direction, not RPN))
   * [start of step1, relative to OP_EXPR_LOCATIONPATH pc]
   * ...
   * [start of stepN, relative to OP_EXPR_LOCATIONPATH pc]
   * [number of steps (i.e. N)]
   * [OP_EXPR_LOCATIONPATH]
   */
  static final byte OP_EXPR_LOCATIONPATH = 87;

  /** The NodeType operator 'node()'.
   * As a lexical token, represents 'node' followed by '('. */
  static final byte OP_NODE = 90;  // 'node()'

  /** The NodeType operator for the document root.
   * As a lexical token, represents an initial '/'. */
  static final byte OP_ROOT = 91;  // initial '/'

  /** The NodeType operator 'comment()'.
   * As a lexical token, represents 'comment' followed by '('. */
  static final byte OP_COMMENT = 92;  // 'comment()' followed by '('.

  /** The NodeType operator 'text()'.
   * As a lexical token, represents 'text' followed by '('. */
  static final byte OP_TEXT = 93; // 'text()'

  /** The NodeType operator 'processing-instructor([Name])'.
   * As a lexical token, represents 'processing-instruction'
   * followed by '('.
   * Wordcode usage:
   * [OP_PROCESSING_INSTRUCTION]
   * [index of Name in values table, or -1]
   */
  static final byte OP_PROCESSING_INSTRUCTION = 94;

  // A NameTest that is a QName.
  // [OP_QNAMETEST]
  // [index of QName in values table]
  static final byte OP_SIMPLE_NAMETEST = 95;

  // A NameTest that is a '*' - i.e. matches any node.
  static final byte OP_ANY_NAMETEST = 96;

  // A NameTest that is NCName ':' '*'.
  static final byte OP_NAMESPACE_NAMETEST = 97;

  // [OP_PREDICATE]
  // [index of counter to increment (for position)]
  // [length of predicate expresison]
  // (predicate expression)
  static final byte OP_PREDICATE = 98;

  static final int OP_AXIS_FIRST = 100;
  static final int COUNT_OP_AXIS = 13;
  static final int AXIS_ANCESTOR = 0;
  static final int AXIS_ANCESTOR_OR_SELF = 1;
  static final int AXIS_ATTRIBUTE = 2;
  static final int AXIS_CHILD = 3;
  static final int AXIS_DESCENDANT = 4;
  static final int AXIS_DESCENDANT_OR_SELF = 5;
  static final int AXIS_FOLLOWING = 6;
  static final int AXIS_FOLLOWING_SIBLING = 7;
  static final int AXIS_NAMESPACE = 8;
  static final int AXIS_PARENT = 9;
  static final int AXIS_PRECEDING = 10;
  static final int AXIS_PRECEDING_SIBLING = 11;
  static final int AXIS_SELF = 12;

  // (operand, as a short)
  // [OP_SHORT]
  static final byte OP_SHORT = 113;

  // ((short)(Double.doubleToLongBits(operand) >> 48))
  // ((short)(Double.doubleToLongBits(operand) >> 32))
  // ((short)(Double.doubleToLongBits(operand) >> 16))
  // ((short)(Double.doubleToLongBits(operand)))
  // [OP_DOUBLE]
  static final byte OP_DOUBLE = 114;

  /** Represents a FunctionCall.
   * Word-code usage:
   * (For each argument expression:
   *   [length of expression]
   *   (argument expression))
   * [length of all the argument expressions, including length words]
   * [number of arguments]
   * [index of FunctionName in values table]
   * [OP_FUNCTION_CALL]
   */
  static final byte OP_FUNCTION_CALL = 115;

  /** Represents a VariableReference.
   * Word-code usage:
   * [index of variable name in values table]
   * [OP_VARIABLE_REFERENCE]
   */
  static final byte OP_VARIABLE_REFERENCE = 116;

  /** Represents a string literal
   * Word-code usage:
   * [index of value (as a String) in values table]
   * [OP_STRING_LITERAL]
   */
  static final byte OP_STRING_LITERAL = 117;

  /** Represents a non-trivial UnionExpr.
   * Word-code usage:
   * (operand 1)
   * ...
   * (operand N)
   * [start of operand1, relative to OP_UNION pc]
   * ...
   * [start of operandN, relative to OP_UNION pc]
   * [number of operands (i.e. N)]
   * [OP_UNION]
   */
  static final byte OP_UNION = 118;

  // Token types for binary operators.
  // When used as a token code, get the priority by shifting 2 right.
  // In word-code, these are used as such:
  // (operand 1)
  // (operand 2)
  // [length of operand1]
  // [length of operand2]
  // [OP_XXX]
  static final int OP_OR  = 120;          // 'or'
  static final int OP_AND = 120 + 4;      // 'and'
  static final int OP_EQU = 120 + 8;      // '='
  static final int OP_NEQ = 120 + 8 + 1;  // '!='
  static final int OP_LSS = 120 + 12;     // '<'
  static final int OP_GRT = 120 + 12 + 1; // '>'
  static final int OP_LEQ = 120 + 12 + 2; // '<='
  static final int OP_GEQ = 120 + 12 + 3; // '>='
  static final int OP_ADD = 120 + 16;     // '+'
  static final int OP_SUB = 120 + 16 + 1; // '-'
  static final int OP_MUL = 120 + 20;     // '*'
  static final int OP_DIV = 120 + 20 + 1; // 'div'
  static final int OP_MOD = 120 + 20 + 2; // 'mod'

  /** A result of any type.
   * As a result code, means result is in context's objResult field.
   */
  static final int ANY_RESULT = 0;

  /** A string result.
   * As a request code, tells eval to leave a String result in objResult.
   * As a result code, means result is a String in context's objResult field.
   */
  static final int STRING_RESULT = 1;

  /** A node-set result.
   * As a result code, means result is a NodeList in context's
   * objResult field.
   */
  static final int NODESET_RESULT = 2;

  /** A boolean result.
   * As a result code, means result is in context's boolResult field.
   * As a request code, tells eval to leave result in boolResult.
   */		
  static final int BOOLEAN_RESULT = 3;

  /** A boolean result, but an number is matches againt position().
   * Invalid as a result code.
   * As a request code, tells eval to convert numbers to booleans by
   * comparing with position(), and to leave result in boolResult.	
   */
  static final int PREDICATE_RESULT = 4;

  /** A numeric result.
   * As a result code, means result is in context's numResult field.
   * As a request code, tells eval to leave result in numResult.
   */		
  static final int NUMBER_RESULT = 5;

  /** A numeric result.
   * As a request code, tells eval to increment numResult for each node.
   */
  static final int COUNT_RESULT = 6;
}
