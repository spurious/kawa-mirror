package kawa.lang;

/**
 * Abstract class for "syntax" objects.
 * Builtins and macros are instances of this class.
 * @author	Per Bothner
 */

abstract public class Syntax
{
  /**
   * Re-write an expression that is an "application" of this Syntax object.
   * @param obj the arguments to this "application" (i.e. the cdr of
   * the macro/builtin invokation)
   * @param interp the interpreter thatprovides context
   * @return the re-written expression
   */
  abstract public Expression rewrite (Object obj, Interpreter interp)
    throws kawa.lang.WrongArguments;
}
