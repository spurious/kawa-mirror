package gnu.expr;

/** An interface of values that are compilable as Literals. */

public interface Compilable
{
  /** Generates a (new) Literal for this for a given Compilation.
   * If this has sub-objects, calls Compilation.findLiteral on its components.
   */
  Literal makeLiteral (Compilation comp);

  /** Emit code (in comp.method) to initialize a Literal for this value.
   * Generates code to make the Literal at least ALLOACTED.
   * Also makes it INITIALIZED, unless ALLOCATING is set (in which
   * one of our callers will make it INITIALIZED).
   * Never called if already ALLOCATED.
   */
  void emit (Literal literal, Compilation comp);
}
