package kawa.lang;

/** An interface of values that are compilable as Literals. */

public interface Compilable
{
  /** Emit code (in comp.method) to initialize a Literal for this value.
   * If the literal's state on input is UNALLOCATED, makes it INITIALIZED.
   * If the input state is ALLOCATING, makes it ALLOCATED.
   * Never called if already ALLOCATED or INITIALIZED.
   */
    void emit (Literal literal, Compilation comp);
}
