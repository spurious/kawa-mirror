package kawa.lang;

/** Interface exported by Scheme procedures that are compiled to bytescodes.
 */

public interface CompiledProc
{
  /** Pass array of literal values to class. */
  public abstract void setLiterals (Object [] values);
}
