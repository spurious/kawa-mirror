package kawa.lang;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;

/**
 * A pattern that matches anything.
 */

public class AnyPat extends Pattern implements Printable, Compilable
{
  public AnyPat () { }

  public static AnyPat make () { return new AnyPat (); }

  public void print(java.io.PrintStream ps)
  {
    ps.print ("#<match any>");
  }

  public int match (Object obj, Object[] vars, int start_vars)
  {
    vars[start_vars] = obj;
    return 1;
  }

  public int varCount () { return 1; }

  static public ClassType thisType;
  static Method makeAnyPatMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.lang.AnyPat");
	makeAnyPatMethod =
	  thisType.new_method ("make", comp.apply0args,
			       thisType, Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, thisType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    comp.method.compile_invoke_static (makeAnyPatMethod);
  }
}
