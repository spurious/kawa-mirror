package kawa.lang;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;

/**
 * A pattern that requires an exact match (using equal?).
 */

public class EqualPat extends Pattern implements Printable, Compilable
{

  Object value;

  public EqualPat (Object obj) { value = obj; }

  static public EqualPat make (Object obj) { return new EqualPat (obj); }

  public int match (Object obj, Object[] vars, int start_vars) {
    if (kawa.standard.equal_p.equal_p (value, obj))
      return 0;
    else
      return -1;
  }

  public int varCount () { return 0; }

  public void print(java.io.PrintStream ps)
  {
    ps.print ("#<match equal");
    kawa.lang.print.print (value, ps);
    ps.print ('>');
  }

  static public ClassType EqualPatType;
  static Method makeEqualPatMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (EqualPatType == null)
      {
	EqualPatType = new ClassType ("kawa.lang.EqualPat");
	makeEqualPatMethod =
	  EqualPatType.new_method ("make", comp.apply1args,
				   EqualPatType, Access.PUBLIC|Access.STATIC);
      }
    Literal literal = new Literal (this, EqualPatType, comp);
    comp.findLiteral (value);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    literal.check_cycle ();
    comp.emitLiteral (value);
    comp.method.compile_invoke_static (makeEqualPatMethod);
    literal.flags |= Literal.ALLOCATED|Literal.INITIALIZED;
  }
}
