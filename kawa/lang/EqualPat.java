package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;

/**
 * A pattern that requires an exact match (using equal?).
 */

public class EqualPat extends Pattern implements Printable, Compilable
{

  Object value;

  public EqualPat (Object obj) { value = obj; }

  static public EqualPat make (Object obj) { return new EqualPat (obj); }

  public boolean match (Object obj, Object[] vars, int start_vars) {
    return value.equals (obj);
  }

  public int varCount () { return 0; }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<match equal");
    SFormat.print (value, ps);
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
	  EqualPatType.addMethod ("make", comp.apply1args,
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
  }
}
