package kawa.lang;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;

public class ListRepeatPat extends Pattern implements Printable, Compilable
{
  Pattern element_pattern;

  public ListRepeatPat (Pattern element_pattern)
  {
    this.element_pattern = element_pattern;
  }

  public static ListRepeatPat make (Pattern element_pattern)
  {
    return new ListRepeatPat (element_pattern);
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print ("#<list-repeat-pattern ");
    SFormat.print (element_pattern, ps);
    ps.print ('>');
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    /* DEBUGGING
      System.err.print ("(match ");
      print (System.err);
      System.err.print (" on ");
      SFormat.print (obj, System.err);
      System.err.print (")\n");
      */
    int length = List.list_length (obj);
    if (length < 0)
      return false;

    int var_count = element_pattern.varCount ();
    for (int i = var_count;  --i >= 0; )
      vars[start_vars + i] = new Object [length];
    Object[] element_vars = new Object [var_count];
    for (int j = 0; j < length;  j++)
      {
	Pair pair = (Pair) obj;
	/* DEBUGGING
	   System.err.print ("(sub-match ["+j+"] ");
	   SFormat.print (element_pattern, System.err);
	   System.err.print (" on ");
	   SFormat.print (pair.car, System.err);
	   */

	if (! element_pattern.match (pair.car, element_vars, 0))
	  return false;
	for (int i = 0;  i < var_count;  i++)
	  ((Object[]) vars[start_vars + i]) [j] = element_vars[i];
	obj = pair.cdr;
      }
    return true;
  }

  public int varCount () { return element_pattern.varCount (); }

  static public ClassType thisType;
  static Method makeListRepeatMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.lang.ListRepeatPat");
	Type[] apply1args = new Type[1];
	apply1args[0] = comp.scmPatternType;
	makeListRepeatMethod =
	  thisType.new_method ("make", apply1args,
			       thisType, Access.PUBLIC|Access.STATIC);
      }
    Literal literal = new Literal (this, thisType, comp);
    comp.findLiteral (element_pattern);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    literal.check_cycle ();
    comp.emitLiteral (element_pattern);
    comp.method.compile_invoke_static (makeListRepeatMethod);
  }
}
