package kawa.lang;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;

public class PairPat extends Pattern implements Printable, Compilable
{
  Pattern car;
  Pattern cdr;

  public PairPat (Pattern car, Pattern cdr)
  {
    this.car = car;
    this.cdr = cdr;
  }

  public static PairPat make (Pattern car, Pattern cdr)
  {
    return new PairPat (car, cdr);
  }

  public int match (Object obj, Object[] vars, int start_vars)
  {
    if (! (obj instanceof Pair))
      return -1;
    Pair pair = (Pair) obj;
    int car_match = car.match (pair.car, vars, start_vars);
    if (car_match < 0)
      return -1;
    int cdr_match = cdr.match (pair.cdr, vars, start_vars + car_match);
    return cdr_match < 0 ? -1 : car_match + cdr_match;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print ("#<pair-pattern car: ");
    kawa.lang.print.print (car, ps);
    ps.print (" cdr: ");
    kawa.lang.print.print (cdr, ps);
    ps.print ('>');
  }

  public int varCount () { return car.varCount () + cdr.varCount (); }

  static public ClassType classPairPat;
  static Method makePairPatMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (classPairPat == null)
      {
	classPairPat = new ClassType ("kawa.lang.PairPat");
	Type[] apply2args = new Type[2];
	apply2args[0] = comp.scmPatternType;
	apply2args[1] = comp.scmPatternType;
	makePairPatMethod =
	  classPairPat.new_method ("make", apply2args,
				   classPairPat, Access.PUBLIC|Access.STATIC);
      }
    Literal literal = new Literal (this, classPairPat, comp);
    comp.findLiteral (car);
    comp.findLiteral (cdr);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    literal.check_cycle ();
    comp.emitLiteral (car);
    comp.emitLiteral (cdr);
    comp.method.compile_invoke_static (makePairPatMethod);
    literal.flags |= Literal.ALLOCATED|Literal.INITIALIZED;
  }

}
