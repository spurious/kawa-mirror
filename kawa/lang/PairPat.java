package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.*;

public class PairPat extends Pattern implements Printable, Compilable
{
  Pattern car;
  Pattern cdr;
  private int car_count, cdr_count;

  public PairPat (Pattern car, Pattern cdr)
  {
    this.car = car;
    this.cdr = cdr;
    car_count = car.varCount ();
    cdr_count = cdr.varCount ();
  }

  public static PairPat make (Pattern car, Pattern cdr)
  {
    return new PairPat (car, cdr);
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    if (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).form;
    if (! (obj instanceof Pair))
      return false;
    Pair pair = (Pair) obj;
    if (! car.match (pair.car, vars, start_vars))
      return false;
    return cdr.match (pair.cdr, vars, start_vars + car_count);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<pair-pattern car: ");
    SFormat.print (car, ps);
    ps.print (" cdr: ");
    SFormat.print (cdr, ps);
    ps.print ('>');
  }

  public int varCount () { return car_count + cdr_count; }

  static public ClassType classPairPat;
  static Method makePairPatMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (classPairPat == null)
      {
	classPairPat = ClassType.make("kawa.lang.PairPat");
	Type[] apply2args = new Type[2];
	apply2args[0] = Pattern.typePattern;
	apply2args[1] = Pattern.typePattern;
	makePairPatMethod =
	  classPairPat.addMethod ("make", apply2args,
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
    comp.getCode().emitInvokeStatic(makePairPatMethod);
  }
}
