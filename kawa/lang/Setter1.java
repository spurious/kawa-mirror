package kawa.lang;

/** A special case of Setter, retricted to one argument (plus the RHS). */

public class Setter1 extends Setter
{
  public Setter1(Procedure getter) { super(getter); }

  public int numArgs() { return 0x2002; }

  public Object apply2(Object result, Object arg)
  { getter.set1(result, arg);  return Interpreter.voidObject; }

  public Object applyN(Object[] args)
  {
    int nargs = args.length;
    if (nargs != 2)
      throw new WrongArguments(this, nargs);
    getter.set1(args[0], args[1]);
    return Interpreter.voidObject;
  }
}
