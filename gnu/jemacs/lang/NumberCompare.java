package gnu.jemacs.lang;
import gnu.math.*;
import gnu.mapping.*;

public class NumberCompare extends ProcedureN
{
  // Return codes from Numeric.compare:
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;
  static final int RESULT_NAN = -2;
  static final int RESULT_NEQ = -3;

  static final int TRUE_IF_GRT = 16;
  static final int TRUE_IF_EQU = 8;
  static final int TRUE_IF_LSS = 4;
  static final int TRUE_IF_NAN = 2;
  static final int TRUE_IF_NEQ = 1;
  int flags;

  public static NumberCompare makeGrt(String name)
  {
    return make(name, TRUE_IF_GRT);
  }

  public static NumberCompare makeGEq(String name)
  {
    return make(name, TRUE_IF_GRT|TRUE_IF_EQU);
  }

  public static NumberCompare makeLss(String name)
  {
    return make(name, TRUE_IF_LSS);
  }

  public static NumberCompare makeLEq(String name)
  {
    return make(name, TRUE_IF_LSS|TRUE_IF_EQU);
  }

  public static NumberCompare make(String name, int flags)
  {
    NumberCompare proc = new NumberCompare();
    proc.setName(name);
    proc.flags = flags;
    return proc;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (apply2(flags, arg1, arg2))
      return ELisp.TRUE;
    else
      return ELisp.FALSE;
  }

  public static boolean apply2 (int flags, Object arg1, Object arg2)
  {
    Numeric num1 = ELisp.asNumber(arg1);
    Numeric num2 = ELisp.asNumber(arg2);
    return ((1 << (3 + num1.compare(num2))) & flags) != 0;
  }

  public Object applyN (Object[] args)
  {
    //  if (args.length < 2)
    //  throw new WrongArguments(this.name(),2,"(< x1 x2 ...)");
    for (int i = 0;  i < args.length - 1;  i++)
      {
	Object arg1 = args[i];
	Object arg2 = args[i+1];
	if (! apply2(flags, arg1, arg2))
	  return ELisp.FALSE;
      }
    return ELisp.TRUE;
  }
}
