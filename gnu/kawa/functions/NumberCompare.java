package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;

/** This implements the numeric comparison relations: <. <=, etc. */

public class NumberCompare extends ProcedureN
{
  // Return codes from Numeric.compare:
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;
  static final int RESULT_NAN = -2;
  static final int RESULT_NEQ = -3;

  // One flag bit for each of the above RESULT_XXX codes:
  static final int TRUE_IF_GRT = 1 << (RESULT_GRT + 3);
  static final int TRUE_IF_EQU = 1 << (RESULT_EQU + 3);
  static final int TRUE_IF_LSS = 1 << (RESULT_LSS + 3);
  static final int TRUE_IF_NAN = 1 << (RESULT_NAN + 3);
  static final int TRUE_IF_NEQ = 1 << (RESULT_NEQ + 3);
  int flags;

  public int numArgs() { return (-1 << 12) | 2; }

  // The funny name of the static fields and methods correspond to
  // the name mangling scheme defined in Compilation.mangleName.
  // They can therefor be statically resolved by the compiler.

  public static final NumberCompare $Eq   = make("=",TRUE_IF_EQU);
  public static final NumberCompare $Gr   = make(">",TRUE_IF_GRT);
  public static final NumberCompare $Gr$Eq= make(">=",TRUE_IF_GRT|TRUE_IF_EQU);
  public static final NumberCompare $Ls   = make("<",TRUE_IF_LSS);
  public static final NumberCompare $Ls$Eq= make("<=",TRUE_IF_LSS|TRUE_IF_EQU);

  public static boolean $Eq(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_EQU, arg1, arg2);
  }

  public static boolean $Gr(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_GRT, arg1, arg2);
  }

  public static boolean $Gr$Eq(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_GRT|TRUE_IF_EQU, arg1, arg2);
  }

  public static boolean $Ls(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_LSS, arg1, arg2);
  }

  public static boolean $Ls$Eq(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_LSS|TRUE_IF_EQU, arg1, arg2);
  }
 
  public static boolean $Eq$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Eq(arg1, arg2) && $Eq(arg2, arg3)
	    && (rest.length == 0
		|| ($Eq(arg3, rest[0]) && applyN(TRUE_IF_EQU, rest))));
  }

  public static boolean $Gr$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Gr(arg1, arg2) && $Gr(arg2, arg3)
	    && (rest.length == 0
		|| ($Gr(arg3, rest[0]) && applyN(TRUE_IF_GRT, rest))));
  }

  public static boolean $Gr$Eq$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Gr$Eq(arg1, arg2) && $Gr$Eq(arg2, arg3)
	    && (rest.length == 0
		|| ($Gr$Eq(arg3, rest[0])
		    && applyN(TRUE_IF_GRT|TRUE_IF_EQU, rest))));
  }

  public static boolean $Ls$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Ls(arg1, arg2) && $Ls(arg2, arg3)
	    && (rest.length == 0
		|| ($Ls(arg3, rest[0]) && applyN(TRUE_IF_LSS, rest))));
  }

  public static boolean $Ls$Eq$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Ls$Eq(arg1, arg2) && $Ls$Eq(arg2, arg3)
	    && (rest.length == 0
		|| ($Ls$Eq(arg3, rest[0])
		    && applyN(TRUE_IF_LSS|TRUE_IF_EQU, rest))));
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
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }

  static boolean apply2 (int flags, Object arg1, Object arg2)
  {
    return ((1 << (3 + ((Numeric)arg1).compare(arg2))) & flags) != 0;
  }

  static boolean applyN (int flags, Object[] args)
  {
    //  if (args.length < 2)
    //  throw new WrongArguments(this.name(),2,"(< x1 x2 ...)");
    for (int i = 0;  i < args.length - 1;  i++)
      {
	Object arg1 = args[i];
	Object arg2 = args[i+1];
	if (! apply2(flags, arg1, arg2))
	  return false;
      }
    return true;
  }

  public Object applyN (Object[] args)
  {
    //  if (args.length < 2)
    //  throw new WrongArguments(this.name(),2,"(< x1 x2 ...)");
    return applyN(flags, args) ? Boolean.TRUE : Boolean.FALSE;
  }
}
