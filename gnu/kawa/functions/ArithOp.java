package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangPrimType;

public abstract class ArithOp extends ProcedureN
{
  static final int ADD = 1;
  static final int SUB = 2;
  static final int MUL = 3;
  static final int DIV = 4;
  static final int REM = 5;
  public static final int ASHIFT_GENERAL = 6;
  public static final int ASHIFT_LEFT = 7;
  public static final int ASHIFT_RIGHT = 8;
  public static final int LSHIFT_RIGHT = 9; // Not implemented yet.
  public static final int AND = 10;
  public static final int IOR = 11;
  public static final int XOR = 12;
  public static final int NOT = 13;

  public ArithOp (String name)
  {
    super(name);
  }

  public Object defaultResult ()
  {
    return IntNum.zero();
  }

  public boolean isSideEffectFree ()
  {
    return true;
  }

  /** Classify an expression according to its numeric type.
   * kind==0:  not a number.
   * kind==1:  a non-real number
   * kind==2:  real number
   * kind==3:  floating-point
   * kind==4:  exact integer
   */
  public static int classify (Type type)
  {
    int kind = 0;
    if (type instanceof PrimType)
      {
	char sig = type.getSignature().charAt(0);
	if (sig == 'V' || sig == 'Z' || sig == 'C')
	  return 0;
	else if (sig == 'D' || sig == 'F')
	  return 3;
	else
	  return 4;
      }
    else if (type.isSubtype(Arithmetic.typeIntNum))
      return 4;
    else if (type.isSubtype(Arithmetic.typeDFloNum))
      return 3;
    else if (type.isSubtype(Arithmetic.typeRealNum))
      return 2;
    else if (type.isSubtype(Arithmetic.typeNumeric))
      return 1;
    else
      return 0;
  }
}
