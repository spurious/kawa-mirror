package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import java.math.*;

/** This implements the numeric comparison relations: <, <=, etc. */

public class NumberCompare extends ProcedureN implements CanInline, Inlineable
{
  Language language;

  // Return codes from Numeric.compare:
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;
  static final int RESULT_NAN = -2;
  static final int RESULT_NEQ = -3;

  // One flag bit for each of the above RESULT_XXX codes:
  public static final int TRUE_IF_GRT = 1 << (RESULT_GRT + 3);
  public static final int TRUE_IF_EQU = 1 << (RESULT_EQU + 3);
  public static final int TRUE_IF_LSS = 1 << (RESULT_LSS + 3);
  public static final int TRUE_IF_NAN = 1 << (RESULT_NAN + 3);
  public static final int TRUE_IF_NEQ = 1 << (RESULT_NEQ + 3);
  int flags;

  public int numArgs() { return (-1 << 12) | 2; }

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

  public static NumberCompare make(Language language, String name, int flags)
  {
    NumberCompare proc = new NumberCompare();
    proc.language = language;
    proc.setName(name);
    proc.flags = flags;
    return proc;
  }

  protected final Language getLanguage ()
  {
    return language;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return getLanguage().booleanObject(apply2(flags, arg1, arg2));
  }

  static public boolean apply2 (int flags, Object arg1, Object arg2)
  {
    return ((1 << (3 + compare(arg1, arg2))) & flags) != 0;
  }
  
  /** Compare two numbers.
   * @return 1 if {@code arg1>arg2}; 0 if {@code arg1==arg2};
   * -1 if {@codearg1<arg2}; -2 if either is {@code NaN};
   * -3 if not comparable (either is not a number). */
  static public int compare (Object arg1, Object arg2)
  {
    int code1 = Arithmetic.classifyValue(arg1);
    int code2 = Arithmetic.classifyValue(arg2);
    if (code1 < 0 || code2 < 0)
      return -3;
    int code = code1 < code2 ? code2 : code1;
    int comp; // A Numeric.compare return code: -1, 0, 1, or rarely: -2, or -3.
    switch (code)
      {
      case Arithmetic.INT_CODE:
	int i1 = Arithmetic.asInt(arg1);
	int i2 = Arithmetic.asInt(arg2);
        comp = i1 < i2 ? -1 : i1 > i2 ? 1 : 0;
	break;
      case Arithmetic.LONG_CODE:
	long l1 = Arithmetic.asLong(arg1);
	long l2 = Arithmetic.asLong(arg2);
        comp = l1 < l2 ? -1 : l1 > l2 ? 1 : 0;
        break;
      case Arithmetic.BIGINTEGER_CODE:
	BigInteger bi1 = Arithmetic.asBigInteger(arg1);
	BigInteger bi2 = Arithmetic.asBigInteger(arg2);
	comp = bi1.compareTo(bi2);
        break;
      case Arithmetic.INTNUM_CODE:
	comp = IntNum.compare(Arithmetic.asIntNum(arg1),
                              Arithmetic.asIntNum(arg2));
        break;
      case Arithmetic.BIGDECIMAL_CODE:
	BigDecimal bd1 = Arithmetic.asBigDecimal(arg1);
	BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
	comp = bd1.compareTo(bd2);
        break;
      case Arithmetic.RATNUM_CODE:
	comp = RatNum.compare(Arithmetic.asRatNum(arg1),
                              Arithmetic.asRatNum(arg2));
        break;
      case Arithmetic.FLOAT_CODE:
        if (code1 > Arithmetic.RATNUM_CODE && code2 > Arithmetic.RATNUM_CODE)
          {
            float f1 = Arithmetic.asFloat(arg1);
            float f2 = Arithmetic.asFloat(arg2);
            comp = f1 > f2 ? 1 : f1 < f2 ? -1 : f1 == f2 ? 0 : -2;
            break;
          }
        // else fall through, to handle exact-inexact comparison
      case Arithmetic.DOUBLE_CODE:
      case Arithmetic.FLONUM_CODE:
        if (code1 > Arithmetic.RATNUM_CODE && code2 > Arithmetic.RATNUM_CODE)
          {
            double d1 = Arithmetic.asDouble(arg1);
            double d2 = Arithmetic.asDouble(arg2);
            comp = d1 > d2 ? 1 : d1 < d2 ? -1 : d1 == d2 ? 0 : -2;
            break;
          }
        // else fall through, to handle exact-inexact comparison
      default:
	Numeric num1 = Arithmetic.asNumeric(arg1);
	Numeric num2 = Arithmetic.asNumeric(arg2);
        comp = ((Numeric) num1).compare(num2);
      }
    return comp;
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
    return getLanguage().booleanObject(applyN(flags, args));
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression folded = exp.inlineIfConstant(this, walker);
    if (folded != exp)
      return folded;
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
	Expression arg0 = args[0];
	Expression arg1 = args[1];
	int kind0 = classify(arg0);
	int kind1 = classify(arg1);
	CodeAttr code = comp.getCode();
	if (kind0 >= RealNum_KIND && kind1 >= RealNum_KIND
	    // Don't optimize if both operands are fractions.
	    && (kind0 != RealNum_KIND || kind1 != RealNum_KIND))
	  {
	    if (! (target instanceof ConditionalTarget))
	      {
		IfExp.compile(exp, QuoteExp.trueExp, QuoteExp.falseExp,
			      comp, target);
		return;
	      }
	    int mask = flags;
	    if (mask == TRUE_IF_NEQ)
	      mask = TRUE_IF_GRT|TRUE_IF_LSS;
	    if (kind0 >= IntNum_KIND && kind1 >= IntNum_KIND
		&& (kind0 < long_KIND || kind1 < long_KIND))
	      {
		Type[] ctypes = new Type[2];
		ctypes[0] = AddOp.typeIntNum;
		if (kind1 >= long_KIND)
		  {
		    ctypes[1] = Type.long_type;
		  }
		else if (kind0 >= long_KIND
			 // Simple check to avoid re-ordering side-effects.
			 && (arg0 instanceof QuoteExp
			     || arg1 instanceof QuoteExp
			     || arg0 instanceof ReferenceExp
			     || arg1 instanceof ReferenceExp))
		  {
		    ctypes[1] = Type.long_type;
		    args = new Expression[2];
		    args[0] = arg1;
		    args[1] = arg0;
		    if (mask != TRUE_IF_EQU && mask != TRUE_IF_GRT+TRUE_IF_LSS)
		      mask ^= TRUE_IF_GRT|TRUE_IF_LSS;
		  }
		else
		  ctypes[1] = AddOp.typeIntNum;
		Method cmeth
		  = AddOp.typeIntNum.getDeclaredMethod("compare", ctypes);
		PrimProcedure compare = new PrimProcedure(cmeth);
		arg0 = new ApplyExp(compare, args);
		arg1 = new QuoteExp(IntNum.zero());
		kind0 = kind1 = int_KIND;
	      }
	    Type commonType;
	    if (kind0 >= int_KIND && kind1 >= int_KIND)
	      commonType = Type.int_type;
	    else if (kind0 >= long_KIND && kind1 >= long_KIND)
	      commonType = Type.long_type;
	    else
	      commonType = Type.double_type;
	    StackTarget subTarget = new StackTarget(commonType);
	    ConditionalTarget ctarget = (ConditionalTarget) target;
	    
	    int opcode;
	    if (arg0 instanceof QuoteExp && ! (arg1 instanceof QuoteExp))
	      {
		Expression tmp = arg1; arg1 = arg0; arg0 = tmp;
		if (mask != TRUE_IF_EQU && mask != TRUE_IF_GRT+TRUE_IF_LSS)
		  mask ^= TRUE_IF_GRT|TRUE_IF_LSS;
	      }
	    Label label1 = ctarget.trueBranchComesFirst ? ctarget.ifFalse : ctarget.ifTrue;
	    if (ctarget.trueBranchComesFirst)
	      mask ^= TRUE_IF_GRT|TRUE_IF_LSS|TRUE_IF_EQU;
	    switch (mask)
	      {
	      case TRUE_IF_GRT:  opcode = 157 /*ifgt*/;  break;
	      case TRUE_IF_EQU:  opcode = 153 /*ifeq*/;  break;
	      case TRUE_IF_LSS:  opcode = 155 /*iflt*/;  break;
	      case TRUE_IF_GRT|TRUE_IF_LSS:  opcode = 154 /*ifne*/;  break;
	      case TRUE_IF_GRT|TRUE_IF_EQU:  opcode = 156 /*ifge*/;  break;
	      case TRUE_IF_LSS|TRUE_IF_EQU:  opcode = 158 /*ifle*/;  break;
	      default:
		opcode = 0;
	      }
	    arg0.compile(comp, subTarget);
	    Object value;
	    if (kind0 >= int_KIND && kind1 >= int_KIND
		&& arg1 instanceof QuoteExp
		&& (value = ((QuoteExp) arg1).getValue()) instanceof IntNum
		&& ((IntNum) value).isZero())
	      {
		code.emitGotoIfCompare1(label1, opcode);
	      }
	    else
	      {
		arg1.compile(comp, subTarget);
		code.emitGotoIfCompare2(label1, opcode);
	      }
	    ctarget.emitGotoFirstBranch(code);
	    return;
	  }
      }
    ApplyExp.compile(exp, comp, target);
  }

  // Return a code indicate type of number:
  private static final int Unknown_KIND = 0; // unknown or invalid type
  private static final int Number_KIND = 1; // java.lang.Number - not used
  private static final int Numeric_KIND = 2;  // gnu.math.Numeric
  private static final int RealNum_KIND = 3; // exact or unknown real
  private static final int double_KIND = 4; // inexact real (double or DFloNum)
  private static final int IntNum_KIND = 5; // gnu.math.IntNum
  private static final int long_KIND = 6; // long
  private static final int int_KIND = 7; // int

  static int classify (Expression exp)
  {
    Type type = exp.getType();
    int kind = classify(type);
    Object value;
    if (kind == IntNum_KIND && exp instanceof QuoteExp
	&& (value = ((QuoteExp) exp).getValue()) instanceof IntNum)
      {
	int ilength = ((IntNum) value).intLength();
	if (ilength < 32)
	  return int_KIND;
	if (ilength < 64)
	  return long_KIND;
      }
    return kind;
  }

  static int classify (Type type)
  {
    if (type instanceof PrimType)
      {
	char sig = type.getSignature().charAt(0);
	if (sig == 'V' || sig == 'Z' || sig == 'C')
	  return Unknown_KIND;
	if (sig == 'D' || sig == 'F')
	  return double_KIND;
	if (sig == 'J')
	  return long_KIND;
	return int_KIND;
      }
     if (type.isSubtype(AddOp.typeIntNum))
       return IntNum_KIND;
     if (type.isSubtype(AddOp.typeDFloNum))
       return double_KIND;
     if (type.isSubtype(AddOp.typeRealNum))
       return RealNum_KIND;
     if (type.isSubtype(AddOp.typeNumeric))
       return Numeric_KIND;
    return Unknown_KIND;
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.scmBooleanType;
  }
}
