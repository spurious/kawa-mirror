package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

/** This implements the numeric comparison relations: <. <=, etc. */

public class NumberCompare extends ProcedureN implements CanInline, Inlineable
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

  static public boolean apply2 (int flags, Object arg1, Object arg2)
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

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression folded = ApplyExp.inlineIfConstant(this, exp);
    if (folded != exp)
      return folded;
    return exp;
  }

  /*
  public void compareIntegers(Expression arg0, Expression arg1,
			      Compilation comp, Target target)
  {
    compile [[ new ApplyExp(this, { new ApplyExp(<IntNum.compare>, { arg0, arg1 }), QuoteExp.zeroExp }) ]];
  }
  */

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
	if (kind0 >= int_KIND && kind1 >= int_KIND)
	  {
	    if (! (target instanceof ConditionalTarget))
	      {
		IfExp.compile(exp, QuoteExp.trueExp, QuoteExp.falseExp,
			      comp, target);
		return;
	      }
	    StackTarget intTarget = new StackTarget(Type.int_type);
	    ConditionalTarget ctarget = (ConditionalTarget) target;
	    
	    int opcode;
	    int mask = flags;
	    if (mask == TRUE_IF_NEQ)
	      mask = TRUE_IF_GRT|TRUE_IF_LSS;
	    if (arg0 instanceof QuoteExp && ! (arg1 instanceof QuoteExp))
	      {
		Expression tmp = arg1; arg1 = arg0; arg0 = tmp;
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
	    arg0.compile(comp, intTarget);
	    Object value;
	    if (arg1 instanceof QuoteExp
		&& (value = ((QuoteExp) arg1).getValue()) instanceof IntNum
		&& ((IntNum) value).isZero())
	      {
		code.emitGotoIfCompare1(label1, opcode);
	      }
	    else
	      {
		arg1.compile(comp, intTarget);
		code.emitGotoIfCompare2(label1, opcode);
	      }
	    ctarget.emitGotoFirstBranch(code);
	    return;
	  }
	if (kind0 >= IntNum_KIND && kind1 >= IntNum_KIND)
	  {
	    /*
	    compareIntegers(arg0, arg1, comp, target);
	    return;
	    */
	  }
      }
    ApplyExp.compile(exp, comp, target);
  }

  private static final int IntNum_KIND = 5;
  private static final int long_KIND = 6;
  private static final int int_KIND = 7;

  /** Return a code indicate type of number:
   * 0: unknown or invalid type
   * 1: java.lang.Number - ignored for now
   * 2: gnu.math.Numeric
   * 3: gnu.math.RealNum
   * 4  floating primitive type
   * 5: gnu.math.IntNum
   * 6: long (including literal in long range??)
   * 7: int
   */
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
	  return 0;
	if (sig == 'D' || sig == 'F')
	  return 4;
	if (sig == 'J')
	  return long_KIND;
	return int_KIND;
      }
     if (type.isSubtype(AddOp.typeIntNum))
       return IntNum_KIND;
     if (type.isSubtype(AddOp.typeDFloNum))
       return 4;
     if (type.isSubtype(AddOp.typeRealNum))
       return 3;
     if (type.isSubtype(AddOp.typeNumeric))
       return 2;
    return 0;
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.scmBooleanType;
  }
}
