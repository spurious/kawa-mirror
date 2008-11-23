package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangPrimType;

public abstract class ArithOp extends ProcedureN implements CanInline, Inlineable
{
  public ArithOp (String name)
  {
    super(name);
  }

  public Object defaultResult ()
  {
    return IntNum.zero();
  }

  public abstract int primitiveOpcode ();

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

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int len = args.length;
    if (len == 0)
      {
	comp.compileConstant(defaultResult(), target);
	return;
      }
    if (len == 1 || target instanceof IgnoreTarget)
      {
	// FIXME implement optimization for unary
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Type type = getReturnType(args);
    int kind = Arithmetic.classifyType(type);
    if (kind == 0)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Type targetType = target.getType();
    int tkind = Arithmetic.classifyType(targetType);
    Type wtype;
    if ((tkind == Arithmetic.INT_CODE || tkind == Arithmetic.LONG_CODE)
        && kind >= Arithmetic.INT_CODE && kind <= Arithmetic.INTNUM_CODE)
      {
        kind = tkind;
        wtype = tkind == Arithmetic.INT_CODE ? LangPrimType.intType
          : LangPrimType.longType;
      }
    else if ((tkind == Arithmetic.DOUBLE_CODE
              || tkind == Arithmetic.FLOAT_CODE)
             
             && kind > 0 && kind <= Arithmetic.REALNUM_CODE)
      {
        kind = tkind;
        wtype = tkind == Arithmetic.FLOAT_CODE ? LangPrimType.floatType
          : LangPrimType.doubleType;

      }
    else if (kind == Arithmetic.FLOAT_CODE)
      wtype = LangPrimType.floatType;
    else if (kind == Arithmetic.DOUBLE_CODE || kind == Arithmetic.FLONUM_CODE)
      {
        kind = Arithmetic.DOUBLE_CODE;
        wtype = LangPrimType.doubleType;
      }
    else
      wtype = type;

    if (kind != Arithmetic.INT_CODE
        && kind != Arithmetic.LONG_CODE
        && kind != Arithmetic.FLOAT_CODE
        && kind != Arithmetic.DOUBLE_CODE)
      {
        // FIXME
        ApplyExp.compile(exp, comp, target);
        return;
      }
      
    Target wtarget = StackTarget.getInstance(wtype);

    CodeAttr code = comp.getCode();
    for (int i = 0;  i < len;  i++)
      {
        args[i].compile(comp, wtarget);
        if (i == 0)
          continue;
        switch (kind)
          {
          case Arithmetic.INT_CODE:
          case Arithmetic.LONG_CODE:
          case Arithmetic.FLOAT_CODE:
          case Arithmetic.DOUBLE_CODE:
            code.emitBinop(primitiveOpcode(), (PrimType) wtype);
            break;
          }
      }
    target.compileFromStack(comp, wtype);
  }

  public Type getReturnType (Expression[] args)
  {
    int len = args.length;
    if (len == 0)
      return Arithmetic.typeIntNum;
    Type type = Type.pointer_type;
    int kindr = 0;
    for (int i = 0;  i < len;  i++)
      {
	Expression arg = args[i];
	int kind = Arithmetic.classifyType(arg.getType());

	if (i == 0 || kind == 0 || kind > kindr)
	  kindr = kind;
      }
    return Arithmetic.kindType(kindr);
  }
}
