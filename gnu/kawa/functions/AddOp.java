// Copyright (c) 2000, 2001, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.math.IntNum;
import gnu.math.Numeric;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangPrimType;

/**
 * Implement the Scheme standard functions "+" and "-".
 * @author Per Bothner
 */

public class AddOp extends ProcedureN implements CanInline, Inlineable
{
  int plusOrMinus = 1;

  public AddOp(String name, int plusOrMinus)
  {
    setName(name);
    this.plusOrMinus = plusOrMinus;
  }

  public static final AddOp $Pl = new AddOp("+", 1);
  public static final AddOp $Mn = new AddOp("-", -1);

  public static Object apply2(int plusOrMinus, Object arg1, Object arg2)
  {
    Numeric num1 = (Numeric) arg1;
    Numeric num2 = (Numeric) arg2;
    return num1.add(num2, plusOrMinus);
  }

  public static Object $Pl(Object arg1, Object arg2)
  {
    return apply2(1, arg1, arg2);
  }

  public static Object $Mn(Object arg1, Object arg2)
  {
    return apply2(-1, arg1, arg2);
  }

  public static Object $Mn(Object arg1)
  {
    return ((Numeric) arg1).neg();
  }

  public static Object $Pl$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(1, apply2(1,apply2(1, arg1, arg2), arg3), rest);
  }

  public static Object $Mn$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(-1, apply2(-1,apply2(-1, arg1, arg2), arg3), rest);
  }

  public static Object applyN(int plusOrMinus, Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    Object result = args[0];
    if (len == 1 && plusOrMinus < 0)
      return $Mn(result);
    for (int i = 1; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public static Object applyN(int plusOrMinus, Object init, Object[] args)
  {
    int len = args.length;
    Object result = init;
    for (int i = 0; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return applyN(plusOrMinus, args);
  }

  /** Convert (PROC A B C) to (PROC (PROC A B) C) etc. */

  public static Expression pairwise(Procedure proc,
                                    Expression rproc, Expression[] args,
				    ExpWalker walker)
  {
    int len = args.length;
    Expression prev = args[0];
    for (int i = 1;  i < len;  i++)
      {
        Expression[] args2 = new Expression[2];
        args2[0] = prev;
        args2[1] = args[i];
        ApplyExp next = new ApplyExp(rproc, args2);
        if (proc instanceof CanInline)
          prev = ((CanInline) proc).inline(next, walker);
        else
          prev = next;
      }
    return prev;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression folded = exp.inlineIfConstant(this, walker);
    if (folded != exp)
      return folded;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return pairwise(this, exp.getFunction(), args, walker);
    if (args.length == 1 && plusOrMinus < 0)
      {
        Type type0 = args[0].getType();
        if (type0 instanceof PrimType)
          {
            char sig0 = type0.getSignature().charAt(0);
            Type type = null;
            int opcode = 0;
            if (sig0 == 'V' || sig0 == 'Z' || sig0 == 'C')
              {
                // error
              }
            else if (sig0 == 'D')
              {
                opcode = 119 /* dneg */;
                type = LangPrimType.doubleType;
              }
            else if (sig0 == 'F')
              {
                opcode = 118 /* fneg */;
                type = LangPrimType.floatType;
              }
            else if (sig0 == 'J')
              {
                opcode = 117 /* lneg */;
                type = LangPrimType.longType;
              }
            else
              {
                opcode = 116 /* ineg */;
                type = LangPrimType.intType;
              }
            if (type != null)
              {
                PrimProcedure prim
                  = PrimProcedure.makeBuiltinUnary(opcode, type);
                return new ApplyExp(prim, args);
              }
          }
      }
    if (args.length == 2)
      {
	return primInline(plusOrMinus > 0 ? 96 /* iadd */ : 100 /* isub */,
			  exp);
      }
    return exp;
  }

  public static Expression primInline (int opcode, ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Type type0 = args[0].getType();
        Type type1 = args[1].getType();
        if (type0 instanceof PrimType && type1 instanceof PrimType)
          {
            char sig0 = type0.getSignature().charAt(0);
            char sig1 = type1.getSignature().charAt(0);
            Type type = null;
            if (sig0 == 'V' || sig0 == 'Z' || sig0 == 'C'
                || sig1 == 'V' || sig1 == 'Z' || sig1 == 'C')
              {
                // error
              }
            else if (sig0 == 'D' || sig1 == 'D')
              {
                opcode += 3;
                type = LangPrimType.doubleType;
              }
            else if (sig0 == 'F' || sig1 == 'F')
              {
                opcode += 2;
                type = LangPrimType.floatType;
              }
            else if (sig0 == 'J' || sig1 == 'J')
              {
                opcode += 1;
                type = LangPrimType.longType;
              }
            else
              {
                type = LangPrimType.intType;
              }
            if (type != null)
              {
                PrimProcedure prim
                  = PrimProcedure.makeBuiltinBinary(opcode, type);
                return new ApplyExp(prim, args);
              }
          }
      }
    return exp;
  }

  /*
  static ClassType typeInteger = ClassType.make("java.lang.Integer");
  static ClassType typeLong = ClassType.make("java.lang.Long");
  */
  static ClassType typeIntNum = ClassType.make("gnu.math.IntNum");
  static ClassType typeDFloNum = ClassType.make("gnu.math.DFloNum");
  static ClassType typeRealNum = ClassType.make("gnu.math.RealNum");
  static ClassType typeNumeric = ClassType.make("gnu.math.Numeric");

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int len = args.length;
    if (len == 0)
      {
	comp.compileConstant(IntNum.zero(), target);
	return;
      }
    Type type = getReturnType(args);
    Type ttype = target.getType();
    if (len == 1 || target instanceof IgnoreTarget)
      {
	// FIXME implement optimization for unary
	ApplyExp.compile(exp, comp, target);
	return;
      }
    PrimType ptype = null;
    if (ttype instanceof PrimType)
      {
	char sig = type.getSignature().charAt(0);
	if (sig == 'V' || sig == 'Z' || sig == 'C')
	 ptype = null; // error
	else if (sig == 'D' || sig == 'F')
	  {
	    if (type.isSubtype(typeRealNum))
	      ptype = LangPrimType.doubleType;
	  }
	else
	  {
	    if (type.isSubtype(typeIntNum))
	      ptype = sig == 'J' ? LangPrimType.longType : LangPrimType.intType;
	  }
      }
    if (ptype != null)
      {
	CodeAttr code = comp.getCode();
	// FIXME would be nice to use iinc when appropriate!
	// We would need to use a special LocalVariableTarget,
	// created by SetExp when dest is a local variable.
	// Then if len==2 && ptype==LangPrimType.intType
	// && target instanceof LocalVariableTarget
	// && one arg is QuoteExp && other arg is same local as target
	// => then emit iinc.
	args[0].compile(comp, ttype);
	for (int i = 1;  i < len;  i++)
	  {
	    args[i].compile(comp, ptype);
	    if (plusOrMinus > 0)
	      code.emitAdd(ptype);
	    else
	      code.emitSub(ptype);
	  }
	target.compileFromStack(comp, ttype);
      }
    else if (type.isSubtype(typeDFloNum))
      {
	PrimType dtype = Type.double_type;
	Target dtarget = new StackTarget(dtype);
	CodeAttr code = comp.getCode();
	args[0].compile(comp, dtarget);
	for (int i = 1;  i < len;  i++)
	  {
	    args[i].compile(comp, dtarget);
	    if (plusOrMinus > 0)
	      code.emitAdd(dtype);
	    else
	      code.emitSub(dtype);
	  }
	target.compileFromStack(comp, dtype);
      }
    else
      ApplyExp.compile(exp, comp, target);
    
  }

  /** Claassify an expression according to its numeric type.
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
    else if (type.isSubtype(typeIntNum))
      return 4;
    else if (type.isSubtype(typeDFloNum))
      return 3;
    else if (type.isSubtype(typeRealNum))
      return 2;
    else if (type.isSubtype(typeNumeric))
      return 1;
    else
      return 0;
  }

  public Type getReturnType (Expression[] args)
  {
    int len = args.length;
    if (len == 0)
      return typeIntNum;
    Type type = Type.pointer_type;
    int kind0 = 0;
    for (int i = 0;  i < len;  i++)
      {
	Expression arg = args[i];
	int kind = classify(arg.getType());

	if (kind == 0)
	  return Type.pointer_type;

	if (i == 0)
	  kind0 = kind;

	if (kind0 == 4 && kind == 4)
	  type = typeIntNum;
	else if (kind0 >= 3 && kind >= 3)
	  {
	    type = typeDFloNum;
	    kind0 = 3;
	  }
	else if (kind0 >= 2 && kind >= 2)
	  {
	    if (kind0 >= 3 || kind >= 3)
	      {
		type = typeDFloNum;
		kind0 = 3;
	      }
	    else
	      {
		type = typeRealNum;
		kind0 = 2;
	      }
	  }
	else if (kind0 >= 1 && kind >= 1)
	  {
	    type = typeNumeric;
	    kind0 = 1;
	  }
	else
	  return Type.pointer_type;
      }
    return type;
  }
}
