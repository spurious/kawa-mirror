package gnu.kawa.functions;
import gnu.math.IntNum;
import gnu.math.Numeric;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.Type;
import gnu.bytecode.PrimType;

/**
 * Implement the Scheme standard functions "+" and "-".
 * @author Per Bothner
 */

public class AddOp extends ProcedureN implements CanInline
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
                                    Expression rproc, Expression[] args)
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
          prev = ((CanInline) proc).inline(next);
        else
          prev = next;
      }
    return prev;
  }

  public Expression inline (ApplyExp exp)
  {
    Expression folded = ApplyExp.inlineIfConstant(this, exp);
    if (folded != exp)
      return folded;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return pairwise(this, exp.getFunction(), args);
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
                type = Type.double_type;
              }
            else if (sig0 == 'F')
              {
                opcode = 118 /* fneg */;
                type = Type.float_type;
              }
            else if (sig0 == 'J')
              {
                opcode = 117 /* lneg */;
                type = Type.long_type;
              }
            else
              {
                opcode = 116 /* ineg */;
                type = Type.int_type;
              }
            if (type != null)
              {
                PrimProcedure prim
                  = PrimProcedure.makeBuiltinBinary(opcode, type);
                return new ApplyExp(prim, args);
              }
          }
      }
    if (args.length == 2)
      {
        Type type0 = args[0].getType();
        Type type1 = args[1].getType();
        if (type0 instanceof PrimType && type1 instanceof PrimType)
          {
            char sig0 = type0.getSignature().charAt(0);
            char sig1 = type1.getSignature().charAt(0);
            Type type = null;
            int opcode = 0;
            if (sig0 == 'V' || sig0 == 'Z' || sig0 == 'C'
                || sig1 == 'V' || sig1 == 'Z' || sig1 == 'C')
              {
                // error
              }
            else if (sig0 == 'D' || sig1 == 'D')
              {
                opcode = plusOrMinus > 0 ? 99 /* dadd */ : 103 /* dsub */;
                type = Type.double_type;
              }
            else if (sig0 == 'F' || sig1 == 'F')
              {
                opcode = plusOrMinus > 0 ? 98 /* fadd */ : 102 /* fsub */;
                type = Type.float_type;
              }
            else if (sig0 == 'J' || sig1 == 'J')
              {
                opcode = plusOrMinus > 0 ? 97 /* ladd */ : 101 /* lsub */;
                type = Type.long_type;
              }
            else
              {
                opcode = plusOrMinus > 0 ? 96 /* iadd */ : 100 /* isub */;
                type = Type.int_type;
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
}
