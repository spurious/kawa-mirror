package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.math.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;

/** Implement that standard Scheme function "eqv?". */

public class eqv_p extends Procedure2 implements Inlineable
{
  public boolean apply (Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return true;
    if (arg1 instanceof Char || arg1 instanceof Numeric)
      return arg1.equals (arg2);
    return false;
   }

  public Object apply2 (Object arg1, Object arg2)
  {
    return Interpreter.boolObject(apply(arg1, arg2));
   }

  private static boolean nonNumeric(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object value = ((QuoteExp) exp).getValue();
        return ! (value instanceof Numeric || value instanceof Char);
      }
    return false;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (nonNumeric(args[0]) || nonNumeric(args[1]))
      eq_p.compile(args, comp, target);
    else
      ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    return Scheme.booleanType;
  }

}
