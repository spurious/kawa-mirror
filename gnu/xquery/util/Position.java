// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.math.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class Position extends Procedure0 implements Inlineable
{
  public static final Position position = new Position();

  public Object apply0 ()
  {
     Focus focus = Focus.getCurrent();
     return gnu.math.IntNum.make(focus.position);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    CodeAttr code = comp.getCode();
    Focus.compileGetCurrent(comp);
    code.emitGetField(Focus.TYPE.getDeclaredField("position"));
    target.compileFromStack(comp, Type.long_type);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.long_type;
  }

}
