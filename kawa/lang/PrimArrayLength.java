package kawa.lang;
import gnu.bytecode.*;

public class PrimArrayLength extends Procedure1 implements Inlineable
{
  Type element_type;

  public PrimArrayLength (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply1 (Object array)
  {
    return gnu.math.IntNum.make(java.lang.reflect.Array.getLength(array));
  }

  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    exp.args[0].compile(comp, 0, new ArrayType(element_type));
    CodeAttr code = comp.getCode();
    code.emitArrayLength();
    if ((flags & Expression.IGNORED) == 0)
      kawa.standard.Scheme.intType.emitCoerceToObject(code);
    else
      code.emitPop(1);
  }
}
