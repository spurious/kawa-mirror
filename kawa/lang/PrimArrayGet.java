package kawa.lang;
import gnu.bytecode.*;

public class PrimArrayGet extends Procedure2 implements Inlineable
{
  Type element_type;

  public PrimArrayGet (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply2 (Object array, Object index)
  {
    return java.lang.reflect.Array.get(array, ((Number) index).intValue());
  }
  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    exp.args[0].compile(comp, 0, new ArrayType(element_type));
    exp.args[1].compile(comp, 0, Type.int_type);
    CodeAttr code = comp.getCode();
    code.emitArrayLoad(element_type);
    if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
  }
}
