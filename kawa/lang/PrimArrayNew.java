package kawa.lang;
import gnu.bytecode.*;

public class PrimArrayNew extends Procedure1 implements Inlineable
{
  Type element_type;

  public PrimArrayNew (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply1 (Object count)
  {
    return java.lang.reflect.Array.newInstance(element_type.getReflectClass(),
					       ((Number) count).intValue());
  }

  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    exp.args[0].compile(comp, 0, Type.int_type);
    CodeAttr code = comp.getCode();
    code.emitNewArray(element_type);
    if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
  }
}
