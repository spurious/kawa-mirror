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
  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    exp.args[0].compile(comp, new ArrayType(element_type));
    exp.args[1].compile(comp, Type.int_type);
    CodeAttr code = comp.getCode();
    code.emitArrayLoad(element_type);
    target.compileFromStack(comp, element_type);
  }
}
