package kawa.lang;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

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

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    exp.getArgs()[0].compile(comp, Type.int_type);
    CodeAttr code = comp.getCode();
    code.emitNewArray(element_type);
    target.compileFromStack(comp, new ArrayType(element_type));
  }
}
