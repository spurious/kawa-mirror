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

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    exp.args[0].compile(comp, new ArrayType(element_type));
    CodeAttr code = comp.getCode();
    code.emitArrayLength();
    target.compileFromStack(comp, kawa.standard.Scheme.intType);
  }
}
