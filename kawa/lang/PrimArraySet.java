package kawa.lang;
import gnu.bytecode.*;

public class PrimArraySet extends Procedure3 implements Inlineable
{
  Type element_type;

  public PrimArraySet (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply3 (Object array, Object index, Object value)
  {
    java.lang.reflect.Array.set(array,
				((Number) index).intValue(),
				element_type.coerceFromObject(value));
    return Interpreter.voidObject;
  }
  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    exp.args[0].compile(comp, 0, new ArrayType(element_type));
    exp.args[1].compile(comp, 0, Type.int_type);
    exp.args[2].compile(comp, 0, element_type);
    comp.getCode().emitArrayStore(element_type);
    if ((flags & Expression.IGNORED) == 0)
      comp.compileConstant (Interpreter.voidObject);
  }
}
