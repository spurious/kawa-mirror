package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangPrimType;

public class ArrayLength extends Procedure1 implements Inlineable
{
  Type element_type;

  public ArrayLength (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply1 (Object array)
  {
    return gnu.math.IntNum.make(java.lang.reflect.Array.getLength(array));
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    exp.getArgs()[0].compile(comp, new ArrayType(element_type));
    CodeAttr code = comp.getCode();
    code.emitArrayLength();
    target.compileFromStack(comp, gnu.kawa.lispexpr.LangPrimType.intType);
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return gnu.kawa.lispexpr.LangPrimType.intType;
  }
}
