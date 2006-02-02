package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

public class ArrayLength 
  extends Procedure1
  implements Inlineable, Externalizable
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
    exp.getArgs()[0].compile(comp, ArrayType.make(element_type));
    CodeAttr code = comp.getCode();
    code.emitArrayLength();
    target.compileFromStack(comp, gnu.kawa.lispexpr.LangPrimType.intType);
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return gnu.kawa.lispexpr.LangPrimType.intType;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(element_type);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    element_type = (Type) in.readObject();
  }
}
