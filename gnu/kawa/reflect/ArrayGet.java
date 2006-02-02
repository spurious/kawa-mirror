package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

public class ArrayGet extends Procedure2 implements Inlineable, Externalizable
{
  Type element_type;

  public ArrayGet (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply2 (Object array, Object index)
  {
    return java.lang.reflect.Array.get(array, ((Number) index).intValue());
  }
  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    args[0].compile(comp, ArrayType.make(element_type));
    args[1].compile(comp, Type.int_type);
    CodeAttr code = comp.getCode();
    code.emitArrayLoad(element_type);
    target.compileFromStack(comp, element_type);
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return element_type;
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
