package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

public class ArrayNew extends Procedure1 implements Inlineable, Externalizable
{
  Type element_type;

  public ArrayNew (Type element_type)
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
    target.compileFromStack(comp, ArrayType.make(element_type));
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return ArrayType.make(element_type);
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
