package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

public class ArraySet extends Procedure3 implements Inlineable, Externalizable
{
  Type element_type;

  public ArraySet (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply3 (Object array, Object index, Object value)
  {
    java.lang.reflect.Array.set(array,
				((Number) index).intValue(),
				element_type.coerceFromObject(value));
    return Values.empty;
  }
  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    args[0].compile(comp, ArrayType.make(element_type));
    args[1].compile(comp, Type.int_type);
    args[2].compile(comp, element_type);
    comp.getCode().emitArrayStore(element_type);
    comp.compileConstant(Values.empty, target);
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return Type.void_type;
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
