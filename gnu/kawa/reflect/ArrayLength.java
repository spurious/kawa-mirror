package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.io.*;

public class ArrayLength 
  extends Procedure1
  implements Externalizable
{
  Type element_type;

  public ArrayLength (Type element_type)
  {
    this.element_type = element_type;
    Procedure.compilerKey.set(this, "*gnu.kawa.reflect.CompileArrays:getForArrayLength");
  }

  public Object apply1 (Object array)
  {
    return gnu.math.IntNum.make(java.lang.reflect.Array.getLength(array));
  }

  public boolean isSideEffectFree ()
  {
    return true;
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
