package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.io.*;

public class ArraySet extends Procedure3 implements Externalizable
{
  Type element_type;

  public ArraySet (Type element_type)
  {
    this.element_type = element_type;
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.reflect.CompileArrays:validateArraySet");
    Procedure.compilerKey.set(this, "*gnu.kawa.reflect.CompileArrays:getForArraySet");
  }

  public Object apply3 (Object array, Object index, Object value)
  {
    java.lang.reflect.Array.set(array,
				((Number) index).intValue(),
				element_type.coerceFromObject(value));
    return Values.empty;
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
