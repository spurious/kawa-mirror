package gnu.kawa.util;
import java.io.*;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.*;

public class FVector extends UniformVector implements Printable, Compilable, Externalizable
{
  public String getTag() { return ""; }

  Object[] value;

  public FVector ()
  {
  }

  public FVector (int num, Object o)
  {
    value = new Object[num];
    for (int i = 0;  i < num;  i++)
      value[i] = o;
  }

  public FVector (Object[] values)
  {
    value = values;
  }

  public final int length ()
  {
    return value.length;
  }

  public final Object get (int index)
  {
    return value[index];
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof FVector))
      return false;
    FVector obj_vec = (FVector) obj;
    int n = value.length;
    if (obj_vec.value == null || obj_vec.value.length != n)
      return false;
    Object[] obj_value = obj_vec.value;
    for (int i = 0;  i < n;  i++)
      {
	if (! (value[i].equals (obj_value[i])))
	  return false;
      }
    return true;
  }

  public final void setElementAt (Object new_value, int index)
  {
    value[index] = new_value;
  }

  public final void setAll (Object new_value)
  {
     for (int index = value.length; --index >= 0; )
       value[index] = new_value;
  }

  static public ClassType scmVectorType;
  static public Method initVectorMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (scmVectorType == null)
      {
	scmVectorType = ClassType.make("gnu.kawa.util.FVector");
	initVectorMethod
	  = scmVectorType.addMethod ("<init>", comp.applyNargs,
				      Type.void_type, Access.PUBLIC);
      }
    Literal literal = new Literal (this, scmVectorType, comp);
    comp.findLiteral (value);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    // FIXME - should just comp.findLiteral (value) - but handle circularity!
    int len = value.length;
    // Allocate the FVector object
    code.emitNew(scmVectorType);
    code.emitPushInt(len);
    code.emitNewArray(comp.typeObject);
    // Stack contents:  ..., FVector, array
    code.emitDup(2, 0);  // dup2
    // Stack contents:  ..., FVector, array, FVector, array
    code.emitInvokeSpecial(initVectorMethod);
    literal.flags |= Literal.ALLOCATED;

    // Stack contents:  ..., FVector, array
    // Initialize the FVector elements.
    for (int i = 0;  i < len;  i++)
      {
	code.emitDup(scmVectorType);
	code.emitPushInt(i);
	comp.emitLiteral (value[i]);
	// Stack contents:  ..., FVector, array, array, i, value[i]
	code.emitArrayStore(comp.typeObject);
	// Stack contents:  ..., FVector, array
      }
    // Remove no-longer-needed array from stack:
    code.emitPop(1);
  }

  public void print(java.io.PrintWriter ps)
  {
    int size = value.length;
    ps.print("#(");
    for (int t=0; t<size; t++)
      {
	if (t!=0)
	  ps.print(" ");
	SFormat.print (value[t], ps);
      }
    ps.print(")");
  }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeObject).
   *   (It might seem simpler (and increase sharing) to just call
   *   writeObject(value), but that exposes the implementation.)
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = value.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeObject(value[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    Object[] value = new Object[len];
    for (int i = 0;  i < len;  i++)
      value[i] = in.readObject();
    this.value = value;
  }
}
