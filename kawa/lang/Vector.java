package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;

public class Vector extends Sequence implements Printable, Compilable
{

  Object[] value;

  public Vector (int num, Object o)
  {
    value = new Object[num];
    for (int i = 0;  i < num;  i++)
      value[i] = o;
  }

  public Vector (Object[] values)
  {
    value = values;
  }

  public final int length ()
  {
    return value.length;
  }

  public final Object elementAt (int index)
  {
    return value[index];
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof Vector))
      return false;
    Vector obj_vec = (Vector) obj;
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

  static public ClassType scmVectorType;
  static public Method initVectorMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (scmVectorType == null)
      {
	scmVectorType = new ClassType ("kawa.lang.Vector");
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
    // FIXME - should just comp.findLiteral (value) - but handle circularity!
    int len = value.length;
    // Allocate the Vector object
    comp.method.compile_new (scmVectorType);
    comp.method.compile_push_int (len);
    comp.method.compile_new_array (comp.scmObjectType);
    // Stack contents:  ..., Vector, array
    comp.method.compile_dup (2, 0);  // dup2
    // Stack contents:  ..., Vector, array, Vector, array
    comp.method.compile_invoke_special (initVectorMethod);
    literal.flags |= Literal.ALLOCATED;

    // Stack contents:  ..., Vector, array
    // Initialize the Vector elements.
    for (int i = 0;  i < len;  i++)
      {
	comp.method.compile_dup (scmVectorType);
	comp.method.compile_push_int (i);
	comp.emitLiteral (value[i]);
	// Stack contents:  ..., Vector, array, array, i, value[i]
	comp.method.compile_array_store (comp.scmObjectType);
	// Stack contents:  ..., Vector, array
      }
    // Remove no-longer-needed array from stack:
    comp.method.compile_pop (1);
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
}
