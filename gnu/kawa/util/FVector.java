package gnu.kawa.util;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.*;

public class FVector extends Sequence implements Printable, Compilable
{

  Object[] value;

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

  public final Object set (int index, Object new_value)
  {
    Object old = value[index];
    value[index] = new_value;
    return old;
  }

  static public ClassType scmVectorType;
  static public Method initVectorMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (scmVectorType == null)
      {
	scmVectorType = ClassType.make("kawa.lang.Vector");
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
}
