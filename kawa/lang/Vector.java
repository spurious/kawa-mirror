package kawa.lang;

public class Vector extends Sequence implements Printable {

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

  public void print(java.io.PrintStream ps)
  {
    int size = value.length;
    ps.print("#(");
    for (int t=0; t<size; t++)
      {
	if (t!=0)
	  ps.print(" ");
	kawa.lang.print.print (value[t], ps);
      }
    ps.print(")");
  }
}
