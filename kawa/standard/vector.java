package kawa.standard;

public class vector extends kawa.lang.ProcedureN
{
  public vector()
  {
    super("vector");
  }

  public Object applyN (Object[] args)
  {
    int count = args.length;
    java.util.Vector v = new java.util.Vector();
    for (int i = 0;  i < count; i++)
      v.addElement (args[i]);
    return new kawa.lang.vector(v);
  }
}
