package kawa.lang;

public class print
{
  public static void print (Object obj, java.io.PrintStream ps)
  {
    if (obj instanceof kawa.lang.Printable)
      ((kawa.lang.Printable)obj).print(ps);
    else if (obj instanceof java.lang.StringBuffer)
      ps.print(((java.lang.StringBuffer)obj).toString());
    else if (obj instanceof java.lang.Integer)
      ps.print(((java.lang.Integer)obj).toString());
    else if (obj instanceof java.lang.Double)
      ps.print(((java.lang.Double)obj).toString());
    else if (obj instanceof java.lang.Boolean)
      if (((java.lang.Boolean)obj).booleanValue())
	ps.print("#t");
      else
	ps.print("#f");
    else if (obj instanceof java.lang.Character)
      ps.print(((java.lang.Character)obj).charValue());
    else if (obj == null)
      ps.print("#<null>");
    else
      ps.print("#<"+obj.getClass().getName()+">");
  }
}
