package kawa.lang;

public class print
{
  public static void print (Object obj, java.io.PrintStream ps)
  {
    if (obj instanceof kawa.lang.Printable)
      ((kawa.lang.Printable)obj).print(ps);
    else if (obj instanceof java.lang.StringBuffer)
      {
	boolean readable = (ps instanceof OutPort)
	  && ((OutPort)ps).printReadable;
	StringBuffer str = (StringBuffer) obj;
	int len = str.length();
	if (readable)
	  ps.print ('\"');
	for (int i = 0;  i < len; i++)
	  {
	    char ch = str.charAt (i);
	    if (readable && (ch == '\\' || ch == '\"'))
	      ps.print ('\\');
	    ps.print (ch);
	  }
	if (readable)
	  ps.print ('\"');
      }
    else if (obj instanceof java.lang.Boolean)
      if (((java.lang.Boolean)obj).booleanValue())
	ps.print("#t");
      else
	ps.print("#f");
    else if (obj == null)
      ps.print("#<null>");
    else
      ps.print(obj.toString ());
  }
}
