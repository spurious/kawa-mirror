package gnu.expr;
import gnu.mapping.*;

/** Utility class containing various routines to manipulate Scheme symbols.
  * Note Scheme symbols are represented using java.lang.String objects,
  * and there are no Symbol objects. */

public class Symbols
{
  /** There are no instances of this class. */
  private Symbols ()
  {
  }

  private static int gensym_counter;

  /* DEBUGGING:
  static java.util.Vector gensyms = new java.util.Vector();

  public static String show (String str)
  {
    StringBuffer buf = new StringBuffer(str);
    if (str.intern() == str)
      buf.append("<I>");
    else
      {
	for (int i = gensyms.size();  ; )
	  {
	    if (--i < 0)
	      {
		buf.append("<?>");
		break;
	      }
	    else if (gensyms.elementAt(i) == str)
	      {
		buf.append('<');
		buf.append(i);
		buf.append('>');
		break;
	      }
	  }
      }
    return buf.toString();
  }
  */

  /**
   * Generate a new un-interned Symbol with a unique name.
   * @return the new Symbol
   */
  public static final String generate ()
  {
    String str = new String ("GS." + Integer.toString(++gensym_counter));
    /* DEBUGGING:
    gensyms.addElement(str);
    */
    return str;
  }

  /**
   * Generate a new (interned) symbol with a unique name.
   * @return the new symbol
   */
  public static final String gentemp ()
  {
    return Symbols.make("GS." + Integer.toString(++gensym_counter));
  }

  /**
   * Create or find a Symbol with a given name.
   * @param name the print-name of the desired Symbol
   * @return a Symbol with the given name, newly created iff none such exist
   */
  static public String make (String name)
  {
    return name.intern();
  }

  static public final String intern (String name)
  {
    return make (name);
  }

  public static void print(String name, java.io.PrintWriter ps)
  {
    boolean readable = (ps instanceof OutPort)
      && ((OutPort)ps).printReadable;
    if (readable)
      {
	int len = name.length ();
	for (int i = 0;  i < len;  i++)
	  {
	    char ch = name.charAt (i);
	    if (!(Character.isLowerCase (ch)
		  || ch == '!' || ch == '$' || ch == '%' || ch == '&'
		  || ch == '*' || ch == '/' || ch == ':' || ch == '<'
		  || ch == '=' || ch == '>' || ch == '?' || ch == '~'
		  || ch == '_' || ch == '^'
		  || ((ch == '+' || ch == '-') && (i > 0 || len == 1))
		  || (Character.isDigit (ch) && i > 0)
		  || (ch == '.' && (i == 0 || name.charAt (i - 1) == '.'))))
	      ps.print ('\\');
	    ps.print (ch);
	  }
      }
    else
      ps.print(name);
  }

}
