package gnu.expr;
import gnu.mapping.*;
import java.io.*;
import gnu.lists.*;

public class Keyword extends CpsProcedure implements Printable, Externalizable
{
  // Does not include final ':'.
  private String name;

  public Keyword()
  {
  }

  private Keyword (String name)
  {
    this.name = name;
  }

  private static java.util.Hashtable keywordTable = new java.util.Hashtable ();

  public int hashCode () { return name.hashCode (); }

  /**
   * Create or find a Keyword with a given name (without final ':').
   * @param name the print-name of the desired Keyword
   * @return a Keyword with the given name, newly created iff none such exist
   */
  static public Keyword make (String name)
  {
    Keyword keyword = (Keyword) keywordTable.get (name);
    if (keyword == null) {
      keyword = new Keyword (name);
      keywordTable.put (name, keyword);
    }
    return keyword;
  }

  /*
  public FString toSchemeString()
  {
    return new FString(name);
  }
  */

  public static boolean isKeyword (Object obj)
  {
    return obj instanceof Keyword;
  }

  public final String toString()
  {
    return name+':';
  }

  public void print(java.io.PrintWriter ps)
  {
    Symbol.print(name, ps);
    ps.print(':');
  }

  /**
   * Search vals[0:offset-1] for a keyword.
   * Each key at vals[i] is followed by a value at keys[i+1].
   * (This is used to search for a keyword parameter in an argument list.)
   * @param vals the list to search in
   * @param offset the index in vals to start the search at
   * @param keyword the keyword to search for
   * @return vals[i+1] such that vals[i]==keyword (and (i-offset) is even
   * and non-negative);  if there is no such i, return Special.dfault.
   */
  public static Object searchForKeyword (Object[] vals,
					 int offset, Object keyword)
  {
    for (int i = offset;  i < vals.length;  i += 2)
      {
	if (vals[i] == keyword)
	  return vals[i+1];
      }
    return Special.dfault;
  }

  /**
   * Search vals[0:offset-1] for a keyword.
   * Each key at vals[i] is followed by a value at keys[i+1].
   * (This is used to search for a keyword parameter in an argument list.)
   * @param vals the list to search in
   * @param offset the index in vals to start the search at
   * @param keyword the keyword to search for
   * @param dfault the value to return if there is no match
   * @return vals[i+1] such that vals[i]==keyword (and (i-offset) is even
   * and non-negative);  if there is no such i, return dfault.
   */
  public static Object searchForKeyword (Object[] vals,
					 int offset, Object keyword,
					 Object dfault)
  {
    for (int i = offset;  i < vals.length;  i += 2)
      {
	if (vals[i] == keyword)
	  return vals[i+1];
      }
    return dfault;
  }

  public final String getName() { return name; }
  
  /**
   * @serialData Write the keyword name (without colons) using writeUTF.
   */

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeUTF(name);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    name = in.readUTF();
  }

  public Object readResolve() throws ObjectStreamException
  {
    return make(name);
  }

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    String tag = getName();
    int nargs = ctx.count;
    out.beginGroup(tag, this);
    for (int i = 0;  i < nargs;  i++)
      {
	Object arg = ctx.getArgAsObject(i);
	if (arg instanceof Keyword && i + 1 < nargs)
	  {
	    Keyword attr = (Keyword) arg;
	    arg = ctx.getArgAsObject(++i);
	    out.beginAttribute(attr.getName(), attr);
	    out.writeObject(arg);
	    out.endAttribute();
	  }
	else
	  {
	    /*
	    if (arg instanceof Consumable)
	      ((Consumable) arg).consume(out);
	    else
	    */
	      out.writeObject(arg);
	  }
      }
    out.endGroup(tag);
  }

}
