package gnu.expr;
import gnu.mapping.*;
import java.io.*;

public class Keyword extends Symbol
  implements Printable, Externalizable
{
  public static final Namespace keywordNamespace = new Namespace();
  static { keywordNamespace.setName("(keywords)"); }

  public Keyword()
  {
  }

  private Keyword (String name)
  {
    super(name);
  }

  /** Get the corresponding non-keyword symbol.
   * Informally, the symbol corresponding to dropping the ':'.
   */
  public Symbol asSymbol ()
  {
    return Namespace.EmptyNamespace.getSymbol(name);
  }

  /**
   * Create or find a Keyword with a given name (without final ':').
   * @param name the print-name of the desired Keyword
   * @return a Keyword with the given name, newly created iff none such exist
   */
  static public Keyword make (String name)
  {
    int hash = name.hashCode();
    Keyword keyword = (Keyword) keywordNamespace.lookup(name, hash, false);
    if (keyword == null)
      {
	keyword = new Keyword(name);
	keywordNamespace.add(keyword, hash);
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
    Symbols.print(name, ps);
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
    int hash = name.hashCode();
    Keyword keyword = (Keyword) keywordNamespace.lookup(name, hash, false);
    if (keyword != null)
      return keyword;
    keywordNamespace.add(this, hash);
    return this;
  }
}
