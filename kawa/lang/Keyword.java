package kawa.lang;
import codegen.*;

import java.io.PrintStream;

public class Keyword extends Object implements Printable, Compilable
{
  // Does not include final ':'.
  private String name;

  // Note:  No public constructor!
  private Keyword (String n)
  {
    name = new String(n);
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

  public FString toSchemeString()
  {
    return new FString(name);
  }

  public static boolean isKeyword (Object obj)
  {
    return obj instanceof Keyword;
  }

  public final String toString()
  {
    return name+':';
  }

  public void print(java.io.PrintStream ps)
  {
    Symbol.print(name, ps);
    ps.print(':');
  }

  /** Search vals[0:offset-1] for keyword;  return following value. */
  public static Object searchForKeyword (Object[] vals,
					 int offset, Object keyword)
  {
    for (int i = offset;  i < vals.length;  i += 2)
      {
	if (vals[i] == keyword)
	  return vals[i+1];
      }
    return null;
  }

  static Method makeKeywordMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (makeKeywordMethod == null)
      {
	Type[] applyargs = new Type[1];
	applyargs[0] = comp.javaStringType;
	makeKeywordMethod =
	  comp.scmKeywordType.new_method ("make", applyargs,
					  comp.scmKeywordType,
					  Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, comp.scmKeywordType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    comp.method.compile_push_string (((Keyword)literal.value).name);
    comp.method.compile_invoke_static (makeKeywordMethod);
  }
}
