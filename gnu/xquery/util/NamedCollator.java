package gnu.xquery.util;
import java.io.*;

public class NamedCollator
/* #ifdef JAVA2 */
extends java.text.Collator
/* #endif */
implements Externalizable
{
  /* #ifdef JAVA2 */
  java.text.Collator collator;
  /* #endif */

  String name;

  public static final String UNICODE_CODEPOINT_COLLATION
  = "http://www.w3.org/2005/xpath-functions/collation/codepoint";

  public static NamedCollator make (String name)
  {
    NamedCollator coll = new NamedCollator();
    coll.name = name;
    coll.resolve();
    return coll;
  }

  public String getName ()
  {
    return name;
  }

  public static NamedCollator find (String name)
  {
    return make(name);
  }

  public static final NamedCollator codepointCollation = new NamedCollator();
  static { codepointCollation.name = UNICODE_CODEPOINT_COLLATION; }

  public void resolve ()
  {
    if (name != null && ! name.equals(UNICODE_CODEPOINT_COLLATION))
      {
	// FIXME!
	throw new RuntimeException("unknown collation: "+name);
      }
  }

  public int compare (String str1, String str2)
  {
    /* #ifdef JAVA2 */
    if (collator != null)
      return collator.compare(str1, str2);
    /* #endif */
    return str1.compareTo(str2);
  }

  /* #ifdef JAVA2 */
  public java.text.CollationKey getCollationKey (String source)
  {
    return collator.getCollationKey(source);
  }
  /* #endif */

  public int hashCode ()
  {
    /* #ifdef JAVA2 */
    if (collator != null)
      return collator.hashCode();
    /* #endif */
    return 0;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeUTF(name);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    name = in.readUTF();
    resolve();
  }
}
