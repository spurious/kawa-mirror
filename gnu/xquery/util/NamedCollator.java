package gnu.xquery.util;
import java.io.*;

public class NamedCollator
/* BEGIN JAVA2 */
extends java.text.Collator
/* END JAVA2 */
implements Externalizable
{
  /* BEGIN JAVA2 */
  java.text.Collator collator;
  /* END JAVA2 */

  String name;

  public static final String UNICODE_CODEPOINT_COLLATION
    = "http://www.w3.org/2004/10/xpath-functions/collation/codepoint";

  public static NamedCollator make (String name)
  {
    NamedCollator coll = new NamedCollator();
    coll.name = name;
    coll.resolve();
    return coll;
  }

  public static final NamedCollator codepointCollation = new NamedCollator();

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
    /* BEGIN JAVA2 */
    if (collator != null)
      return collator.compare(str1, str2);
    /* END JAVA2 */
    return str1.compareTo(str2);
  }

  /* BEGIN JAVA2 */
  public java.text.CollationKey getCollationKey (String source)
  {
    return collator.getCollationKey(source);
  }
  /* END JAVA2 */

  public int hashCode ()
  {
    /* BEGIN JAVA2 */
    if (collator != null)
      return collator.hashCode();
    /* END JAVA2 */
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
