// Copyright (c) 2007  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.text;
import java.io.*;
import java.net.*;
import gnu.mapping.*;

/** A generalized path/location, including File and URIs. */

public abstract class Path
// Possibly FUTURE: #ifdef JAVA6: implements javax.tools.FileObject
{
  public static final FilePath userDirPath =
    FilePath.valueOf(new File(""));
  //FilePath.valueOf(new File(System.getProperty("user.dir")));

  public static Path defaultPath = userDirPath;

  /* #ifdef JAVA2 */
  private static ThreadLocal pathLocation = new ThreadLocal();
  /* #endif */

  protected Path ()
  {
  }

  public static Path currentPath ()
  {
    /* #ifdef JAVA2 */
    Object path = pathLocation.get();
    if (path != null)
      return (Path) path;
    /* #endif */
    return defaultPath;
  }

  public static Path coerceToPathOrNull (Object path)
  {
    if (path instanceof Path)
      return (Path) path;
    if (path instanceof URL)
      return URLPath.valueOf((URL) path);
    /* #ifdef use:java.net.URI */
    if (path instanceof URI)
      return URIPath.valueOf((URI) path);
    /* #endif */
    if (path instanceof File)
      return FilePath.valueOf((File) path);
    String str;
    if (path instanceof gnu.lists.FString) // FIXME: || UntypedAtomic
      str = path.toString();
    else if (! (path instanceof String))
      return null;
    else
      str = (String) path;
    if (Path.uriSchemeSpecified(str))
      return URIPath.valueOf(str);
    else
      return FilePath.valueOf(str);
  }

  public static Path valueOf (Object arg)
  {
    Path path = coerceToPathOrNull(arg);
    if (path == null)
      throw new WrongType((String) null, WrongType.ARG_CAST, arg, "path");
    return path;
  }

  public static URL toURL (String str)
  {
    try
      {
        if (! Path.uriSchemeSpecified(str))
          {
            Path cur = currentPath();
            Path path = cur.resolve(str);
            if (path.isAbsolute())
              return path.toURL();
            str = path.toString();
          }
        return new URL(str);
      }
    catch (Throwable ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
 }

  /** Helper routine to get the scheme part of a URI.
   * The scheme part is "http:" or "file:" or "ftp:" most commonly.
   * This functions searches for the first ':' that doesn't follow a '/'.
   * @return The length of the scheme component, not counting the colon,
   * (or alternatively the index of the colon), or -1 if the is no scheme.
   */
  public static int uriSchemeLength (String uri)
  {
    int len = uri.length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = uri.charAt(i);
	if (ch == ':')
	  return i;
        if (i == 0 ? ! Character.isLetter(ch)
            : (! Character.isLetterOrDigit(ch)
               && ch != '+' && ch != '-' && ch != '.'))
	  return -1;
      }
    return -1;
  }

  /** Tests if a URL has a scheme.
   * For convenience, we treat a 1-character "scheme" as an
   * MS-DOS-style "drive letter" - i.e. not a scheme. */
  public static boolean uriSchemeSpecified (String name)
  {
    int ulen = uriSchemeLength(name);
    if (ulen == 1 && File.separatorChar == '\\')
      {
        char drive = name.charAt(0);
        return ! ((drive >= 'a' && drive <= 'z')
                  || (drive >= 'A' && drive <= 'Z'));
      }
    return ulen > 0;
  }

  public abstract boolean isAbsolute ();

  public boolean exists ()
  {
    return getLastModified() != 0;
  }

  public abstract long getLastModified ();
 
  public Path resolve (Path relative)
  {
    if (relative.isAbsolute())
      return relative;
    return resolve(relative.toString());
  }

  public String getHost()
  {
    return null;
  }

  public int getPort ()
  {
    return -1;
  }

  public String getQuery ()
  {
    return null;
  }

  public String getFragment ()
  {
    return null;
  }

  public abstract URL toURL ();

  /* #ifdef use:java.net.URI */
  public abstract URI toURI ();
  /* #else */
  // public String toURI () { return toURIString(); }
  /* #endif */
  public String toURIString () { return toURI().toString(); }

  // FIXME merge in URI_urils.toFileOrURL(Object) functionality.  FIXME.
  public abstract Path resolve (String relative);

  public static InputStream openInputStream (Object uri) throws IOException
  {
    return Path.valueOf(uri).openInputStream();
  }

  public abstract InputStream openInputStream () throws IOException;
  public abstract OutputStream openOutputStream () throws IOException;

  /** Convert an absolute URI to one relatve to a given base.
   * This goes beyond java.net.URI.relativize in that if the arguments
   * have a common prefix, it can create a relative URI using "../" steps.
   */
  public static String relativize (String in, String base)
    throws java.net.URISyntaxException, java.io.IOException
  {
    String baseStr = base;
    String inStr = in;
    /* #ifdef use:java.net.URI */
    baseStr = new URI(baseStr).normalize().toString();
    inStr = URLPath.valueOf(in).toURI().normalize().toString();
    /* #endif */
    int baseLen = baseStr.length();
    int inLen = inStr.length();
    int i = 0;
    int sl = 0;
    int colon = 0;
    for (; i < baseLen && i < inLen;  i++)
      {
        char cb = baseStr.charAt(i);
        char ci = inStr.charAt(i);
        if (cb != ci)
          break;
        if (cb == '/')
          sl = i;
        if (cb == ':')
          colon = i;
      }
    if (colon > 0
        && (sl > colon + 2 || baseLen <= colon+2 || baseStr.charAt(colon+2) != '/')
        /*
        && (colon + 2 != CLASS_RESOURCE_URI_PREFIX_LENGTH
            || ! inStr.substring(0, colon + 2).equals(CLASS_RESOURCE_URI_PREFIX)
            || getClassLoaderForURI(base) == getClassLoaderForURI(in))
        */
        )
      {
        baseStr = baseStr.substring(sl+1);
        inStr = inStr.substring(sl+1);
      }
    else
      return in;
    /* #ifdef JAVA5 */
    // StringBuilder sbuf = new StringBuilder();
    /* #else */
    StringBuffer sbuf = new StringBuffer();
    /* #endif */
    sl = 0;
    for (i = baseLen = baseStr.length(); --i >= 0; )
      if (baseStr.charAt(i) == '/') // sep?
        sbuf.append("../");
    sbuf.append(inStr);
    return sbuf.toString();
  }
}
