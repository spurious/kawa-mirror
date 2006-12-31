// Copyright (c) 2007  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.text;
import java.io.*;
import java.net.*;
import gnu.lists.FString;
import gnu.mapping.*;

/** A Path that wraps a URI.
 * The URI can be a java.net.URI, or a String, if compiled without URI support.
 */

public class URIPath
  extends Path
  /* #ifdef JAVA2 */
  /* #ifdef JAVA5 */
  // implements Comparable<URIPath>
  /* #else */
  implements Comparable
  /* #endif */
  /* #endif */
{
  /* #ifdef use:java.net.URI */
  URI uri;
  URIPath (URI uri) { this.uri = uri; } 
  /* #else */
  // String uri;
  // private URIPath (String uri) { this.uri = uri; } 
  /* #endif */

  public static URIPath coerceToURIPathOrNull (Object path)
  {
    if (path instanceof URIPath)
      return (URIPath) path;
    if (path instanceof URL)
      return URLPath.valueOf((URL) path);
    /* #ifdef use:java.net.URI */
    if (path instanceof URI)
      return URIPath.valueOf((URI) path);
    /* #endif */
    String str;
    if (path instanceof File || path instanceof Path || path instanceof FString)
      str = path.toString();
    else if (path instanceof String)
      str = (String) path;
    else
      return null;
    return URIPath.valueOf(str);
  }

  public static URIPath makeURI (Object arg)
  {
    URIPath path = coerceToURIPathOrNull(arg);
    if (path == null)
      throw new WrongType((String) null, WrongType.ARG_CAST, arg, "URI");
    return path;
  }

  /* #ifdef use:java.net.URI */
  public static URIPath valueOf (URI uri)
  { return new URIPath(uri); }
  /* #endif */

  public static URIPath valueOf (String uri)
  {
    /* #ifdef use:java.net.URI */
    try
      {
        return new URIPath(new URI(uri));
      }
    catch (Throwable ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
   /* #else */
    // return new URIPath(uri)
    /* #endif */
  }

  public static URIPath valueOf (File file)
  {
    /* #ifdef use:java.net.URI */
    return valueOf(FilePath.toURI(file));
    /* #else */
    // return valueOf(FilePath.toURIString(file));
    /* #endif */
  }

  public boolean isAbsolute ()
  {
    /* #ifdef use:java.net.URI */
    return uri.isAbsolute();
    /* #else */
    // return Path.uriSchemeSpecified(uri);
    /* #endif */
  }

  public long getLastModified ()
  {
    return URLPath.getLastModified(toURL());
  }

  /* #ifdef use:java.net.URI */
  public URI toURI () { return uri; }
  public String toURIString () { return uri.toString(); }
  /* #else */
  // public String toURI () { return uri; }
  // public String toURIString () { return uri; }
  /* #endif */

  public Path resolve (String rstr)
  {
    if (Path.uriSchemeSpecified(rstr))
      return URIPath.valueOf(rstr);
    char fileSep = File.separatorChar;
    if (fileSep != '/')
      {
        // Check for Windows absolute filename.
        if (rstr.length() >= 2
            && ((rstr.charAt(1) == ':' && Character.isLetter(rstr.charAt(0)))
                || (rstr.charAt(0) == fileSep && rstr.charAt(1) == fileSep)))
          {
            return FilePath.valueOf(new File(rstr));
          }
        rstr = rstr.replace(fileSep, '/');
      }
    /* #ifdef use:java.net.URI */
    URI resolved;
    try
      {
        resolved = uri.resolve(new URI(null, rstr, null));
      }
    catch (Throwable ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }

    /* #else */
    // /* The following is an appximation of URI's rsolve method. */
    // /* For example, it doesn't simplify "../" to "". */
    // String sbase = toURI(base);
    // int lastSl = sbase.lastIndexOf('/');
    // StringBuffer sbuf = new StringBuffer(sbase);
    // if (lastSl >= 0)
    //   sbuf.setLength(lastSl+1);
    // else
    //   sbuf.append('/');
    // if (rstr.length() > 0 && rstr.charAt(0) == '/')
    //   { 
    //     /* Rstr is an absolute file name, but doesn't have a uri scheme. */
    //     int baseLen = sbase.length();
    //     int pathStart = Path.uriSchemeLength(sbase);
    //     if (pathStart <= 1)
    //       return rstr;
    //     pathStart++;
    //     /* Add the "authority" - usually a host-name. */
    //     if (pathStart + 1 < baseLen
    //         && sbase.charAt(pathStart) == '/'
    //         && sbase.charAt(pathStart+1) == '/')
    //       {
    //         int p2 = sbase.indexOf('/', pathStart+2);
    //         if (p2 < 0)
    //           p2 = baseLen; // ? append '/'? FIXME
    //         pathStart = p2;
    //       }
    //     sbuf.setLength(pathStart);
    //   }
    // sbuf.append(rstr);
    // String resolved = sbuf.toString();
    /* #endif */
    return URIPath.valueOf(resolved);
  }

  public int compareTo (URIPath path)
  {
    return uri.compareTo(path.uri);
  }

  /* #ifndef JAVA5 */
  public int compareTo (Object obj)
  {
    return compareTo((URIPath) obj);
  }
  /* #endif */

  public boolean equals (Object obj)
  {
    return obj instanceof URIPath && uri.equals(((URIPath) obj).uri);
  }

  public int hashCode ()
  {
    return uri.hashCode();
  }

  public String toString ()
  {
  /* #ifdef use:java.net.URI */
    return uri.toString();
    /* #else */
    // return uri;
    /* #endif */
  }

  public URL toURL ()
  {
    return Path.toURL(uri.toString());
  }

  public InputStream openInputStream () throws IOException
  {
    // If relative and base is a File, should be a File? FIXME
    return URLPath.openInputStream(toURL());
  }

  public OutputStream openOutputStream () throws IOException
  {
    return URLPath.openOutputStream(toURL());
  }

  public String getScheme ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getScheme();
    /* #else */
    // return Path.uriSchemeSpecified(uri) ? toURL().getProtocol() : null;
    /* #endif */
  }

  public String getHost ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getHost();
    /* #else */
    // return Path.uriSchemeSpecified(uri) ? toURL().getHost() : null;
    /* #endif */
  }

  public int getPort ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getPort();
    /* #else */
    // return Path.uriSchemeSpecified(uri) ? toURL().getPort() : -1;
    /* #endif */
  }

  public String getQuery ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getQuery();
    /* #else */
    // return toURL().getQuery();
    /* #endif */
  }

  public String getFragment ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getFragment();
    /* #else */
    // return toURL().getFragment();
    /* #endif */
  }
}
