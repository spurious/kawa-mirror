// Copyright (c) 2007  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.kawa.io;
import java.io.*;
import java.net.*;
import gnu.lists.FString;
import gnu.mapping.*;

/** A Path that wraps a URI.
 * The URI can be a java.net.URI, or a String, if compiled without URI support.
 */

public class URIPath
  extends Path
  implements Comparable<URIPath>
{
  final URI uri;
  URIPath (URI uri) { this.uri = uri; } 

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
    if (path instanceof CharSequence ||
        path instanceof File || path instanceof Path)
      str = path.toString();
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
        return new URIStringPath(new URI(encodeForUri(uri, 'I')), uri);
      }
    catch (Exception ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
    /* #else */
    // return new URIPath(uri);
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

  public boolean exists ()
  {
    try
      {
        URLConnection conn = toURL().openConnection();
        if (conn instanceof HttpURLConnection)
          return (((HttpURLConnection) conn).getResponseCode()
                  == HttpURLConnection.HTTP_OK);
        else
          return conn.getLastModified() != 0;
      }
    catch (Exception ex)
      {
        return false;
      }
  }

  public long getLastModified ()
  {
    return URLPath.getLastModified(toURL());
  }

  public long getContentLength ()
  {
    return URLPath.getContentLength(toURL());
  }

  /* #ifdef use:java.net.URI */
  public URI toUri () { return uri; }
  public String toURIString () { return uri.toString(); }
  /* #else */
  // public String toUri () { return uri; }
  // public String toURIString () { return uri; }
  /* #endif */

    public Path resolve(String rstr) {
        char fileSep = File.separatorChar;
        if (fileSep != '/') {
            // Check for Windows absolute filename.
            if (rstr.length() >= 2
                && ((rstr.charAt(1) == ':' && Character.isLetter(rstr.charAt(0)))
                    || (rstr.charAt(0) == fileSep && rstr.charAt(1) == fileSep))) {
                return FilePath.valueOf(new File(rstr));
            }
            rstr = rstr.replace(fileSep, '/');
        }
        return URIPath.valueOf(resolveToUri(rstr));
    }

    public URI resolveToUri(String rstr) {
        try {
            URI r = new URI(rstr);
            if (uri.isOpaque())
                return r;
            String bScheme = getScheme();
            String bAuthority = getAuthority();
            String bPath = getPath();
            String bQuery = getQuery();
            String bFragment = getFragment();
            String rScheme = r.getScheme();
            String rAuthority = r.getAuthority();
            String rPath = r.getPath();
            String rQuery = r.getQuery();
            String rFragment = r.getFragment();
            String tScheme = rScheme != null ? rScheme : bScheme;
            String tAuthority;
            String tPath;
            String tQuery;
            boolean removeDotsNeeded = true;
            if (rScheme != null || rAuthority != null) {
                tAuthority = rAuthority;
                tPath = rPath;
                tQuery = rQuery;
            } else {
                tAuthority = bAuthority;
                if (rPath == null || rPath.length() == 0) {
                    tPath = bPath;
                    removeDotsNeeded = false;
                    tQuery = rQuery != null ? rQuery : bQuery;
                } else {
                    if (rPath.charAt(0) == '/') {
                        tPath = rPath;
                    } else {
                        // tPath = merge(bPath, rPath);
                        if (bAuthority != null && bPath.length() == 0)
                            tPath = "/" + rPath;
                        else {
                            int bSlash = bPath.lastIndexOf('/');
                            if (bSlash >= 0)
                                tPath = bPath.substring(0, bSlash+1) + rPath;
                            else
                                tPath = rPath;
                        }
                    }
                    tQuery = rQuery;
                }
            }
            int len = tPath.length();
            if (removeDotsNeeded && len > 0) {
                StringBuilder pbuf = new StringBuilder();
                int ch0 = tPath.charAt(0);
                int i = 1;
                while (ch0 >= 0) {
                    // The "input buffer" in the rfc 3986 algorithm is
                    // represented by ch0 followed by the tail of tPath:
                    // It is empty if ch0 < 0; otherwise it is ch0
                    // followed by tPath.subString(i).
                    // A: "../"
                    if (ch0 == '.' && i + 1 < len && tPath.charAt(i) == '.' && tPath.charAt(i+1) == '/') {
                        if (tScheme != null) {
                            // Skip - for compatibility with RFC-3986.
                            ch0 = i + 2 < len ? tPath.charAt(i+2) : -1;
                            i += 3;
                        } else {
                            pbuf.append("..");
                            ch0 = '/';
                            i += 2;
                        }
                    }
                    // A: "./"
                    else if (ch0 == '.' && i < len && tPath.charAt(i) == '/') {
                        ch0 = i + 1 < len ? tPath.charAt(i+1) : -1;
                        i += 2;
                    }
                    // B: "/./" or "/." END
                    else if (ch0 == '/' && i < len
                             && tPath.charAt(i) == '.' 
                             && (i + 1 == len
                                 || tPath.charAt(i+1) == '/')) {
                        ch0 = '/';
                        i = i + 1 == len ? len : i + 2;
                    }
                    // C: "/../" OR "/.." END
                    else if (ch0 == '/' && i + 1 < len
                             && tPath.charAt(i) == '.'
                             && tPath.charAt(i+1) == '.'
                             && (i + 2 == len || tPath.charAt(i+2) == '/')) {
                        ch0 = '/';
                        i = i + 2 == len ? len : i + 3;
                        int plen = pbuf.length();
                        while (plen > 0 && pbuf.charAt(--plen) != '/')
                            ;
                        pbuf.setLength(plen);
                    }
                    // D "." END or ".." END
                    else if (ch0 == '.'
                             && (i == len || (i + 1 == len && tPath.charAt(i) == '.'))) {
                        ch0 = -1;
                    } else {
                        for (;;) {
                            pbuf.append((char) ch0);
                            if (i == len) {
                                ch0 = -1;
                                break;
                            }
                            ch0 = tPath.charAt(i++);
                            if (ch0 == '/')
                                break;
                        }
                    }
                    
                }
                tPath = pbuf.toString();
            }
            return new URI(tScheme, tAuthority, tPath, tQuery, rFragment);
        } catch (Exception ex) {
            throw WrappedException.wrapIfNeeded(ex);
        }
  }

  public int compareTo (URIPath path)
  {
    return uri.compareTo(path.uri);
  }

  /* #ifndef JAVA5 */
  // public int compareTo (Object obj)
  // {
  //   return compareTo((URIPath) obj);
  // }
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
    return toURIString();
  }

  public URL toURL ()
  {
    return Path.toURL(uri.toString());
  }

    public File toFile() {
        return FilePath.valueOf(toURIString ()).toFile();
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
    return uri.getScheme();
  }

  public String getHost ()
  {
    return uri.getHost();
  }

  public String getAuthority ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getAuthority();
    /* #else */
    // return Path.uriSchemeSpecified(uri) ? toURL().getAuthority() : null;
    /* #endif */
  }

  public String getUserInfo ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getUserInfo();
    /* #else */
    // return Path.uriSchemeSpecified(uri) ? toURL().getUserInfo() : null;
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

  public String getPath ()
  {
    /* #ifdef use:java.net.URI */
    return uri.getPath();
    /* #else */
    // return toURL().getFile();
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
    // int hash = uri.lastIndexOf('#');
    // return hash < 0 ? null : uri.substring(hash+1);
    /* #endif */
  }

  public Path getCanonical ()
  {
    if (isAbsolute())
      {
        /* #ifdef use:java.net.URI */
        URI norm = uri.normalize();
        if (norm == uri)
          return this;
        return valueOf(norm);
        /* #else */
        // return this; // FIXME!
        /* #endif */
      }
    else
      return getAbsolute().getCanonical();
  }

  public static String encodeForUri (String str, char mode)
  {
    StringBuffer sbuf = new StringBuffer();
    int len = str.length();
    for (int i = 0; i <len;  )
      {
        int ch = str.charAt(i++);
        // Check for surrogate.
        if (ch >= 0xD800 && ch < 0xDC00 && i < len)
          ch = (ch - 0xD800) * 0x400
            + (str.charAt(i++) - 0xDC00) + 0x10000;
        if (mode == 'H' ? ch >= 32 && ch <= 126
            : ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
               || (ch >= '0' && ch <= '9')
               || ch == '-' || ch == '_' || ch == '.' || ch == '~'
               || (mode == 'I'
                   && (ch == ';' || ch == '/' || ch == '?' || ch == ':'
                       || ch == '*' || ch == '\'' || ch == '(' || ch == ')'
                       || ch == '@' || ch == '&' || ch == '=' || ch == '+'
                       || ch == '$' || ch == ',' || ch == '[' || ch == ']'
                       || ch == '#' || ch == '!' || ch == '%'))))
          sbuf.append((char) ch);
        else
          {
            int pos = sbuf.length();
            int nbytes = 0;
            int needed = ch < (1 << 7) ? 1
              : ch < (1 << 11) ? 2
              : ch < (1 << 16) ? 3
              : 4;
            do
              {
                // We insert encodings for the bytes in right-to-left order.
                int availbits = nbytes == 0 ? 7 : 6 - nbytes;
                int b;
                if (ch < (1 << availbits))
                  {
                    // The rest fits: handling first bytes.
                    b = ch;
                    if (nbytes > 0)
                      b |= (0xff80 >> nbytes) & 0xff;
                    ch = 0;
                  }
                else
                  {
                    b = 0x80 | (ch & 0x3f);
                    ch >>= 6;
                  }
                nbytes++;
                for (int j = 0; j <= 1; j++)
                  {
                    int hex = b & 15;
                    sbuf.insert(pos,
                                (char) (hex <= 9 ? hex + '0' : hex - 10 + 'A'));
                    b >>= 4;
                  }
                sbuf.insert(pos, '%');
              }
            while (ch != 0);
          }
      }
    return sbuf.toString();
  }
}

/* #ifdef use:java.net.URI */
/** A URIPath that also remembers the "orginal" (unencoded) String. */
class URIStringPath extends URIPath
{
  String uriString;
  public String toURIString () { return uriString; }

  public URIStringPath (URI uri, String uriString)
  {
    super(uri);
    this.uriString = uriString;
  }
}
/* #endif */
