package gnu.text;
/* #ifdef use:java.net.URI */
import java.net.URI;
/* #endif */
/* #ifdef JAVA2 */
import java.util.WeakHashMap;
/* #endif */
import java.net.URL;
import java.net.URLConnection;
import java.io.*;
import gnu.lists.FString;
import gnu.mapping.CallContext;
import gnu.mapping.InPort;

public class URI_utils
{
  public static boolean isAbsolute (Object uri)
  {
    if (uri instanceof URL)
      return true;
    if (uri instanceof File)
      return ((File) uri).isAbsolute();
    /* #ifdef use:java.net.URI */
    if (uri instanceof URI)
      return ((URI) uri).isAbsolute();
    /* #endif */
    return InPort.uriSchemeSpecified(uri.toString());
  }

  /** Handle name that starts with "class-resource:/". */
  static URL resourceURL (Object uri, String str)
    throws java.io.IOException
  {
    // Either: class-resource:/PACKAGE/CLASS
    // or: class-resource:/CLASS
    // or: class-resource:/PACKAGE/RESOURCE
    // or: class-resource:/RESOURCE
    ClassLoader loader = getClassLoaderForURI(uri);
    if (loader == null)
      throw new IOException("unknown class-loader for URI '"+str+'\'');
    URL url
      = loader.getResource(str.substring(CLASS_RESOURCE_URI_PREFIX_LENGTH));
    if (url == null)
      throw new FileNotFoundException(str);
    return url;
  }

  public static URL FiletoURL (File file)
  {
    try
      {
        /* #ifdef JAVA2 */
        return file.toURL();
        /* #else */
        // char fileSep = File.separatorChar;
        // return new URL("file:" + file.getAbsolutePath().replace(fileSep, '/'));
        /* #endif */
      }
    catch (Throwable ex)
      {
        throw gnu.mapping.WrappedException.wrapIfNeeded(ex);
      }
  }

  public static URL toURL (Object uri)
    throws java.io.IOException
  {
    uri = toFileOrURL(uri);
    if (uri instanceof File)
      return FiletoURL((File) uri);
    return (URL) uri;
  }

  public static Object toFileOrURL (Object uri)
    throws java.io.IOException
  {
    if (uri instanceof URL || uri instanceof File)
      return uri;
    String str = uri.toString();
    if (str.startsWith(CLASS_RESOURCE_URI_PREFIX))
      return resourceURL(uri, str);
    if (! InPort.uriSchemeSpecified(str))
      {
        CallContext ctx = CallContext.getInstance();
        String base = ctx.getBaseUriRaw();
        char fileSep = File.separatorChar;
        if (base == null)
          {
            if (fileSep != '/')
              str = str.replace('/', fileSep);
            return new File(str);
          }
        else if (! InPort.uriSchemeSpecified(base))
          {
            if (fileSep != '/')
              {
                str = str.replace('/', fileSep);
                base = base.replace('/', fileSep);
              }
            File dir = new File(base);
            if (! dir.isDirectory())
              {
                /* #ifdef JAVA2 */
                dir = dir.getParentFile();
                /* #else */
                // dir = new File(dir.getParent());
                /* #endif */
              }
            return new File(dir, str);
          }
        try
          {
            str = resolve(str, base).toString();
          }
        catch (java.net.URISyntaxException ex)
          {
            throw new IOException("invalid URI syntax: '"+str+'\'');
          }
      }
    // Note JDK (upto 1.5.0, at least) throws an UnknownServiceException
    // "protocol doesn't support output" if you do getOutputStream on
    // a "file:" URL.  That seems lame, but let's avoid that!
    if (str.startsWith("file:"))
      {
        /* #ifdef use:java.net.URI */
        try { return new File(new URI(str)); }
        catch (Throwable ex) { }
        /* #else */
        // if (File.separatorChar == '/') // Otherwise it's more complicated.
        //   return new File(str.substring(5));
        /* #endif */
      }

    return new URL(str);
  }

  public static InputStream getInputStream (Object uri)
    throws java.io.IOException
  {
    uri = toFileOrURL(uri);
    if (uri instanceof File)
      return new FileInputStream((File) uri);
    return ((URL) uri).openConnection().getInputStream();
  }

  public static OutputStream getOutputStream (Object uri)
    throws java.io.IOException
  {
    uri = toFileOrURL(uri);
    if (uri instanceof File)
      return new FileOutputStream((File) uri);
    URLConnection conn = ((URL) uri).openConnection();
    conn.setDoInput(false);
    conn.setDoOutput(true);
    return conn.getOutputStream();
  }

  /* #ifdef use:java.net.URI */
  public static URI toURI (Object uri)
    throws java.net.URISyntaxException
  {
    if (uri instanceof String || uri instanceof FString)
      return nametoURI(uri.toString());
    if (uri instanceof File)
      return FiletoURI((File) uri);
    if (uri instanceof URL)
      uri = URLtoURI((URL) uri);
    return (URI) uri;
  }
  /* #else */
  // public static String toURI (Object uri)
  // {
  //   if (uri instanceof String || uri instanceof FString)
  //     return nametoURIString(uri.toString());
  //   if (uri instanceof File)
  //     return FiletoURI((File) uri).toString();
  //   return URLtoURI((URL) uri);
  // }
  /* #endif */

  /* #ifdef use:java.net.URI */
  static URI URLtoURI (URL url)
    throws java.net.URISyntaxException
  {
    /* #ifdef JAVA5 */
    // return url.toURI();
    /* #else */
    return new URI(url.toString());
    /* #endif */
  }
  /* #else */
  // static String URLtoURI (URL url)
  // {
  //   return url.toString();
  // }
  /* #endif */

  public static String toURIString (Object uri)
  /* #ifdef use:java.net.URI */
    throws java.net.URISyntaxException
  /* #endif */
  {
    if (uri instanceof String || uri instanceof FString)
      return nametoURIString(uri.toString());
    if (uri instanceof File)
      return FiletoURI((File) uri).toString();
    /* #ifdef use:java.net.URI */
    if (uri instanceof URL)
      uri = URLtoURI((URL) uri);
    return ((URI) uri).toString();
    /* #else */
    // return ((URL) uri).toString();
    /* #endif */
  }

  public static String nametoURIString (String str)
  /* #ifdef use:java.net.URI */
    throws java.net.URISyntaxException
  /* #endif */
  {
    if (InPort.uriSchemeSpecified(str))
      return str;
    char fileSep = File.separatorChar;
    if (fileSep != '/')
      str = str.replace('/', fileSep);
    return FiletoURI(new File(str)).toString();
  }

  /* #ifdef use:java.net.URI */
  public static URI nametoURI (String str)
    throws java.net.URISyntaxException
  {
    try
      {
        return new URI(str);
      }
    catch (java.net.URISyntaxException ex)
      {
        if (InPort.uriSchemeSpecified(str))
          throw ex;
        char fileSep = File.separatorChar;
        if (fileSep != '/')
          str = str.replace('/', fileSep);
        return FiletoURI(new File(str));
      }
  }

  public static URI FiletoURI (File file)
    throws java.net.URISyntaxException
  {
    if (file.isAbsolute())
      return file.toURI();
    /* We don't want to just use File.toURI(),
       because that turns a relative File into an absolute URI. */
    String fname = file.toString();
    char fileSep = File.separatorChar;
    if (fileSep != '/')
      fname = fname.replace(fileSep, '/');
    return new URI(null, null, fname, null);
  }
  /* #else */
  // public static String FiletoURI (File file)
  // {
  //   if (file.isAbsolute())
  //     return FiletoURL(file).toString();
  //   else
  //     return file.toString().replace(File.separatorChar, '/');
  // }
  /* #endif */

  /* #ifdef JAVA2 */
  static WeakHashMap resourceMap;
  /* #endif */

  synchronized static ClassLoader getClassLoaderForURI (Object uri)
  {
    /* #ifdef JAVA2 */
    if (resourceMap == null) return null;
    return (ClassLoader) resourceMap.get(uri);
    /* #else */
    // return null;
    /* #endif */
  }

  synchronized static void setClassLoaderForURI (Object uri, ClassLoader loader)
  {
    /* #ifdef JAVA2 */
    if (resourceMap == null)
      resourceMap = new WeakHashMap();
    resourceMap.put(uri, loader);
    /* #endif */
  }

  /** A special URI-scheme for accessing resources relative to a ClassLoader.
   * The resource is found using ClassLoader's getResource method.
   * The actual ClassLoader is found using getClassLoaderForURI. */
  public static final String CLASS_RESOURCE_URI_PREFIX = "class-resource:/";

  /** The length of CLASS_RESOURCE_URI_PREFIX, including ":/". */
  public static final int CLASS_RESOURCE_URI_PREFIX_LENGTH = 16;

  public static
  /* #ifdef use:java.net.URI */
  URI
  /* #else */
  // String
  /* #endif */
  makeClassResourceURI (Class clas)
  {
    String cname = clas.getName();
    int dot = cname.lastIndexOf('.');
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(CLASS_RESOURCE_URI_PREFIX);
    if (dot >= 0)
      {
        sbuf.append(cname.substring(0, dot));
        sbuf.append('/');
        cname = cname.substring(dot+1);
      }
    sbuf.append(cname);
    String str = sbuf.toString();
    /* #ifdef use:java.net.URI */
    URI uri;
    try
      {
        uri = new URI(str);
      }
    catch (Throwable ex)
      {
        throw gnu.mapping.WrappedException.wrapIfNeeded(ex);
      }
    /* #else */
    // String uri = str;
    /* #endif */
    setClassLoaderForURI(uri, clas.getClassLoader());
    return uri;
  }

  public static
  /* #ifdef use:java.net.URI */
  URI
  /* #else */
  // String
  /* #endif */
  resolve (Object relative, Object base)
    throws java.net.URISyntaxException
  {
    String rstr;
    if (relative instanceof URL)
      { // URLs are always absolute.
        return URLtoURI((URL) relative);
      }
    /* #ifdef use:java.net.URI */
    else if (relative instanceof URI)
      {
        URI rel = (URI) relative;
        if (rel.isAbsolute())
          return rel;
        rstr = rel.toString();
      }
    /* #endif */
    else if (relative instanceof String || relative instanceof FString)
      {
        rstr = relative.toString();
        if (InPort.uriSchemeSpecified(rstr))
          return toURI(rstr);
      }
    else if (relative instanceof File)
      {
        File file = (File) relative;
        if (file.isAbsolute())
          {
            /* #ifdef use:java.net.URI */
            return file.toURI();
            /* #endif */
          }
        rstr = file.toString();
      }
    else
      throw new gnu.mapping.WrongType("resolve-uri", 1, "uri/file/string");
    char fileSep = File.separatorChar;
    if (fileSep != '/')
      {
        // Check for Windows absolute filename.
        if (rstr.length() >= 2
            && ((rstr.charAt(1) == ':' && Character.isLetter(rstr.charAt(0)))
                || (rstr.charAt(0) == fileSep && rstr.charAt(1) == fileSep)))
          {
            return toURI(new File(rstr));
          }
        rstr = rstr.replace(fileSep, '/');
      }
    /* #ifdef use:java.net.URI */
    URI resolved =  toURI(base).resolve(rstr);
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
    //     int pathStart = InPort.uriSchemeLength(sbase);
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
     if (resolved.toString().startsWith(CLASS_RESOURCE_URI_PREFIX))
      setClassLoaderForURI(resolved, getClassLoaderForURI(base));
    return resolved;
  }
}
