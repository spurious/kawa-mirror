package gnu.text;
/* #ifdef use:java.net.URI */
import java.net.URI;
import java.util.WeakHashMap;
/* #endif */
import java.net.URL;
import java.io.File;
import gnu.lists.FString;
import gnu.mapping.InPort;

public class URI_utils
{
  /* #ifdef use:java.net.URI */
  public static URI toURI (Object uri)
    throws java.net.URISyntaxException
  {
    if (uri instanceof String || uri instanceof FString)
      return new URI(filenameToURIString(uri.toString()));
    if (uri instanceof File)
      {
        File file = (File) uri;
        if (file.isAbsolute())
          return file.toURI();
        else
          return new URI(toURIString(file));
      }
    if (uri instanceof URL)
      uri = URLtoURI((URL) uri);
    return (URI) uri;
  }
  /* #else */
  // public static String toURI (Object uri)
  // {
  //   if (uri instanceof String || uri instanceof FString)
  //     return filenameToURIString(uri.toString());
  //   if (uri instanceof File)
  //     return toURIString((File) uri);
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
    throws java.net.URISyntaxException // ???
  {
    if (uri instanceof String || uri instanceof FString)
      return filenameToURIString(uri.toString());
    if (uri instanceof File)
      return toURIString((File) uri);
    /* #ifdef use:java.net.URI */
    if (uri instanceof URL)
      uri = URLtoURI((URL) uri);
    return ((URI) uri).toString();
    /* #else */
    // return ((URL) uri).toString();
    /* #endif */
  }

  public static String filenameToURIString (String fname)
  {
    int flen = fname.length();
    char fileSep = File.separatorChar;
    if (fileSep != '/')
      {
        // Check for Windows absolute filename.
        if (fname.length() >= 2
            && ((fname.charAt(1) == ':' && Character.isLetter(fname.charAt(0)))
                || (fname.charAt(0) == fileSep && fname.charAt(1) == fileSep)))
          return toURIString(new File(fname));
        fname = fname.replace(fileSep, '/');
      }
    if (flen > 0 && fname.charAt(0) == '/')
      fname = "file:" + fname;
    return fname;
  }

  public static String toURIString (File file)
  {
    char fileSep = File.separatorChar;
    if (file.isAbsolute())
      {
        /* #ifdef use:java.net.URI */
        return file.toURI().toString();
        /* #else */
        /* #ifdef JAVA2 */
        // try
        //   {
        //     return file.toURL().toString();
        //   }
        // catch (Throwable ex)
        //   {
        //     throw gnu.mapping.WrappedException.wrapIfNeeded(ex);
        //   }
        /* #else */
        // return "file:" + file.getAbsolutePath().replace(fileSep, '/');
        /* #endif */
        /* #endif */
      }
    String fname = file.toString();
    if (fileSep != '/')
      fname = fname.replace(fileSep, '/');
    return fname;
  }
}
