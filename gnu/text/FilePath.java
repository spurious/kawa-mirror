// Copyright (c) 2007  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.text;
import java.io.*;
import java.net.*;
import gnu.mapping.WrappedException; // FIXME

/** A wrapper around a {@code java.io.File} that extends {@code Path}. */

public class FilePath
  extends Path
  /* #ifdef JAVA2 */
  /* #ifdef JAVA5 */
  // implements Comparable<FilePath>
  /* #else */
  implements Comparable
  /* #endif */
  /* #endif */
{
  File file;

  private FilePath (File file)
  {
    this.file = file;
  }

  public static FilePath valueOf (String str)
  {
    int len = str.length();
    /* FIXME: Should we expand '~'?
    if (len > 0 && str.charAt(0) == '~' && File.separatorChar == '/')
      {
        if (len == 1 || str.charAt(1) == '/')
          {
            String user = System.getProperty("user.home");
            if (user != null)
              str = user + str.substring(1);
          }
        else
          {
            // We don't support '~USER/...'  Do that using /bin/sh. FIXME
          }
      }
    */
    return valueOf(new File(str));
  }

  public static FilePath valueOf (File file)
  {
    return new FilePath(file);
  }

  public static FilePath makeFilePath (Object path)
  {
    if (path instanceof FilePath)
      return (FilePath) path;
    if (path instanceof URIPath)
      return FilePath.valueOf(new File(((URIPath) path).uri));
    /*
    if (path instanceof URL)
      return URLPath.valueOf((URL) path);
    */
    /* #ifdef use:java.net.URI */
    if (path instanceof URI)
      return FilePath.valueOf(new File((URI) path));
    /* #endif */
    if (path instanceof File)
      return FilePath.valueOf((File) path);
    if (path instanceof gnu.lists.FString) // FIXME: || UntypedAtomic
      path = path.toString();
    String str = (String) path;
    return FilePath.valueOf(str);
  }

  public boolean isAbsolute ()
  {
    return this == Path.userDirPath || file.isAbsolute();
  }

  public long getLastModified ()
  {
    return file.lastModified();
  }

  public boolean exists ()
  {
    return file.exists();
  }

  public int compareTo (FilePath path)
  {
    return file.compareTo(path.file);
  }

  /* #ifndef JAVA5 */
  public int compareTo (Object obj)
  {
    return compareTo((FilePath) obj);
  }
  /* #endif */

  public boolean equals (Object obj)
  {
    return obj instanceof FilePath && file.equals(((FilePath) obj).file);
  }

  public int hashCode ()
  {
    return file.hashCode();
  }

  public String toString ()
  {
    return file.toString();
  }

  public File toFile ()
  {
    return file;
  }

  public static URL toURL (File file)
  {
    try
      {
        // FIXME this resolves relative paths relative to Path.userDirPath,
        // but should be Path.currentPath().
        /* #ifdef JAVA2 */
        /* #ifdef use:java.net.URI */
        return file.toURI().toURL();
        /* #else */
        // return file.toURL();
        /* #endif */
        /* #else */
        // char fileSep = File.separatorChar;
        // return new URL("file:" + file.getAbsolutePath().replace(fileSep, '/'));
        /* #endif */
      }
    catch (Throwable ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public URL toURL ()
  {
    return toURL(file);
  }

  /* #ifdef use:java.net.URI */
  public static String toURIString (File file)
  {
    return toURI(file).toString();
  }
  /* #else */
  // public static String toURIString (File file)
  // {
  //   if (file.isAbsolute())
  //     return FiletoURL(file).toString();
  //   else
  //     return file.toString().replace(File.separatorChar, '/');
  // }
  /* #endif */

  /* #ifdef use:java.net.URI */
  public static URI toURI (File file)
  {
    try
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
    catch (Throwable ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }

  }

  public URI toURI ()
  {
    return toURI(file);
  }
  /* #endif */

  public InputStream openInputStream () throws IOException
  {
    return new FileInputStream(file);
  }

  public OutputStream openOutputStream () throws IOException
  {
    return new FileOutputStream(file);
  }

  public String getScheme ()
  {
    return "file";
  }

  public Path resolve (String relative)
  {
    if (Path.uriSchemeSpecified(relative))
      return URLPath.valueOf(relative);
    File rfile = new File(relative);
    if (rfile.isAbsolute())
      return FilePath.valueOf(rfile);
    char sep = File.separatorChar;
    if (sep != '/')
      relative = relative.replace('/', sep);
    // FIXME? Or: if (file.getPath().length() == 0) return relative;
    File nfile;
    if (this == Path.userDirPath)
      nfile = new File(System.getProperty("user.dir"), relative);
    else
      nfile = new File(file, relative);
    return valueOf(nfile);
  }
}
