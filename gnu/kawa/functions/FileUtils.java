package gnu.kawa.functions;
import java.io.*;
import gnu.mapping.InPort;

public class FileUtils
{
  /* #ifndef JAVA2 */
  // static private int tempFileNumber;
  /* #endif */

  public static
  /* #ifndef JAVA2 */
  // synchronized
  /* #endif */
  File createTempFile (String format)
    throws java.io.IOException
  {
    if (format == null)
      format = "kawa~d.tmp";
    int tilde = format.indexOf('~');
    String prefix, suffix;
    File directory = null;
    if (tilde < 0)
      {
        prefix = format;
        suffix = ".tmp";
      }
    else
      {
        prefix = format.substring(0, tilde);
        suffix = format.substring(tilde+2);
      }
    int sep = prefix.indexOf(File.separatorChar);
    if (sep >= 0)
      {
        directory = new File(prefix.substring(0, sep));
        prefix = prefix.substring(sep+1);
      }
    /* #ifdef JAVA2 */
    return File.createTempFile(prefix, suffix, directory);
    /* #else */
    // if (directory == null)
    //   directory = new File(File.separatorChar == '\\' ? "c:\\temp" : "/tmp");
    // for (;;)
    //   {
    //     File temp = new File(directory,
    //                          prefix + (++tempFileNumber) + suffix);
    //     if (! temp.exists())
    //       {
    //         OutputStream out = new FileOutputStream(temp);
    //         out.close();
    //         return temp;
    //       }
    //   }
    /* #endif */
  }
}