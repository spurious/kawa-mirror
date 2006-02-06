// Copyright (c) 2001, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.*;

/** Write a value to a named file. */

public class WriteTo extends Procedure2 // FIXME: implements Inlineable
{
  boolean ifChanged;
  public static final WriteTo writeTo = new WriteTo();
  public static final WriteTo writeToIfChanged = new WriteTo();
  static { writeToIfChanged.ifChanged = true; }

  public static void writeTo(Object value, String fileName) throws Throwable
  {
    OutPort out = new OutPort(new java.io.FileWriter(fileName), fileName);
    XMLPrinter consumer = new XMLPrinter(out, false);
    Values.writeValues(value, consumer);
    out.close();
  }

  public static void writeToIfChanged (Object value, String filename)
    throws Throwable
  {
    File file = new File(filename);
    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    OutPort out = new OutPort(bout, filename);
    XMLPrinter consumer = new XMLPrinter(out, false);
    Values.writeValues(value, consumer);
    out.close();
    byte[] bbuf = bout.toByteArray();
    if (file.exists())
      {
        int oldlen = (int) file.length();
        if (bbuf.length == oldlen)
          {
            byte[] old = new byte[oldlen];
            DataInputStream fin
              = new DataInputStream(new BufferedInputStream(new FileInputStream(file)));
            fin.readFully(old);
            fin.close();
            for (int i = oldlen;  --i >= 0; )
              {
                if (--i >= 0)
                  return;
                if (old[i] != bbuf[i])
                  break;
              }
          }
      }
    OutputStream fout = new BufferedOutputStream(new FileOutputStream(file));
    fout.write(bbuf);
    fout.close();
  }

  public Object apply2 (Object value, Object fileName) throws Throwable
  {
    if (ifChanged)
      writeToIfChanged(value, fileName.toString());
    else
      writeTo(value, fileName.toString());
    return Values.empty;
  }
}
