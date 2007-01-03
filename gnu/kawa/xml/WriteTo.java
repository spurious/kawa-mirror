// Copyright (c) 2001, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.*;
import gnu.text.Path;

/** Write a value to a named file. */

public class WriteTo extends Procedure2 // FIXME: implements Inlineable
{
  boolean ifChanged;
  public static final WriteTo writeTo = new WriteTo();
  public static final WriteTo writeToIfChanged = new WriteTo();
  static { writeToIfChanged.ifChanged = true; }

  public static void writeTo(Object value, Object path) throws Throwable
  {
    Path ppath = Path.valueOf(path);
    OutputStream outs = ppath.openOutputStream();
    OutPort out = new OutPort(outs, ppath);
    XMLPrinter consumer = new XMLPrinter(out, false);
    Values.writeValues(value, consumer);
    out.close();
  }

  public static void writeToIfChanged (Object value, Object path)
    throws Throwable
  {
    Path ppath = Path.valueOf(path);
    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    OutPort out = new OutPort(bout, ppath);
    XMLPrinter consumer = new XMLPrinter(out, false);
    Values.writeValues(value, consumer);
    out.close();
    byte[] bbuf = bout.toByteArray();
    try
      {
        InputStream ins = new BufferedInputStream(ppath.openInputStream());
        for (int i = 0;  ; )
          {
            int b = ins.read();
            boolean atend = i == bbuf.length;
            if (b < 0)
              {
                if (! atend)
                  break;
                ins.close();
                return;
              }
            if (atend || bbuf[i++] != b)
              break;
          }
        ins.close();
      }
    catch (Throwable ex)
      {
        // fall through
      }
    OutputStream fout
      = new BufferedOutputStream(ppath.openOutputStream());
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
