// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.xml.*;

/** Write a value to a named file. */

public class WriteTo extends Procedure2 // FIXME: implements Inlineable
{
  public static final WriteTo writeTo = new WriteTo();

  public static void writeTo(Object value, String fileName) throws Throwable
  {
    OutPort out = new OutPort(new java.io.FileWriter(fileName), fileName);
    XMLPrinter consumer = new XMLPrinter(out, false);
    Values.writeValues(value, consumer);
    out.close();
  }

  public Object apply2 (Object value, Object fileName) throws Throwable
  {
    writeTo(value, fileName.toString());
    return Values.empty;
  }
}
