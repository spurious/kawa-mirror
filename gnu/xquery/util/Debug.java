package gnu.xquery.util;
import gnu.mapping.*;

public class Debug
{
  public static String tracePrefix = "XQuery-trace: ";
  public static OutPort tracePort = null;
  public static String traceFilename = "XQuery-trace.log";
  public static boolean traceShouldFlush = true;
  public static boolean traceShouldAppend = false;

  public static synchronized Object trace (Object value, Object message)
  {
    OutPort out = tracePort;
    if (out == null)
      {
        try
          {
            out = new OutPort(new java.io.FileOutputStream(traceFilename,
                                                           traceShouldAppend));
          }
        catch (Throwable ex)
          {
            new WrappedException("Could not open '"+traceFilename
                                 +"' for fn:trace output", ex);
          }
        tracePort = out;
      }
    out.print(tracePrefix);
    out.print(message);
    out.print(' ');
    out.println(value);
    if (traceShouldFlush)
      out.flush();
    return value;
  }
}
