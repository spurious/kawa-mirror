package gnu.xquery.util;
import gnu.mapping.*;

public class Debug
{
  public static Object trace (Object value, Object message)
  {
    OutPort err = OutPort.errDefault();
    err.print("XQuery-trace: ");
    err.print(message);
    err.print(' ');
    err.println(value);
    return value;
  }
}
