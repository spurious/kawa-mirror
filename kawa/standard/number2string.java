package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.util.*;

public class number2string extends Procedure1or2
{
  public static String format (Object arg, int radix)
  {
    return ((Numeric)arg).toString (radix);
  }

  public final Object apply1 (Object arg1)
  {
    return new FString (format (arg1, 10));
  }

  public final Object apply2 (Object arg1, Object arg2)
  {
    return new FString (format (arg1, ((IntNum) arg2).intValue ()));
  }
}
