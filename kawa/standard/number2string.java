package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.util.*;

public class number2string extends Procedure1or2
{
  public static String apply (Object arg, int radix)
  {
    return ((Numeric)arg).toString (radix);
  }

  public static String apply (Object arg)
  {
    return ((Numeric)arg).toString(10);
  }

  public final Object apply1 (Object arg1)
  {
    return new FString(apply(arg1));
  }

  public final Object apply2 (Object arg1, Object arg2)
  {
    return new FString(apply(arg1, ((IntNum) arg2).intValue()));
  }
}
