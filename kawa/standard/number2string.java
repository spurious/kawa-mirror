package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.util.*;

public class number2string extends Procedure1or2
{
  public static FString apply (Object arg, int radix)
  {
    return new FString(((Numeric)arg).toString (radix));
  }

  public static FString apply (Object arg)
  {
    return new FString(((Numeric)arg).toString(10));
  }

  public final Object apply1 (Object arg1)
  {
    return apply(arg1);
  }

  public final Object apply2 (Object arg1, Object arg2)
  {
    return apply(arg1, ((IntNum) arg2).intValue());
  }
}
