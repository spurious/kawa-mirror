package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.FVector;
import gnu.math.IntNum;
import gnu.mapping.*;

public class make_vector extends Procedure1or2
{
  public static Object apply (int arg1)
  {
    return new FVector (arg1, Interpreter.undefinedObject);
  }

  public static Object apply (int arg1, Object arg2)
  {
    return new FVector (arg1, arg2);
  }

  public final Object apply1 (Object arg1)
  {
    return apply2 (arg1, Interpreter.undefinedObject);
  }

  public final Object apply2 (Object arg1,Object arg2)
  {
    return new FVector (IntNum.intValue (arg1), arg2);
  }
}
