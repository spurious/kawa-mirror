package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class make_vector extends Procedure1or2
{
  public final Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply2 (arg1, Interpreter.undefinedObject);
  }

  public final Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return new Vector (IntNum.intValue (arg1), arg2);
  }
}
