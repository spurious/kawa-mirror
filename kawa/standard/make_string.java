package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class make_string extends Procedure1or2
{
  public final Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply2 (arg1, Char.make (0));
  }

  public final Object apply2 (Object arg1,Object arg2)
  {
    return new FString (IntNum.intValue (arg1),
			((Char)arg2).charValue());
  }
}
