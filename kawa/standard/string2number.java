package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

public class string2number extends Procedure1or2
{
  static Object parse (String str, int radix)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	InPort iport = call_with_input_string.open_input_string (str);
	return iport.readSchemeNumber (radix);
      }
    catch (ReadError ex)
      {
	return Scheme.falseObject;
      }
    catch (java.io.IOException ex)
      {
	throw new GenericError ("internal input error in string->number - " +
                                ex.getMessage ());
      }
  }

  public final Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return parse (arg1.toString (), 10);
  }

  public final Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return parse (arg1.toString (), IntNum.intValue (arg2));
  }
}
