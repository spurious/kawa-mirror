package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.text.CharArrayInPort;

public class string2number extends Procedure1or2
{
  static Object parse (Object str, int radix)
  {
    try
      {
	InPort iport;
	if (str instanceof FString)
	  iport = ((FString) str).open();
	else
	  iport = new CharArrayInPort(str.toString());
	ScmRead lexer = new ScmRead(iport);
	Object result = lexer.readNumber(radix);
	if (lexer.checkErrors(null, 0))
	  return Scheme.falseObject;
	return result;
      }
    catch (java.io.IOException ex)
      {
	throw new GenericError ("internal input error in string->number - " +
                                ex.getMessage ());
      }
  }

  public final Object apply1 (Object arg1)
  {
    return parse(arg1, 10);
  }

  public final Object apply2 (Object arg1, Object arg2)
  {
    return parse(arg1, IntNum.intValue (arg2));
  }
}
