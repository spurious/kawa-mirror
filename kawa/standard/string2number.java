package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.kawa.lispexpr.*;

public class string2number extends Procedure1or2
{
  static Object parse (Object str, int radix)
  {
    try
      {
	FString fstr = (FString) str;
	int len = fstr.length();
	char[] data = fstr.data;
	Object result = LispReader.parseNumber(data, 0, len, '\0',
					       radix, LispReader.SCM_NUMBERS);
	if (! (result instanceof Numeric))
	  return Boolean.FALSE;
	return result;
      }
    catch (Throwable ex)
      {
	ex.printStackTrace();
	return Boolean.FALSE;
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
