package kawa.standard;
import gnu.kawa.util.*;
import gnu.mapping.*;

public class string_v extends ProcedureN
{
  public string_v()
  {
    super("string");
  }

  public Object applyN (Object[] args)
  {
    int count = args.length;
    FString str = new FString (count);
    for (int i = 0; i < count; i++)
      {
	str.setCharAt (i, ((Char)(args[i])).charValue());
      }
    return str;
  }
}
