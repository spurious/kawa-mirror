package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme procedure "string-copy". */

public class string_copy extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof FString)
      return ((FString)arg1).copy ();
    else
      return new FString (arg1.toString().toCharArray());
  }
}
