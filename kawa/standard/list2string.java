package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "list->string".
 */

public class list2string extends Procedure1
{
  public Object apply1 (Object arg1)
       throws WrongType
  {
    List l = (List)arg1;
    int len = l.length();
    FString result = new FString (len);
    for (int i=0; i < len; i++)
      {
	Pair pair = (Pair) arg1;
	result.setCharAt (i, ((Char) pair.car).charValue ());
	arg1 = pair.cdr;
      }
     return result;
  }
}
