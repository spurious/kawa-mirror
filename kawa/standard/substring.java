package kawa.standard;
import kawa.lang.*;

public class substring extends Procedure3
{
  public substring()
  {
    super("substring");
  }
  
  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongType, GenericError
  {
    return new StringBuffer (
         ((StringBuffer)arg1).toString().substring(
            ((Integer)arg2).intValue(),
            ((Integer)arg3).intValue()
         )
      );
   }

}
