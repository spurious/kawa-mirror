package kawa.standard;
import kawa.lang.*;

public class string_ci_equal_p extends Procedure2
{
   public string_ci_equal_p() {
      super("string-ci=?");
   }

   public Object apply2 (Object arg1, Object arg2)
   {
     if (((StringBuffer)arg1).toString().equalsIgnoreCase(
	     ((StringBuffer)arg2).toString()
          ))
       return Interpreter.trueObject;
     else
       return Interpreter.falseObject;
   }
}
