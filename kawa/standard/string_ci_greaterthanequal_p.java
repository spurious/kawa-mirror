package kawa.standard;
import kawa.lang.*;

public class string_ci_greaterthanequal_p extends Procedure2
{
  public string_ci_greaterthanequal_p() {
    super("string-ci>=?");
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (((java.lang.StringBuffer)arg1).toString().toLowerCase().compareTo(
             ((java.lang.StringBuffer)arg2).toString().toLowerCase()
          )>=0)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }

}
