package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

public class logtest extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError
  {
    return Interpreter.boolObject (BitOps.test ((IntNum) arg1, (IntNum) arg2));
  }
}
