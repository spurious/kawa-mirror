package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

public class logbit_p extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError
  {
    boolean result = BitOps.bitValue ((IntNum) arg1, IntNum.intValue (arg2));
    return Interpreter.boolObject (result);
  }
}
