package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme procedure "eof-object?". */

public class eof_object_p extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return Interpreter.boolObject (arg1 == Sequence.eofValue);
  }
}
