import gnu.mapping.*;
import gnu.expr.*;
public class MyModule implements Runnable
{
  public void run ()
  {
    Interpreter interp = Interpreter.getInterpreter();
    Object arg = Boolean.TRUE;
    interp.defineFunction (new MyFunc ("my-func-t", arg));
  }
}

