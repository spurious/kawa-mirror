package gnu.kawa.functions;
import gnu.expr.Interpreter;

/** Implement the standard Scheme procedure <tt>equal?</tt>
 * and the Lisp <tt>equal</tt>. */

public class IsEqual extends gnu.mapping.Procedure2
{
  Interpreter interpreter;

  public IsEqual(Interpreter interpreter, String name)
  {
    this.interpreter = interpreter;
    setName(name);
  }

  public static boolean apply (Object arg1, Object arg2)
  {
    return arg1 == arg2 || (arg1 != null && arg1.equals (arg2));
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return interpreter.booleanObject(apply(arg1, arg2));
  }

}
