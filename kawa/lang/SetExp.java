package kawa.lang;

/** An Expression to set (bind) or define a new value to a named variable.
 * @author	Per Bothner
 */

public class SetExp extends Expression
{
  /** The name of the variable to set. */
  Symbol name;
  /** If non-null, the local Declaration that matches name. */
  public Declaration binding;
  /** The new value to assign to the variable. */
  Expression new_value;

  public SetExp (Symbol sym, Expression val)
  { name = sym;  new_value = val; }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Object new_val = new_value.eval (env);

    if (binding != null && env.values != null)
      env.values[binding.index] = new_val;
    else
      env.interp.define (name, new_val);
    return Interpreter.undefinedObject;
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%set! ");
    kawa.lang.print.print (name, ps);
    ps.print(" ");
    new_value.print (ps);
    ps.print(")");
  }
}
