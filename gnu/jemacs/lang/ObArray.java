package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.kawa.util.*;

public class ObArray extends Environment
{
  /**
   * Get the function binding for a symbol.
   * @exception gnu.mapping.UnboundSymbol the name has no function binding
   */
  public Object getFunction(String name)
  {
    return Symbol.getFunctionBinding(this, name);
  }

  /** Set the function binding for a symbol.
   * this is equivalent to put.
   */
  public void putFunction(String name, Object value)
  {
    gnu.jemacs.lang.Symbol.setFunctionBinding(this, name, value);
  }

  /**
   * Define the value or function binding for a symbol, as appropriate
   */
  public Binding define (String name, Object value)
  {
    Binding2 binding = Binding2.getBinding2(this, name);
    if (value instanceof Procedure || value instanceof kawa.lang.Syntax)
      binding.functionValue = value;
    else
      binding.set(value);
    return binding;
  }

  public static Environment asEnvironment(Object env)
  {
    if (env instanceof FVector)
      {
        // LispRef says the only valid way to create an obarray is
        // (make-vector LENGTH 0).  This here is a kludge to associate
        // an ObArray with a Vector.  It wastes memory (since we only
        // use element 0 as a link to the assocaited ObArray), but Emacs
        // does not create obarrays much, and the few places that create
        // them could be speeded up by using (make-obarray LENGTH).
        FVector vec = (FVector) env;
        env = vec.get(0);
        if (! (env instanceof Environment))
          {
            env = new ObArray(vec.length());
            vec.set(0, env);
          }
        
      }
    return (Environment) env;
  }

  public ObArray ()
  {
    super();
  }

  public ObArray (int initialCapacity)
  {
    super(initialCapacity);
  }

  public ObArray (Environment previous)
  {
    super(previous);
  }
}
