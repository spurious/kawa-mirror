package gnu.jemacs.lang;
import gnu.mapping.*;

public class ObArray extends Environment
{
  ValueSymbolConstraint valueConstraint = new ValueSymbolConstraint(this);
  FunctionSymbolConstraint functionConstraint = new FunctionSymbolConstraint(this);

  public static Environment asEnvironment(Object env)
  {
    if (env instanceof kawa.lang.Vector)
      {
        // LispRef says the only valid way to create an obarray is
        // (make-vector LENGTH 0).  This here is a kludge to associate
        // an ObArray with a Vector.  It wastes memory (since we only
        // use element 0 as a link to the assocaited ObArray), but Emacs
        // does not create obarrays much, and the few places that create
        // them could be speeded up by using (make-obarray LENGTH).
        kawa.lang.Vector vec = (kawa.lang.Vector) env;
        env = vec.elementAt(0);
        if (! (env instanceof Environment))
          {
            env = new ObArray(vec.length());
            vec.setElementAt(env, 0);
          }
        
      }
    return (Environment) env;
  }

  public ObArray (int initialCapacity)
  {
    super(initialCapacity);
    this.unboundConstraint = new UnboundSymbolConstraint(this);
  }

  public ObArray (Environment previous)
  {
    super(previous);
    this.unboundConstraint = new UnboundSymbolConstraint(this);
  }
}
