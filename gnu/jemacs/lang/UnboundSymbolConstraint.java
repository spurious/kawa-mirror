package gnu.jemacs.lang;
import gnu.mapping.*;

public class UnboundSymbolConstraint extends SymbolConstraint
{
  ObArray environment;

  UnboundSymbolConstraint (ObArray environment)
  {
    this.environment = environment;
  }  

  public boolean isBound (Binding binding)
  {
    return false;
  }

  public Object get (Binding binding)
  {
     throw new UnboundSymbol(binding.getName());
  }

  public void set (Binding binding, Object value)
  {
    synchronized (binding)
      {
	if (getConstraint(binding) == this)
	  binding.setConstraint(environment.valueConstraint);
	getConstraint(binding).set(binding, value);
      }
  }

  public Procedure getProcedure (Binding binding)
  {
    throw new UnboundSymbol(binding.getName());
  }

  public Object getFunctionBinding (Binding binding)
  {
    throw new UnboundSymbol(binding.getName());
  }
}
