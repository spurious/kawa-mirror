package gnu.jemacs.lang;
import gnu.mapping.*;

/** This is used for s symbol that has a function only. */

public class FunctionSymbolConstraint extends SymbolConstraint
{
  Environment environment;

  FunctionSymbolConstraint (Environment environment)
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
	  binding.setConstraint(new GeneralSymbolConstraint());
	getConstraint(binding).set(binding, value);
      }
  }

  public Object getFunctionBinding (Binding binding)
  {
    return getValue(binding);
  }

  public void setFunctionBinding (Binding binding, Object value)
  {
    setValue(binding, value);
  }

  public Procedure getProcedure (Binding binding)
  {
    return (Procedure) getValue(binding);
  }

}
