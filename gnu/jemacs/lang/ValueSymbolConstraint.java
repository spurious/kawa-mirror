package gnu.jemacs.lang;
import gnu.mapping.*;

/** This is used for s symbol that has a value binding only. */

public class ValueSymbolConstraint extends SymbolConstraint
{
  ObArray environment;

  ValueSymbolConstraint (ObArray environment)
  {
    this.environment = environment;
  }  

  public Object get (Binding binding)
  {
    return getValue(binding);
  }

  public void set (Binding binding, Object value)
  {
    setValue(binding, value);
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
