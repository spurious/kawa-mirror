package gnu.jemacs.lang;
import gnu.mapping.*;

public class GeneralSymbolConstraint extends SymbolConstraint
{
  Object value;
  Object function;
  //  Object properties;

  public Object get (Binding binding)
  {
    return value;
  }

  public void set (Binding binding, Object value)
  {
    this.value = value;
  }

  public void setFunctionBinding (Binding binding, Object value)
  {
    this.function = value;
  }

  public Object getFunctionBinding (Binding binding)
  {
    return function;
  }

  public Procedure getProcedure (Binding binding)
  {
    return (Procedure) function;
  }
}
