package gnu.jemacs.lang;
import gnu.mapping.*;

public abstract class SymbolConstraint extends Constraint
{
  public abstract Object getFunctionBinding (Binding binding);

  public void setFunctionBinding (Binding binding, Object value)
  {
    synchronized (binding)
      {
        Constraint constraint = getConstraint(binding);
        SymbolConstraint sconstraint;
	if (constraint != this && constraint instanceof SymbolConstraint)
          sconstraint = (SymbolConstraint) constraint;
        else
          binding.setConstraint(sconstraint = (new GeneralSymbolConstraint()));
        sconstraint.setFunctionBinding(binding, value);
      }
  }

  public Procedure getProcedure (Binding binding)
  {
    return (Procedure) getFunctionBinding (binding);
  }

}
