package kawa.standard;
import gnu.mapping.*;

/** An Environment that does special handling for names of the form "<TYPE>".
 * I.e. if an identifier of the form is unbound, then get a matching Type.
 */

public class ScmEnv extends Environment
{
  public ScmEnv (Environment previous)
  {
    super (previous);
    unboundConstraint = new ScmEnvConstraint(this);
  }

  static gnu.bytecode.Type getType (String name)
  {
    int len = name.length();
    if (len > 2 && name.charAt(0) == '<' && name.charAt(len-1) == '>')
      {
	String tname = name.substring(1, len-1);
	return Scheme.string2Type(tname);
      }
    return null;
  }

  public Object getChecked (String name)
  {
    try
      {
	return super.getChecked(name);
      }
    catch (UnboundSymbol ex)
      {
	gnu.bytecode.Type type = getType(name);
	if (type != null)
	  return type;
	throw ex;
      }
  }

}

class ScmEnvConstraint extends UnboundConstraint
{
  public ScmEnvConstraint (Environment environment)
  {
    super(environment);
  }

  public Object get (Binding binding)
  {
    String name = binding.getName();
    gnu.bytecode.Type type = ScmEnv.getType(name);
    if (type == null)
      throw new UnboundSymbol(name);
    set(binding, type);
    return type;
  }
}
