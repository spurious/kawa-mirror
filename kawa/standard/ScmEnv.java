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
  }

  public Object get (String name)
  {
    Binding binding = lookup (name);
    if (binding != null)
      return binding.get();
    int len = name.length();
    if (len > 2 && name.charAt(0) == '<' && name.charAt(len-1) == '>')
      {
	String tname = name.substring(1, len-1);
	gnu.bytecode.Type type = Scheme.string2Type(tname);
	if (type != null && type.getReflectClass() != null)
	  return type;
      }
    return null;
  }
}
