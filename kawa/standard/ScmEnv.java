package kawa.standard;
import gnu.mapping.*;
import gnu.math.Unit;
import gnu.kawa.xml.ElementConstructor;

/** An Environment that does special handling for names of the form "<TYPE>".
 * I.e. if an identifier of the form is unbound, then get a matching Type.
 *
 * Also, handles U$unit by doing Unit.lookup("U").  (The Scheme reader
 * translates a quantity like 2in to (* 2 in$unit).  The advantage is
 * is that we can have clean scoping rules for unit names;  the downside
 * is that 2in is no longer a literal.)
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
        if (name.endsWith("$unit"))
          {
            Unit unit = Unit.lookup(name.substring(0, name.length()-5));
            if (unit != null)
              return unit;
          }
	gnu.bytecode.Type type = getType(name);
	if (type != null)
	  return type;
	int i = name.indexOf(':');
	if (i >= 0)
	  {
	    String prefix = name.substring(0, i);
	    try
	      {
		String uri = super.getChecked(("xmlns:"+prefix).intern()).toString();
		String localName = name.substring(i+1);
		return ElementConstructor.make(name, uri, localName);
	      }
	    catch (UnboundSymbol ex2)
	      {
		ex2.printStackTrace();
	      }
	  }
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
    if (name.endsWith("$unit"))
      {
        Unit unit = Unit.lookup(name.substring(0, name.length()-5));
        if (unit != null)
          return unit;
      }
    gnu.bytecode.Type type = ScmEnv.getType(name);
    if (type == null)
      throw new UnboundSymbol(name);
    set(binding, type);
    return type;
  }
}
