package kawa.standard;
import gnu.mapping.*;
import gnu.math.Unit;

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
  Environment[] extras;
  int  numExtras;

  /** Add an extra Environment that also gets searched. */
  public final void addExtra(Environment env)
  {
    if (extras == null)
      {
	extras = new Environment[4];
	extras[0] = env;
	numExtras = 1;
      }
    else
      {
	if (numExtras > extras.length)
	  {
	    Environment[] tmp = new Environment[2 * numExtras];
	    System.arraycopy(extras, 0, tmp, 0, numExtras);
	    extras = tmp;
	  }
	extras[numExtras++] = env;
      }
  }

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

  public Object get (String name, Object defaultValue)
  {
    Symbol symbol = lookup(name);
    Object value = symbol == null ? defaultValue : symbol.get(defaultValue);
    if (value != defaultValue
	|| (defaultValue != Symbol.UNBOUND
	    && super.get(name, Symbol.UNBOUND) != Symbol.UNBOUND))
      return value;

    if (symbol != null)
      {
	symbol = AliasConstraint.followAliases(symbol);
	if (symbol != null)
	  name = symbol.getName();
      }

    if (name.endsWith("$unit"))
      {
	Unit unit = Unit.lookup(name.substring(0, name.length()-5));
	if (unit != null)
	  return unit;
      }
    gnu.bytecode.Type type = getType(name);
    if (type != null)
      return type;

    for (int i = numExtras;  --i >= 0; )
      {
	value = extras[i].get(name, Symbol.UNBOUND);
	if (value != Symbol.UNBOUND)
	  return value;
      }
    return defaultValue;
  }

}

class ScmEnvConstraint extends UnboundConstraint
{
  public ScmEnvConstraint (Environment environment)
  {
    super(environment);
  }

  public Object get (Symbol symbol, Object defaultValue)
  {
    String name = symbol.getName();
    if (name.endsWith("$unit"))
      {
        Unit unit = Unit.lookup(name.substring(0, name.length()-5));
        if (unit != null)
          return unit;
      }
    gnu.bytecode.Type type = ScmEnv.getType(name);
    if (type == null)
      return defaultValue;
    set(symbol, type);
    return type;
  }
}
