package kawa.standard;
import gnu.mapping.*;
import gnu.expr.Compilation;
import gnu.bytecode.ClassType;
import gnu.math.Unit;
import gnu.kawa.reflect.ClassMethods;

/** An Environment that does special handling for names of the form "<TYPE>".
 * I.e. if an identifier of the form is unbound, then get a matching Type.
 *
 * Also, handles U$unit by doing Unit.lookup("U").  (The Scheme reader
 * translates a quantity like 2in to (* 2 in$unit).  The advantage is
 * is that we can have clean scoping rules for unit names;  the downside
 * is that 2in is no longer a literal.)
 */

public class ScmEnv extends InheritingEnvironment
{
  public ScmEnv (String name, Environment previous)
  {
    super(name, previous);
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

  public NamedLocation lookupExtend (Symbol name, Object property, int hash)
  {
    String nam = name.getName();
    if (property == null)
      {
	Object val = null;
	String uri = name.getNamespaceURI();
	if (uri != null && uri.startsWith("class:"))
	  {
	    String mname = nam.equals("new") ? "<init>"
	      : Compilation.mangleName(nam);
	    ClassType methodClass = ClassType.make(uri.substring(6));
	    val = ClassMethods.apply(methodClass, mname, null, null, 0, 0);
	  }
	else if (nam.endsWith("$unit"))
	  val = Unit.lookup(nam.substring(0, nam.length()-5));
	else 
	  val = getType(nam);
	if (val != null)
	  return define(name, property, hash, val);
      }
    
    return null;
  }
}
