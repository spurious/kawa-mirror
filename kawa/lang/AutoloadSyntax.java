package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * Implement autoloading of Syntax (including macros).
 * A named class is loaded, and apply requests are forwarded to it.
 * @author	Per Bothner
 */

public class AutoloadSyntax extends Syntax
{
  /** The name of the class that defines the macro/builtin.
   * It must be the name of a class in the CLASSPATH (for example:
   * "kawa.standard.list").  Either the class must extend Syntax
   * (and have a default constructor), or the class must be a ModuleMody,
   * (whose apply0 () is expected to define the Syntax in the
   * global environment. */
  String className;

  /** The loaded syntax, or null if it has not yet been loaded. */
  Syntax loaded;

  public AutoloadSyntax (String name, String className)
  {
    super(name);
    this.className = className;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<syntax ");
    if (getName() != null)
      {
	ps.print(getName());
	ps.print(' ');
      }
    if (loaded != null)
      ps.print ("autoloaded>");
    else
      {
	ps.print ("autoload ");
	ps.print (className);
	ps.print (">");
      }
  }

  private void throw_error (String prefix)
  {
    throw new GenericError (prefix + className
				+ " while autoloading "
				+ (getName() == null ? "" : getName().toString()));
  }

  /** Load the class named in className. */
  void load ()
  {
    String name = this.getName();
    try
      {
	Object value = Class.forName (className).newInstance ();
	if (value instanceof ModuleBody)
	  {
            Environment env = Environment.current ();
	    ((ModuleBody) value).run();
	    value = env.get (name);
	    if (value == null || value == this
		|| !(value instanceof Syntax))
	      throw_error("syntax not found in ");
	    loaded = (Syntax) value;
	  }
	else if (value instanceof Syntax)
	  {
	    loaded = (Syntax) value;
	    if (name != null)
	      {
		if (Environment.lookup_global (name) == this)
		  Environment.define_global (name, loaded);
		if (loaded.getName() == null)
		  loaded.setName (name);
	      }
	  }
	else
	  throw_error ("failed to autoload valid syntax object ");
      }
    catch (ClassNotFoundException ex)
      {	throw_error ("failed to find class "); }
    catch (InstantiationException ex)
      { throw_error ("failed to instantiate class "); }
    catch (IllegalAccessException ex)
      { throw_error ("illegal access in class "); }
    catch (UnboundSymbol e)
      { throw_error ("missing symbol `" + e.getMessage () + "' "); }
    catch (WrongArguments ex)
      { throw_error ("type error"); }
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (loaded == null)
      {
	try
	  {
	    load ();
	  }
	catch (RuntimeException e)
	  {
	    tr.syntaxError (e.getMessage ());
            return false;
	  }
      }
    return loaded.scanForDefinitions(st, forms, defs, tr);
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (loaded == null)
      {
	try
	  {
	    load ();
	  }
	catch (GenericError e)
	  {
	    return tr.syntaxError (e.getMessage ());
	  }
	catch (WrongType e)
	  {
	    return tr.syntaxError (e.getMessage ());
	  }
      }
    return loaded.rewriteForm(form, tr);
  }
}
