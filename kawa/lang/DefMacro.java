package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;

public class DefMacro extends Syntax
{
  Procedure expander;

  public DefMacro (Procedure expander)
  {
    this.expander = expander;
  }

  public DefMacro (String name, Procedure expander)
  {
    String sym = Symbol.make(name);
    this.setName(sym);
    expander.setName(sym);
    this.expander = expander;
    Environment.define_global(sym, this);
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    int count = List.length(obj);
    /* DEBUGGING:
    System.err.print("{Before defmacro expansion: ");
    SFormat.print (obj, System.err);
    System.err.println ('}');
    */
    Object[] args = new Object[count];
    for (int i = 0; obj instanceof Pair; i++)
      {
	Pair pair = (Pair) obj;
	args[i] = pair.car;
	obj = pair.cdr;
      }
    try
      {
	Object expansion = expander.applyN(args) ;
	/* DEBUGGING:
	System.err.print("{Expanded macro: ");
	SFormat.print (expansion, System.err);
	System.err.println ('}');
	*/
	return tr.rewrite(expansion);
      }
    catch (Exception ex)
      {
	return tr.syntaxError("caught exception ("+ex+") while expanding macro "+ name());
      }
  }
}

