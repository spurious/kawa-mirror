package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.*;

public class module_name extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object arg = ((Pair) form.cdr).car;
    String name;
    Pair p;
    if (arg instanceof Pair && (p = (Pair) arg).car == "quote")
      {
	arg = p.cdr;
	if (! (arg instanceof Pair)
	    || (p = (Pair) arg).cdr != LList.Empty
	    || ! (p.car instanceof String))
	  return tr.syntaxError("invalid quoted symbol for `module-name'");
	name = (String) p.car;
      }
    else if (arg instanceof FString)
      name = arg.toString();
    else if (arg instanceof String)
      {
	name = (String) arg;
	int len = name.length();
	if (len > 2
	    && name.charAt(0) == '<'
	    && name.charAt(len-1) == '>')
	  {
	    name = name.substring(1, len-1);
	  }
	else
	  return tr.syntaxError("not implemented: plain name in module-name");
      }
    else
      return tr.syntaxError("un-implemented expression in module-name");
    if (name.indexOf('.') < 0
	&& kawa.repl.compilationPrefix != null)
      name = kawa.repl.compilationPrefix + name;
    ModuleExp module = tr.getModule();
    module.setName(name);
    return QuoteExp.voidExp;
  }
}
