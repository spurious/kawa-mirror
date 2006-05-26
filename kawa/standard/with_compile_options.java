package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import java.util.Stack;

public class with_compile_options extends Syntax
{
  public static final with_compile_options with_compile_options
    = new with_compile_options();
  static { with_compile_options.setName("with-compile-options"); }

  public void scanForm (Pair form, ScopeExp defs, Translator tr)
  {
    Stack stack = new Stack();
    Object rest = getOptions(form.cdr, stack, this, tr);
    if (rest == LList.Empty)
      return;
    if (rest == form.cdr)
      {
	tr.scanBody(rest, defs, false);
	return;
      }
    rest = tr.scanBody(rest, defs, true);
    rest = new Pair(stack, rest);
    tr.currentOptions.popOptionValues(stack);
    tr.formStack.add(Translator.makePair(form, form.car, rest));
  }

  public static Object getOptions (Object form, Stack stack,
				   Syntax command, Translator tr)
  {
    boolean seenKey = false;
    gnu.text.Options options = tr.currentOptions;
    SyntaxForm syntax = null;
    for (;;)
      {
	while (form instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) form;
	    form = syntax.form;
	  }
	if (! (form instanceof Pair))
	  break;
	Pair pair = (Pair) form;
	Object pair_car = Translator.stripSyntax(pair.car);
	if (! (pair_car instanceof Keyword))
	  break;
	String key = ((Keyword) pair_car).getName();
	seenKey = true;
	Object savePos = tr.pushPositionOf(pair);
	try
	  {
	    form = pair.cdr;
	    while (form instanceof SyntaxForm)
	      {
		syntax = (SyntaxForm) form;
		form = syntax.form;
	      }
	    if (! (form instanceof Pair))
	      {
		tr.error('e', "keyword " + key + " not followed by value");
		return LList.Empty;
	      }
	    pair = (Pair) form;
	    Object value = Translator.stripSyntax(pair.car);
	    form = pair.cdr;
	    Object oldValue = options.getLocal(key);
	    if (options.getInfo(key) == null)
	      {
		tr.error('w', "unknown compile option: "+key);
		continue;
	      }
	    if (value instanceof FString)
	      value = value.toString();
	    else if (value instanceof Boolean
		     || value instanceof Number)
	      ;
	    else
	      {
		value = null;
		tr.error('e', "invalid literal value for key "+key);
	      }
	    options.set(key, value, tr.getMessages());
	    if (stack != null)
	      {
		stack.push(key);
		stack.push(oldValue);
		stack.push(value);
	      }
	  }
	finally
	  {
	    tr.popPositionOf(savePos);
	  }
      }
    if (! seenKey)
      tr.error('e', "no option keyword in "+command.getName());
    return Translator.wrapSyntax(form, syntax);
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object rest;
    Stack stack;
    Object obj = form.cdr;
    Pair p;
    if (obj instanceof Pair
	&& (p = (Pair) obj).car instanceof Stack)
      {
	stack = (Stack) p.car;
	rest = p.cdr;
	tr.currentOptions.pushOptionValues(stack);
      }
    else
      {
	stack = new Stack();
	rest = getOptions(obj, stack, this, tr);
      }

    try
      {
	Expression result = tr.rewrite_body(rest);
	BeginExp bresult;
	if (result instanceof BeginExp)
	  bresult = (BeginExp) result;
	else
	  bresult = new BeginExp (new Expression[] { result });
	bresult.setCompileOptions(stack);
	return bresult;
      }
    finally
      {
	tr.currentOptions.popOptionValues(stack);
      }
  }
}

