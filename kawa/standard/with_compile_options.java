package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import java.util.Vector;
import java.util.Stack;

public class with_compile_options extends Syntax
{
  public boolean scanForDefinitions (Pair st, Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Stack stack = new Stack();
    Object rest = getOptions(st.cdr, stack, this, tr);
    if (rest != LList.Empty)
      {
	int oldCount = forms.size();
	tr.scan_body(rest, forms, defs);
	int newCount = forms.size();
	Vector newForms = new Vector(newCount - oldCount);
	for (int i = oldCount;  i < newCount;  i++)
	  newForms.addElement(forms.elementAt(i));
	forms.setSize(oldCount);
	forms.addElement(LList.list4(this, stack, newForms, defs));
      }
    tr.currentOptions.popOptionValues(stack);
    return true;
  }

  public static Object getOptions (Object form, Stack stack,
				      Syntax syntax, Translator tr)
  {
    boolean seenKey = false;
    System.err.println("getProp start form:"+form);
    gnu.text.Options options = tr.currentOptions;
    while (form instanceof Pair)
      {
	Pair pair = (Pair) form;
	if (! (pair.car instanceof Keyword))
	  break;
	String key = ((Keyword) pair.car).getName();
	seenKey = true;
	System.err.println("key:"+key);
	Object savePos = tr.pushPositionOf(pair);
	try
	  {
	    form = pair.cdr;
	    if (! (form instanceof Pair))
	      {
		tr.error('e', "keyword " + key + " not followed by value");
		return LList.Empty;
	      }
	    pair = (Pair) form;
	    Object value = pair.car;
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
      tr.error('e', "no option keyword in "+syntax.getName());
    return form;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Expression result;
    Stack stack;
    Object obj = form.cdr;
  rewrite:
    if (obj instanceof Pair
	&& ((Pair) obj).car instanceof Stack)
      {
	form = (Pair) obj;
	stack = (Stack) form.car;
	obj = form.cdr;
	if (obj instanceof Pair)
	  {
	    form = (Pair) obj;
	    Vector forms = (Vector) form.car;
	    obj = form.cdr;
	    if (form.car instanceof Vector && obj instanceof Pair)
	      {
		form = (Pair) obj;
		ScopeExp defs = (ScopeExp) form.car;
		if (form.cdr == LList.Empty)
		  {
		    tr.currentOptions.pushOptionValues(stack);
		    result = tr.makeBody(forms, defs);
		    tr.currentOptions.popOptionValues(stack);
		    break rewrite;
		  }
	      }
	  }
	return tr.syntaxError("internal error handling "+getName());
      }
    else
      {
	stack = new Stack();
	Object rest = getOptions(obj, stack, this, tr);
	try
	  {
	    result = tr.rewrite_body(rest);
	  }
	finally
	  {
	    tr.currentOptions.popOptionValues(stack);
	  }
      }
    BeginExp bresult;
    if (result instanceof BeginExp)
      bresult = (BeginExp) result;
    else
      bresult = new BeginExp (new Expression[] { result });
    bresult.setCompileOptions(stack);
    return bresult;
  }
}

