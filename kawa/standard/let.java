package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.Stack;

/**
 * The Syntax transformer that re-writes the Scheme "let" primitive.
 * This only handles standard "unnamed" let.
 * The let macro in ../lib/let.scm handles named let as well.
 * @author	Per Bothner
 */

public class let extends Syntax
{
  public static final let let = new let();
  static { let.setName("let"); }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = Translator.listLength(bindings);
    if (decl_count < 0)
      return tr.syntaxError("bindings not a proper list");
      
    Expression[] inits = new Expression[decl_count];
    LetExp let = new LetExp (inits);
    Stack renamedAliases = null;
    int renamedAliasesCount = 0;
    SyntaxForm syntaxRest = null;
    for (int i = 0; i < decl_count; i++)
      {
	while (bindings instanceof SyntaxForm)
	  {
	    syntaxRest = (SyntaxForm) bindings;
	    bindings = syntaxRest.form;
	    // The SyntaxForm "surrounds" both the current binding (the car),
	    // as well as the cdr - i.e. the remaining bindings.
	  }
	Pair bind_pair = (Pair) bindings;
	Object bind_pair_car = bind_pair.car;
	SyntaxForm syntax = syntaxRest;
	if (bind_pair_car instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) bind_pair_car;
	    bind_pair_car = syntax.form;
	  }
	if (! (bind_pair_car instanceof Pair))
	  return tr.syntaxError ("let binding is not a pair:"+bind_pair_car);
	Pair binding = (Pair) bind_pair_car;
	Object name = binding.car;
	TemplateScope templateScope;
	if (name instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) name;
	    name = sf.form;
	    templateScope = sf.scope;
	  }
	else
	  templateScope = syntax == null ? null : syntax.scope;
        name = tr.namespaceResolve(name);
	if (! (name instanceof String) && ! (name instanceof Symbol))
	  return tr.syntaxError("variable "+name+" in let binding is not a symbol: "+obj);

	Declaration decl = let.addDeclaration(name);

	if (templateScope != null)
	  {
	    Declaration alias = tr.makeRenamedAlias(decl, templateScope);
	    if (renamedAliases == null)
	      renamedAliases = new Stack();
	    renamedAliases.push(alias);
	    renamedAliasesCount++;
	  }

	Object binding_cdr = binding.cdr;
	while (binding_cdr instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) binding_cdr;
	    binding_cdr = syntax.form;
	  }
	if (! (binding_cdr instanceof Pair))
	  return tr.syntaxError("let has no value for '"+name+"'");
	binding = (Pair) binding_cdr;
	binding_cdr = binding.cdr;
	Pair init;
	while (binding_cdr instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) binding_cdr;
	    binding_cdr = syntax.form;
	  }
	if (tr.matches(binding.car, "::"))
	  {
	    if (! (binding_cdr instanceof Pair)
		|| (binding = (Pair) binding_cdr).cdr == LList.Empty)
	      return tr.syntaxError("missing type after '::' in let");
	    binding_cdr = binding.cdr;
	    while (binding_cdr instanceof SyntaxForm)
	      {
		syntax = (SyntaxForm) binding_cdr;
		binding_cdr = syntax.form;
	      }
	  }
	if (binding_cdr == LList.Empty)
	  {
	    init = binding;
	  }
	else if (binding_cdr instanceof Pair)
	  {
	    decl.setType(tr.exp2Type(binding));
	    decl.setFlag(Declaration.TYPE_SPECIFIED);
	    init = (Pair) binding_cdr;
	  }
	else
	  return tr.syntaxError("let binding for '"+name+"' is improper list");
	inits[i] = tr.rewrite_car (init, syntax);
	if (init.cdr != LList.Empty)
	  return tr.syntaxError("junk after declaration of "+name);
	decl.noteValue (inits[i]);
	bindings = bind_pair.cdr;
      }

    for (int i = renamedAliasesCount;  --i >= 0; )
      tr.pushRenamedAlias((Declaration) renamedAliases.pop());

    tr.push(let);
    let.body = tr.rewrite_body(body);
    tr.pop(let);
    tr.popRenamedAlias(renamedAliasesCount);

    return let;
  }
}
