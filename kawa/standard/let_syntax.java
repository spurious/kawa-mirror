package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.Stack;

/** Implementation of the standard Scheme let-syntax and letrec-syntax forms.
 * Not quite working yet. */

public class let_syntax extends Syntax
{
  public static final let_syntax let_syntax
    = new let_syntax(false, "let-syntax");
  public static final let_syntax letrec_syntax
    = new let_syntax(true, "letrec-syntax");

  boolean recursive;

  public let_syntax(boolean recursive, String name)
  {
    super(name);
    this.recursive = recursive;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let-syntax arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = Translator.listLength(bindings);
    if (decl_count < 0)
      return tr.syntaxError("bindings not a proper list");
    Stack renamedAliases = null;
    int renamedAliasesCount = 0;
    Expression[] inits = new Expression[decl_count];
    Declaration[] decls = new Declaration[decl_count];
    Macro[] macros = new Macro[decl_count];
    Pair[] transformers = new Pair[decl_count];
    SyntaxForm[] trSyntax = new SyntaxForm[decl_count];
    LetExp let = new LetExp (inits);
    SyntaxForm listSyntax = null;
    for (int i = 0; i < decl_count; i++)
      {
	while (bindings instanceof SyntaxForm)
	  {
	    listSyntax = (SyntaxForm) bindings;
	    bindings = listSyntax.form;
	  }
	SyntaxForm bindingSyntax = listSyntax;
	Pair bind_pair = (Pair) bindings;
	Object bind_pair_car = bind_pair.car;
	if (bind_pair_car instanceof SyntaxForm)
	  {
	    bindingSyntax = (SyntaxForm) bind_pair_car;
	    bind_pair_car = bindingSyntax.form;
	  }
	if (! (bind_pair_car instanceof Pair))
	  return tr.syntaxError (getName()+" binding is not a pair");
	Pair binding = (Pair) bind_pair_car;
	Object name = binding.car;
	SyntaxForm nameSyntax = bindingSyntax;
	while (name instanceof SyntaxForm)
	  {
	    nameSyntax = (SyntaxForm) name;
	    name = nameSyntax.form;
	  }
	if (! (name instanceof String || name instanceof Symbol))
	  return tr.syntaxError("variable in "+getName()+" binding is not a symbol");
	Object binding_cdr = binding.cdr;
	while (binding_cdr instanceof SyntaxForm)
	  {
	    bindingSyntax = (SyntaxForm) binding_cdr;
	    binding_cdr = bindingSyntax.form;
	  }
	if (! (binding_cdr instanceof Pair))
	  return tr.syntaxError(getName()+" has no value for '"+name+"'");
	binding = (Pair) binding_cdr;
	if (binding.cdr != LList.Empty)
	  return tr.syntaxError("let binding for '"+name+"' is improper list");
	Declaration decl = new Declaration(name);
        Macro macro = Macro.make(decl);
        macros[i] = macro;
	transformers[i] = binding;
	trSyntax[i] = bindingSyntax;
        let.addDeclaration(decl);
	ScopeExp templateScope = nameSyntax == null ? null : nameSyntax.scope;
	if (templateScope != null)
	  {
	    Declaration alias = tr.makeRenamedAlias(decl, templateScope);
	    if (renamedAliases == null)
	      renamedAliases = new Stack();
	    renamedAliases.push(alias);
	    renamedAliasesCount++;
	  }
        macro.setCapturedScope(bindingSyntax != null ? bindingSyntax.scope
                               : recursive ? let : tr.currentScope());
        decls[i] = decl;
	inits[i] = QuoteExp.nullExp;
	bindings = bind_pair.cdr;
      }
    if (recursive)
      push(let, tr, renamedAliases);
    Macro savedMacro = tr.currentMacroDefinition;
    for (int i = 0; i < decl_count; i++)   
      {
        Macro macro = macros[i];
	tr.currentMacroDefinition = macro;
        Expression value = tr.rewrite_car(transformers[i], trSyntax[i]);
        inits[i] = value;
        Declaration decl = decls[i];
        macro.expander = value;
        decl.noteValue(new QuoteExp(macro));
        if (value instanceof LambdaExp)
          {
            LambdaExp lvalue = (LambdaExp) value;
            lvalue.nameDecl = decl;
            lvalue.setSymbol(decl.getSymbol());
          }
      }
    tr.currentMacroDefinition = savedMacro;
    if (! recursive)
      push(let, tr, renamedAliases);
    Expression result = tr.rewrite_body(body);
    tr.pop(let);
    tr.popRenamedAlias(renamedAliasesCount);
    return result;
  }

  private void push (LetExp let, Translator tr, Stack renamedAliases)
  {
    tr.push(let);
    if (renamedAliases != null)
      for (int i = renamedAliases.size();  --i >= 0; )
	tr.pushRenamedAlias((Declaration) renamedAliases.pop());
  }
}
