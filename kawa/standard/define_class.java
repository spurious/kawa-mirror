package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.mapping.Symbol;

public class define_class extends Syntax
{
  boolean isSimple;
  object objectSyntax;

  define_class (object objectSyntax, boolean isSimple)
  {
    this.objectSyntax = objectSyntax;
    this.isSimple = isSimple;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object st_cdr = st.cdr;
    while (st_cdr instanceof SyntaxForm)
      st_cdr = ((SyntaxForm) st_cdr).form;
    if (! (st_cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st_cdr;
    Object name = p.car;
    SyntaxForm nameSyntax = null;
    while (name instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) name;
	name = nameSyntax.form;
      }
    if (! (name instanceof String || name instanceof Symbol))
      return super.scanForDefinitions(st, forms, defs, tr);
    Declaration decl = tr.define(name, nameSyntax, defs);
    if (p instanceof PairWithPosition)
      {
        PairWithPosition declPos = (PairWithPosition) p;
        decl.setFile(declPos.getFile());
        decl.setLine(declPos.getLine(), declPos.getColumn());
      }
    ClassExp oexp = new ClassExp();
    oexp.setSimple(isSimple);
    decl.noteValue(oexp);
    if (isSimple)
      decl.setFlag(Declaration.STATIC_SPECIFIED);
    decl.setFlag(Declaration.IS_CONSTANT);
    decl.setType(Compilation.typeClassType);
    tr.mustCompileHere();
    Object members = p.cdr;
    while (members instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) members;
	members = nameSyntax.form;
      }
    if (! (members instanceof Pair))
      {
	tr.error('e', "missing class members");
	return false;
      }
    p = (Pair) members;
    ScopeExp save_scope = tr.currentScope();
    if (nameSyntax != null)
      tr.setCurrentScope(nameSyntax.scope);
    Object[] saved = objectSyntax.scanClassDef(p, oexp, tr);
    if (nameSyntax != null)
      tr.setCurrentScope(save_scope);
    if (saved == null)
	return false;
    st = Translator.makePair(st, this, Translator.makePair(p, decl, saved));
    forms.addElement (st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    //FIXME needs work
    Object symbol = null;
    Declaration decl = null;
    if (form.cdr instanceof Pair)
      {
        form = (Pair) form.cdr;
	if (! (form.car instanceof Declaration))
	  return tr.syntaxError(this.getName() + " can only be used in <body>");
	decl = (Declaration) form.car;
	symbol = decl.getSymbol();
      }
    if (symbol == null)
      return tr.syntaxError("missing class name in "+this.getName());
    String name = symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
    ClassExp oexp = (ClassExp) decl.getValue();
    int nlen = name.length();
    String cname
      = (nlen > 2 && name.charAt(0) == '<' && name.charAt(nlen-1) == '>'
	 ? name.substring(1, nlen-1)
	 : name);
    oexp.setName(cname);
    objectSyntax.rewriteClassDef((Object[]) form.cdr, tr);
    SetExp sexp = new SetExp (symbol, oexp);
    sexp.binding = decl;
    sexp.setDefining (true);
    return sexp;
  }
}
