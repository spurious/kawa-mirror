package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.mapping.Symbol;

public class define_class extends Syntax
{
  public static final define_class define_class
    = new define_class("define-class", false);
  public static final define_class define_simple_class
    = new define_class("define-simple-class", true);

  boolean isSimple;
  object objectSyntax;

  define_class (object objectSyntax, boolean isSimple)
  {
    this.objectSyntax = objectSyntax;
    this.isSimple = isSimple;
  }

  define_class (String name, boolean isSimple)
  {
    super(name);
    this.objectSyntax = object.objectSyntax;
    this.isSimple = isSimple;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object st_cdr = st.getCdr();
    SyntaxForm nameSyntax = null;
    while (st_cdr instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) st_cdr;
	st_cdr = nameSyntax.getDatum();
      }
    if (! (st_cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st_cdr;
    Object name = p.getCar();
    while (name instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) name;
	name = nameSyntax.getDatum();
      }
    name = tr.namespaceResolve(name);
    if (! (name instanceof String || name instanceof Symbol))
      {
	tr.error('e', "missing class name");
	return false;
      }
    Declaration decl = tr.define(name, nameSyntax, defs);
    if (p instanceof PairWithPosition)
      decl.setLocation((PairWithPosition) p);
    ClassExp oexp = new ClassExp(isSimple);
    decl.noteValue(oexp);
    decl.setFlag(Declaration.IS_CONSTANT|Declaration.EARLY_INIT);
    decl.setType(isSimple ? Compilation.typeClass : Compilation.typeClassType);
    tr.mustCompileHere();

    String cname = name instanceof Symbol ? ((Symbol) name).getName()
      : name.toString();
    int nlen = cname.length();
    if (nlen > 2 && cname.charAt(0) == '<' && cname.charAt(nlen-1) == '>')
      cname = cname.substring(1, nlen-1);
    oexp.setName(cname);

    Object members = p.getCdr();
    while (members instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) members;
	members = nameSyntax.getDatum();
      }
    if (! (members instanceof Pair))
      {
	tr.error('e', "missing class members");
	return false;
      }
    p = (Pair) members;
    ScopeExp save_scope = tr.currentScope();
    if (nameSyntax != null)
      tr.setCurrentScope(nameSyntax.getScope());
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
    Declaration decl = null;
    Object form_cdr = form.getCdr();
    if (form_cdr instanceof Pair)
      {
        form = (Pair) form_cdr;
        Object form_car = form.getCar();
	if (! (form_car instanceof Declaration))
	  return tr.syntaxError(this.getName() + " can only be used in <body>");
	decl = (Declaration) form_car;
      }
    ClassExp oexp = (ClassExp) decl.getValue();
    objectSyntax.rewriteClassDef((Object[]) form.getCdr(), tr);
    SetExp sexp = new SetExp(decl, oexp);
    sexp.setDefining (true);
    return sexp;
  }
}
