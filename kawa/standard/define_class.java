package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;

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
    Pair p;
    if (! (st.cdr instanceof Pair)
        || ! ((p = (Pair) st.cdr).car instanceof String))
      return super.scanForDefinitions(st, forms, defs, tr);
    String name = (String) p.car;
    Declaration decl = new Declaration(name);
    ClassExp oexp = new ClassExp();
    decl.noteValue(oexp);
    if (isSimple)
      decl.setFlag(Declaration.STATIC_SPECIFIED);
    decl.setFlag(Declaration.IS_CONSTANT);
    decl.setType(Compilation.typeClassType);
    if (defs instanceof ModuleExp)
      {
        tr.mustCompileHere();
        tr.mustCompileHere();
        tr.push(decl);
      }
    Pair declForm = tr.makePair(p, decl, p.cdr);
    if (declForm instanceof PairWithPosition)
      {
        PairWithPosition declPos = (PairWithPosition) declForm;
        decl.setFile(declPos.getFile());
        decl.setLine(declPos.getLine(), declPos.getColumn());
      }
    defs.addDeclaration(decl);
    st = tr.makePair(st, this, declForm);
    forms.addElement (st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    //FIXME needs work
    String name = null;
    Declaration decl = null;
    if (form.cdr instanceof Pair)
      {
        form = (Pair) form.cdr;
        if (form.car instanceof String)
          name = (String) form.car;
        else if (form.car instanceof Declaration)
          {
            decl = (Declaration) form.car;
            name = decl.getName();
          }
      }
    if (name == null)
      return tr.syntaxError("missing class name in "+this.getName());
    //LambdaExp lexp = new LambdaExp();
    //lexp.setName(name);
    //    tr.push(lexp);
    ClassExp oexp = (ClassExp) decl.getValue();
    oexp.setSimple(isSimple);
    int nlen = name.length();
    String cname
      = (nlen > 2 && name.charAt(0) == '<' && name.charAt(nlen-1) == '>'
	 ? name.substring(1, nlen-1)
	 : name);
    oexp.setName(cname);
    Expression oe = objectSyntax.rewriteClassDef((Pair) form.cdr, oexp, tr);
    // lexp.body = oe;
    // tr.pop(lexp);
    SetExp sexp = new SetExp (name, oe);
    sexp.binding = decl;
    sexp.setDefining (true);
    // sexp.binding = decl;
    // decl.noteValue (value);
    return sexp;
  }
}
