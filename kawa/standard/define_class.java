package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.expr.*;

public class define_class extends Syntax
{
  object objectSyntax;

  define_class (object objectSyntax)
  {
    this.objectSyntax = objectSyntax;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    /*
    Pair p;
    System.err.println("def_class def:"+defs+" is "+defs.getClass());
    if (! (st.cdr instanceof Pair)
        || ! ((p = (Pair) st.cdr).car instanceof String))
      return super.scanForDefinitions(st, forms, defs, tr);
    String name = (String) p.car;
    Declaration decl = new Declaration(name);
    if (defs instanceof ModuleExp)
      {
        tr.mustCompileHere();
        tr.mustCompileHere();
        tr.push(decl);
        System.err.println("def_class rewrite push "+decl);
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
    */
    return false;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
    /*
    FIXME needs work
    String name = null;
    Declaration decl = null;
    if (form.cdr instanceof Pair)
      {
        form = (Pair) form.cdr;
        System.err.println("def clas rewr "+form.car);
        if (form.car instanceof String)
          name = (String) form.car;
        else if (form.car instanceof Declaration)
          {
            decl = (Declaration) form.car;
            name = decl.getName();
          }
      }
    if (name == null)
      return tr.syntaxError("missing class name in define-class");
    //LambdaExp lexp = new LambdaExp();
    //lexp.setName(name);
    //    tr.push(lexp);
    ClassExp oexp = new ClassExp();
    oexp.setName(name);
    Expression oe = objectSyntax.rewriteClassDef((Pair) form.cdr, oexp, tr);
    // lexp.body = oe;
    // tr.pop(lexp);
    SetExp sexp = new SetExp (name, oe);
    if (decl != null)
      {
	sexp.binding = decl;
	decl.noteValue(oe);
      }
    sexp.setDefining (true);
    // sexp.binding = decl;
    // decl.noteValue (value);
    return sexp;
    */
  }
}
