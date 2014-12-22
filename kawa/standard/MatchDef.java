package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/** Handle the {@code (! pattern init)} syntax. */

public class MatchDef extends Syntax {
    public static final MatchDef matchDef = new MatchDef();

    public void scanForm (Pair st, ScopeExp defs, Translator tr) {
        Object arg = st.getCdr();
        if (! (arg instanceof Pair)) {
            tr.error('e', "missing pattern following '!'");
            return;
        }
        Pair p1 = (Pair) arg;
        SetExp sexp = new SetExp(null, null);
        st = Translator.makePair(st, this,  sexp);

        tr.pushForm(st);
        Object[] r = BindDecls.parsePatternCar(p1, defs, tr);
        Object rest = r[0];
        Declaration decl = (Declaration) r[1];
        sexp.setBinding(decl);
        Expression init;
        if (rest instanceof Pair) {
            Pair prest = (Pair) rest;
            if (prest.getCdr() != LList.Empty) {
                tr.error('e', "junk after initializer");
            }
            init = new LangExp(rest);
        }
        else {
             tr.error('e', "missing initializer");
             init = QuoteExp.nullExp;
        }
        sexp.setNewValue(init);
    }

    public Expression rewriteForm (Pair form, Translator tr) {
        Object arg = form.getCdr();
        if (! (arg instanceof SetExp))
            return tr.syntaxError("! definition is only allowed in a <body>");
        SetExp sexp = (SetExp) arg;
   
        Declaration decl = sexp.getBinding();
        Expression init = sexp.getNewValue();
        if (init instanceof LangExp) { // FIXME check for bad syntax
            init = tr.rewrite_car((Pair) ((LangExp) init).getLangValue(), false);
            sexp.setNewValue(init);
        }
        decl.noteValueFromSet(sexp);
        
        sexp.setDefining (true);
        return sexp;
    }
}
