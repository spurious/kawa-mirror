package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax {
    public static final define_alias define_alias = new define_alias();
    static { define_alias.setName("define-alias"); }

    public void scanForm (Pair st, ScopeExp defs, Translator tr) {
        Object formCdr = st.getCdr();
        SyntaxForm formSyntax = null;
        while (formCdr instanceof SyntaxForm) {
            formSyntax = (SyntaxForm) formCdr;
            formCdr = formSyntax.getDatum();
        }
        if (formCdr instanceof Pair) {
            Pair p1 = (Pair) formCdr;
            SyntaxForm nameSyntax = formSyntax;
            Object name = p1.getCar();
            while (name instanceof SyntaxForm) {
                nameSyntax = (SyntaxForm) name;
                name = nameSyntax.getDatum();
            }
            Object f2 = p1.getCdr();
            while (f2 instanceof SyntaxForm) {
                formSyntax = (SyntaxForm) f2;
                f2 = formSyntax.getDatum();
            }
            Pair p2;
            if ((name instanceof String || name instanceof Symbol)
                && f2 instanceof Pair
                && (p2 = (Pair) f2).getCdr() == LList.Empty) {
                Declaration decl = tr.define(name, nameSyntax, defs);
                decl.setIndirectBinding(true);
                decl.setAlias(true);
                if (formSyntax != null)
                    f2 = SyntaxForms.makeForm(f2, formSyntax.getScope());
                tr.formStack.addElement(Translator.makePair(st, this,
                        Translator.makePair(p1, decl, f2)));
                return;
            }
        }
        tr.error('e', "invalid syntax for define-alias");
    }

    public Expression rewriteForm (Pair form, Translator tr) {
        Object f1 = form.getCdr();
        if (f1 instanceof Pair) {
            Pair p1 = (Pair) f1;
            Object f2 = p1.getCar();
            if (f2 instanceof Declaration) {
                Declaration decl = (Declaration) f2;
                Expression arg = tr.rewrite_car((Pair) p1.getCdr(), false);
                if (arg instanceof ReferenceExp) {
                    ReferenceExp rarg = (ReferenceExp) arg;
                    Declaration d = Declaration.followAliases(rarg.getBinding());
                    Expression dval;
                    if (d != null
                        && ((dval = d.getValue()) instanceof ClassExp
                            || dval instanceof ModuleExp)) {
                        decl.setIndirectBinding(false);
                        decl.setFlag(Declaration.IS_CONSTANT);
                    }
                    else
                        rarg.setDontDereference(true);
                }
                else if (arg instanceof QuoteExp) {
                    decl.setIndirectBinding(false);
                    decl.setFlag(Declaration.IS_CONSTANT);
                } else {
                    arg = location.rewrite(arg, tr);
                    decl.setType(Compilation.typeLocation);
                }
                decl.setFlag(Declaration.EARLY_INIT);
                tr.mustCompileHere(); // For simplicity.
                tr.push(decl);
                SetExp sexp = new SetExp(decl, arg);
                tr.setLineOf(sexp);
                decl.noteValueFromSet(sexp);
                sexp.setDefining (true);
                return sexp;
            }
        }
        return tr.syntaxError ("define-alias is only allowed in a <body>");
    }
}
