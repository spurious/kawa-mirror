package kawa.lang;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.lists.*;
import java.util.Stack;
import java.util.Vector;
import gnu.mapping.Environment;
import gnu.mapping.Symbol;
import gnu.kawa.lispexpr.SeqSizeType;
import gnu.kawa.lispexpr.LispLanguage;

/** Methods for parsing patterns. */

public class BindDecls {
    static final Symbol underScoreSymbol = Symbol.valueOf("_");

    public static Object[] parsePatternCar(Pair patList,
                                         ScopeExp scope,
                                         Translator comp) {
        Object next = patList.getCdr();
        Type type = null;
        if (next instanceof Pair) {
            Pair nextPair = (Pair) next;
            if (comp.matches(nextPair.getCar(), "::")) {
                Object nextCdr = nextPair.getCdr();
                if (nextCdr instanceof Pair) {
                    Pair nextCdrPair = (Pair) nextCdr;
                    type = comp.exp2Type(nextCdrPair);
                    next = nextCdrPair.getCdr();
                }
                else {
                    Object saveLoc = comp.pushPositionOf(nextPair);
                    comp.error('e', "missing type after '::'");
                    comp.popPositionOf(saveLoc);
                    next = nextCdr;
                }
            }
        }
        Object pattern = patList.getCar();
        Object saveLoc = comp.pushPositionOf(patList);

        Object patval = pattern;
        SyntaxForm nameSyntax = null;
        if (patval instanceof SyntaxForm) {
            nameSyntax = (SyntaxForm) patval;
            patval = nameSyntax.getDatum();
        }
        patval = comp.namespaceResolve(patval);
        Declaration decl = null;
        if (patval instanceof Symbol) {
            if (patval == underScoreSymbol) {
                decl = scope.addDeclaration((Object) null);
            } else {
                Declaration oldDecl = comp.lexical.lookup(patval, false);
                decl = comp.define(patval, nameSyntax, scope);
                if (oldDecl != null && oldDecl.context != scope) {
                    comp.error('w', decl, "new declaration '", "' shadows old declaration");
                    comp.error('w', oldDecl, "(this is the previous declaration of '", "')");
                }
                Translator.setLine(decl, patList);
            }
            if (scope instanceof ModuleExp
                && (patval == underScoreSymbol
                    || ! scope.getFlag(ModuleExp.INTERACTIVE)))
                decl.setPrivate(true);
            decl.setFlag(Declaration.IS_CONSTANT);
            decl.setFlag(Declaration.IS_SINGLE_VALUE);
        }
        else if (pattern instanceof Pair) {
            Pair patpair = (Pair) pattern;
            Object patcar = patpair.getCar();
            if (patcar == LispLanguage.bracket_list_sym) {
                decl = scope.addDeclaration((Object) null);
                decl.setPrivate(true);
                decl.setFlag(Declaration.IS_CONSTANT);
                decl.setFlag(Declaration.IS_SINGLE_VALUE);
                parseBracketListPattern(patpair, scope, decl, comp);
            }
            /*else if (patcar == LispLanguage.bracket_quote_sym)
              ....
            */
            else
                comp.syntaxError("unrecognized pattern operator "+patcar);
        }
        else
            comp.error('e', "unrecognized pattern "+pattern);
        if (type != null && decl != null) {
            decl.setType(type);
            decl.setFlag(Declaration.TYPE_SPECIFIED);
        }
        comp.popPositionOf(saveLoc);
        return new Object[]{next,decl};
    }

    /** Handle patterns of the form {@code [pat1 ... patN]}.
     */
    public static void parseBracketListPattern
        (Pair patpair, ScopeExp scope, Declaration decl, Translator comp) {
        ClassType listType = ClassType.make("java.util.List");
        decl.setFlag(Declaration.SKIP_FOR_METHOD_PARAMETER);
        if (decl.getTypeExpRaw() != null) {
            Declaration d = scope.addDeclaration((Object) null);
            d.setFlag(Declaration.PATTERN_NESTED|Declaration.SKIP_FOR_METHOD_PARAMETER);
            setInitializer(d, new ReferenceExp(decl), scope, comp);
            decl = d;
        }
        int count = 0;
        Object cdr = patpair.getCdr();
        for (;; count++) {
            if (cdr == LList.Empty)
                break;
            if (! (cdr instanceof Pair))
                break;  // FIXME ERROR - or handle "rest" pattern
            patpair = (Pair) cdr;
            Object[] r = parsePatternCar(patpair, scope, comp);
            cdr = r[0];
            Declaration d = (Declaration) r[1];
            d.setFlag(Declaration.PATTERN_NESTED|Declaration.SKIP_FOR_METHOD_PARAMETER);
            // FIXME Probably better to use an Iterator or "position indexes"
            Method indexMethod = listType
                .getMethod("get", new Type[] { Type.intType  });
            Expression init = new ApplyExp(indexMethod, new Expression[] {
                    new ReferenceExp(decl),
                    new QuoteExp(Integer.valueOf(count), Type.intType) });
            setInitializer(d, init, scope, comp);
        }
        Type type = new SeqSizeType("list#"+count, "java.util.List",
                                    count, true);
        decl.setType(type);
    }

    private static void setInitializer(Declaration decl, Expression init, ScopeExp scope, Translator comp) {
        if ((scope instanceof ModuleExp)
            // scope is a <body> (artificial LetExp allocated in rewrite_body)
            || (scope instanceof LetExp && scope.firstDecl() == null)) {
            SetExp sexp = new SetExp(decl, init);
            comp.pushForm(sexp);
            decl.noteValueFromSet(sexp);
        }
        else {
            decl.setInitValue(init);
            decl.noteValueFromLet(scope);
        }
    }
}
