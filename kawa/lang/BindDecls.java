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
    public static final BindDecls instance = new BindDecls();

    public boolean allowShadowing = false;

    public boolean makeConstant = true;

    static final Symbol underScoreSymbol = Symbol.valueOf("_");

    public Declaration define(Symbol name, SyntaxForm nameSyntax,
                              ScopeExp scope, Translator comp) {
        Declaration oldDecl = comp.lexical.lookup(name, false);
        Declaration decl = comp.define(name, nameSyntax, scope);
        if (! allowShadowing
            && oldDecl != null && oldDecl.context != scope) {
            comp.error('w', decl, "new declaration '", "' shadows old declaration");
            comp.error('w', oldDecl, "(this is the previous declaration of '", "')");
        }
        return decl;
    }

    /** Parse a declaration or more generally a pattern.
     * The actual pattern is an initial sublist (usuing just the initial
     * car) of the patList.
     * @return A 2-element array, where element 0 is the unused remainder
     *   of patList, while element 1 is a Declaration for that pattern.
     */
    public Object[] parsePatternCar(Pair patList, int scanNesting,
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
                decl = define((Symbol) patval, nameSyntax, scope, comp);
                Translator.setLine(decl, patList);
            }
            if (scope instanceof ModuleExp
                && (patval == underScoreSymbol
                    || ! scope.getFlag(ModuleExp.INTERACTIVE)))
                decl.setPrivate(true);
            if (makeConstant)
                decl.setFlag(Declaration.IS_CONSTANT);
            decl.setFlag(Declaration.IS_SINGLE_VALUE);
        }
        else if (pattern instanceof Pair) {
            Pair patpair = (Pair) pattern;
            Object patcar = patpair.getCar();
            if (patcar == LispLanguage.bracket_list_sym) {
                decl = scope.addDeclaration((Object) null);
                if (type != null)
                    ; // FIXME
                decl.setPrivate(true);
                decl.setFlag(Declaration.IS_CONSTANT);
                decl.setFlag(Declaration.IS_SINGLE_VALUE);
                parseBracketListPattern(patpair, scanNesting, scope, decl, comp);
            }
            /*else if (patcar == LispLanguage.bracket_quote_sym)
              ....
            */
            else
                comp.syntaxError("unrecognized pattern operator "+patcar);
        }
        else
            comp.error('e', "unrecognized pattern "+pattern);
        if (decl != null)
            decl.setScanNesting(scanNesting);
        if (type != null && decl != null) {
            decl.setType(type);
            decl.setFlag(Declaration.TYPE_SPECIFIED);
        }
        comp.popPositionOf(saveLoc);
        return new Object[]{next,decl};
    }

    /** Handle patterns of the form {@code [pat1 ... patN]}.
     */
    public void parseBracketListPattern
        (Pair patpair, int scanNesting, ScopeExp scope, Declaration decl, Translator comp) {
        ClassType listType = ClassType.make("java.util.List");
        decl.setFlag(Declaration.SKIP_FOR_METHOD_PARAMETER);
        if (decl.getTypeExpRaw() != null) {
            Declaration d = scope.addDeclaration((Object) null);
            d.setFlag(Declaration.PATTERN_NESTED|Declaration.SKIP_FOR_METHOD_PARAMETER);
            d.setScanNesting(scanNesting);
            setInitializer(d, new ReferenceExp(decl), scope, comp);
            decl = d;
        }
        int count = 0;
        Object cdr = patpair.getCdr();
        int ellipsisCount = 0;
        for (;; count++) {
            if (cdr == LList.Empty)
                break;
            if (! (cdr instanceof Pair))
                break;  // FIXME ERROR - or handle "rest" pattern
            patpair = (Pair) cdr;
            boolean sawEllipsis = false;
            int curScanNesting = scanNesting;
            if (patpair.getCdr() instanceof Pair) {
                Object nextCar = ((Pair) patpair.getCdr()).getCar();
                Object ellipsis = SyntaxRule.dots3Symbol;
                if (SyntaxPattern.literalIdentifierEq(nextCar, null/*FIXME*/, ellipsis, null)) {
                    sawEllipsis = true;
                    curScanNesting++;
                    if (ellipsisCount > 0)
                        comp.error('e', "multiple '...' in pattern");
                    ellipsisCount++;
                }
            }
            Object[] r = parsePatternCar(patpair, curScanNesting,
                                         scope, comp);
            cdr = sawEllipsis ?  ((Pair) patpair.getCdr()).getCdr() : r[0];
            Declaration d = (Declaration) r[1];
            d.setScanNesting(curScanNesting);
            d.setFlag(Declaration.PATTERN_NESTED|Declaration.SKIP_FOR_METHOD_PARAMETER);
            Expression init;
            if (sawEllipsis) {
                int restCount = Translator.listLength(cdr);
                Method dropMethod = ClassType.make("gnu.lists.Sequences")
                    .getDeclaredMethod("drop", restCount==0 ? 2 : 3);
                Expression[] args = new Expression[restCount==0 ? 2 : 3];
                args[0] = new ReferenceExp(decl);
                args[1] = new QuoteExp(count, Type.intType);
                if (restCount != 0)
                    args[2] = new QuoteExp(restCount, Type.intType);
                init = new ApplyExp(dropMethod, args);
            } else {
                // FIXME Probably better to use an Iterator or "position indexes"
                Method indexMethod = listType
                    .getMethod("get", new Type[] { Type.intType  });
                init = new ApplyExp(indexMethod, new Expression[] {
                        new ReferenceExp(decl),
                        new QuoteExp(count, Type.intType) });
            }
            setInitializer(d, init, scope, comp);
        }
        Type type = new SeqSizeType(count-ellipsisCount, ellipsisCount==0,
                                    "java.util.List");
        decl.setType(type);
    }

    private void setInitializer(Declaration decl, Expression init, ScopeExp scope, Translator comp) {
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
