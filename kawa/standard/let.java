package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import java.util.ArrayList;
import java.util.Stack;

/**
 * The Syntax transformer that re-writes the Scheme "let" primitive.
 * This only handles standard "unnamed" let.
 * The let macro in ../lib/let.scm handles named let as well.
 * @author	Per Bothner
 */

public class let extends Syntax {

    public static final let let = new let("let", false);

    /**
     * Used for constructs such as FLET, where we intend to set a
     * function binding rather than an ordinary binding.
     */
    protected boolean settingProcedures;
  
    public let(String name, boolean settingProcedures) {
        this.setName(name);
        this.settingProcedures = settingProcedures;
    }

    @Override
    public Expression rewrite (Object obj, Translator tr) {
        final ArrayList<Declaration> decls = new ArrayList<Declaration>();
        final Stack<Declaration> renamedAliases = new Stack<Declaration>();
        // Used to check for duplicate definitions.
        final SimpleEnvironment dupenv = new SimpleEnvironment();
        final LetExp let = new LetExp();

        BindDecls bindDecls =
            new BindDecls() {
                @Override
                public Declaration define(Symbol name, SyntaxForm nameSyntax,
                                          ScopeExp defs, Translator comp) {
                   ScopeExp templateScope = nameSyntax == null ? null
                       : nameSyntax.getScope();
                    Declaration decl = new Declaration(name);
                    Object old = dupenv.get(name, templateScope, null);
                    if (old != null)
                        ScopeExp.duplicateDeclarationError((Declaration) old,
                                                           decl, tr);
                    dupenv.put(name, templateScope, decl);
                    let.add(decl);
                    decl.setFlag(Declaration.IS_SINGLE_VALUE);
                    if (templateScope != null) {
                        renamedAliases.push
                            (tr.makeRenamedAlias(decl, templateScope));
                    }
                    return decl;
                }
            };
        bindDecls.allowShadowing = true;
        bindDecls.makeConstant = false;
    
        if (! (obj instanceof Pair))
            return tr.syntaxError ("missing " + getName() + " arguments");
        Pair pair = (Pair) obj;
        Object bindings = pair.getCar();
        Object body = pair.getCdr();

        for (;;) {
            if (bindings == LList.Empty)
                break;
            if (! (bindings instanceof Pair))
                return tr.syntaxError("bindings not a proper list");
            Pair bind_pair = (Pair) bindings;
            Object bind_pair_car = bind_pair.getCar();
            if (! (bind_pair_car instanceof Pair))
                return tr.syntaxError (getName() + 
                                       " binding is not a pair:"+bind_pair_car);
            Pair binding = (Pair) bind_pair_car;
            Object saveLoc1 = tr.pushPositionOf(binding);
            Object[] r = bindDecls.parsePatternCar(binding, let, tr);
            Object binding_cdr = r[0];
            Declaration decl = (Declaration) r[1];
            maybeSetProcedure(decl);
            Object name = decl.getSymbol();
        
            if (binding_cdr instanceof Pair) {
                Pair init = (Pair) binding_cdr;
                binding_cdr = init.getCdr();

                Expression initExp = tr.rewrite_car(init, null);
                decl.setInitValue(initExp);
                if (initExp != QuoteExp.undefined_exp)
                    decl.noteValueFromLet(let);

                if (init.getCdr() != LList.Empty) {
                    Object saveLoc2 = tr.pushPositionOf(init.getCdr());
                    tr.error('e', "junk after initializer");
                    tr.popPositionOf(saveLoc2);
                }
            } else {
                tr.error('e', "let has no initializer");
            }
            tr.popPositionOf(saveLoc1);
            bindings = bind_pair.getCdr();
        }

        int renamedAliasesCount = renamedAliases.size();
        for (int i = renamedAliasesCount;  --i >= 0; )
            tr.pushRenamedAlias(renamedAliases.pop());

        tr.push(let);
        let.setBody(tr.rewrite_body(body));

        tr.pop(let);
        tr.popRenamedAlias(renamedAliasesCount);
    
        return let;
    }

    /**
     * Set the procedure flag of a declaration if binding a function property.
     * 
     * This is used for FLET .vs. LET distinction, where {@code settingProcedures}
     * is true for FLET, and false for LET.
     * 
     * @param decl The declaration to possibly set the {@code PROCEDURE} flag.
     */
    protected void maybeSetProcedure (Declaration decl)
    {
        if (settingProcedures)
            decl.setProcedureDecl(true); 
    }
}
