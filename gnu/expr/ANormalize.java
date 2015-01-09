package gnu.expr;

import gnu.bytecode.ClassType;
import gnu.bytecode.Type;
import gnu.kawa.functions.GetNamedPart;
import gnu.kawa.functions.MakeSplice;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.kawa.reflect.TypeSwitch;
import java.util.Vector;

/**
 * A visitor that performs transformation to Administrative Normal Form.
 *
 * This is an adaptation and extension of the A-normalization algoritm
 * described in the paper "The Essence of Compiling with Continuations" 
 * by Flanagan et al. and in "A-Normalization: Why and How" by Matt Might
 * (http://matt.might.net/articles/a-normalization/). 
 * 
 * The algoritm performs a monadic transformation combining three steps:
 * <blockquote>
 * 1. Monadic conversion:
 * <pre>
 * 
 *    (+ 1                             (bind (if (>= x 0)
 *       (if (>= x 0)                            (f x)
 *           (f x)             -->               (return 0))
 *           0))                             (lambda (t) (+ 1 t)))
 * 
 * </pre>
 * 2. The result is interpreted in the Identity monad:
 * <pre>
 * 
 *                 (return a)  =>  a
 * 
 *    (bind a (lambda (x) b))  =>  (let ((x a)) b)
 * 
 * 
 *    (bind (if (>= x 0)               (let ((t (if (>= x 0)
 *              (f x)          -->                  (f x)
 *              (return 0))                         (return 0))))
 *           (lambda (t) (+ 1 t)))        (+ 1 t)) 
 * 
 * </pre>
 * 3. Nested let are flattened:
 * <pre>
 * 
 *    (let ((x (let ((y a))               (let ((y a))
 *               b)))          -->          (let ((x b))
 *      c)                                    c))
 * 
 * </pre>
 * </blockquote>
 * In the actual Java code "return" operation is called "identity", while the 
 * "bind" operation is called "normalizeName" as in the Flanagan et al. paper.
 * The ExpVisitor type matching mechanism replaces the role of the "match" in 
 * the paper, while the Context class replaces the "k" parameter. Lambdas are 
 * simulated with classes for backward compatibility with Java version 7 and 
 * lower.
 * 
 * Each visit[...]Exp method is called with two parameters, an expression and
 * a context (the very first context is the identity function that returns 
 * its argument). If the visit method is called with a non-atomic expression 
 * a new context is created and the passed context is called only inside the
 * new one. The new-context is then passed to "normalizeName"  that has two 
 * purposes: 
 * 
 * 1. to create a context, that generates a "let" expression to let-bind
 *    the expression next to come in the "visiting" process; 
 * 2. to call visit() on the passed expression, to continue the syntax
 *    tree traversing.
 * 
 * This chain will finish when a leaf (an atomic expression) is encountered
 * in the tree, in this case the passed context is invoked (which in turn
 * will invoke the previous context and so on). At this point the chain of 
 * context invocations starts to wrap each expression in a "let" binding, 
 * processing the expressions backward, and enclosing them step by step in 
 * nested "let" expressions. This backward traversing stops when the context
 * called is the identity function.
 * 
 * When the expression to normalize is a conditional, "normalizeTerm" is used
 * on each branch expression. Instead of creating a let binding for each branch, 
 * as they cannot be evaluated before the test outcome, "normalizeTerm" calls 
 * the visit method with the identity context, restarting the normalization in
 * each branch.
 */
public class ANormalize extends ExpExpVisitor<ANormalize.Context> {
  
    static final Context identity = new Context();
    int varCount;
    
    /* Context accepting a single expression */
    static class Context {
        Expression invoke (Expression expr) { return expr; };
    }

    /* Context accepting multiple expressions */
    static abstract class MultiContext {
        abstract Expression invoke (Expression[] exprs, int index);
    }    
    
    public static void aNormalize(Expression exp, Compilation comp) {
        ANormalize visitor = new ANormalize();
        visitor.setContext(comp);
        
        // Starts the normalization of expression exp. It has the effect of 
        // monadic conversion plus interpreting it in the Identity monad.
        visitor.visit(exp, identity);
    }
    
    /** 
     * Generates a new Declaration, assigns the Expression val to 
     * it and adds it to the provided let Expression. If an already
     * existing Declaration is passed, it will be updated with the 
     * new value and added to the let.
     */
    Declaration genLetDeclaration(Expression val, LetExp let,
                                         Declaration providedDecl) { 
        boolean isProvided = providedDecl != null;
        
        // create a new name for the binding, or get the passed one
        Declaration decl = (isProvided)
                           ? providedDecl 
                           : new Declaration((Object)null);
        
        let.add(decl);
        decl.setInitValue(val);
        decl.setFlag(Declaration.IS_SINGLE_VALUE);
        decl.setNext(null);
        
        if (!isProvided) {
            // set the properties for the newly generated Declaration
          
            if (!(val instanceof ClassExp) 
                && val instanceof LambdaExp) {
                decl.setCanCall();
                if (let instanceof FluidLetExp)
                    decl.setProcedureDecl(true);
                decl.setType(Compilation.typeProcedure);
            }

            if (val != QuoteExp.undefined_exp)
                decl.noteValueFromLet(let);

        } else {

            // if decl has ben provided, we need to update its base,
            // because we are moving it in a different let
            for (int i = 0; i < decl.nvalues; i++) {
                if (decl.values[i].kind 
                    == Declaration.ValueSource.LET_INIT_KIND) {
                    decl.values[i].base = let;
                }
            }
        }   

        if (val == QuoteExp.undefined_exp)
            decl.setFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS);
        if (val == QuoteExp.undefined_exp
            || (val instanceof SetExp)) {
            // in this case the declaration is ignorable
            decl.setCanRead(false);
            decl.setCanWrite(false);
        }        
        
        return decl;
    }
    
    Declaration genLetDeclaration(Expression val, LetExp let) {
        return genLetDeclaration(val, let, null);
    }    
        
    /**
     * Starts the normalization of expression exp. It has the effect of monadic 
     * conversion plus interpreting it in the Identity monad.
     */
    protected Expression normalizeTerm(Expression exp) {
        return visit(exp, identity);
    }
    
    /**
     * Determines if an Expression is atomic, that is if it needs to
     * be normalized or not.
     */
    protected static boolean isAtomic(Expression exp) {
        return (exp instanceof QuoteExp)
            || (exp instanceof ReferenceExp)
            || (MakeSplice.argIfSplice(exp) != null)
            || isGetNamedPart(exp)
            || isBracketList(exp)
            || isTypeSwitch(exp);
    }

    /**
     * Performs the bind operation, introducing a new let expression
     * to capture the result of non-trivial expressions, which is bound
     * to a new let variable.
     */
    protected Expression normalizeName(Expression exp, final Context context) {
        Context newContext = new Context() {
            @Override
            Expression invoke(Expression expr) {
                if (isAtomic(expr))
                    return context.invoke(expr);
                else {
                    // create a new Let
                    LetExp newlet = new LetExp();

                    // create a new declaration in the let, using
                    // the new expression value
                    Declaration decl = genLetDeclaration(expr, newlet);

                    // occurrences of expr in the next computation are
                    // referenced
                    // using the new declaration
                    newlet.body = context.invoke(new ReferenceExp(decl));
                    return newlet;
                }
            }
        };

        return visit(exp, newContext);
    }

    /**
     * Deals with the normalization of multiple expressions. It is actually used
     * to normalize the arguments of a function application. The array is updated 
     * inplace with references to the introduced let-bound variables.
     */
    protected Expression normalizeNames(final Expression[] exps, final int index,
                                        final MultiContext context) {
        if (exps.length == 0 || index == exps.length) {
            return context.invoke(exps, exps.length-1);
        } else {
            Context newContext = new Context() {

                @Override
                Expression invoke(final Expression expr) {
                    MultiContext newNewContext = new MultiContext() {

                        @Override
                        Expression invoke(Expression[] exprs, int ind) {
                            exprs[ind] = expr;
                            return context.invoke(exprs, ind-1);
                        }
                    };
                    return normalizeNames(exps, index + 1, newNewContext);
                }
            };

            return normalizeName(exps[index], newContext);
        }
    }    
    
    protected Expression visitQuoteExp(QuoteExp exp, Context context) {
        return context.invoke(exp);
    }

    protected Expression visitReferenceExp(ReferenceExp exp, Context context) {
        return context.invoke(exp);
    }
    
    protected Expression visitApplyExp(final ApplyExp exp, final Context context) {
        Expression func = exp.getFunction();
        
        // Special cases that we threat as atomic
        if ((isApplyToArgs(func)
                && exp.args[0] instanceof QuoteExp
                && (((QuoteExp)exp.args[0]).getValue() instanceof Type))
            || isGetNamedPart(exp)
            || isBracketList(exp)) {
          
            for (int i = 0; i < exp.args.length; i++) {
                exp.args[i] = normalizeTerm(exp.args[i]);
            }
            return context.invoke(exp);
            
        } else if (MakeSplice.argIfSplice(exp) != null
                   || isTypeSwitch(exp)){
          
            exp.args[0] = normalizeTerm(exp.args[0]);
            return context.invoke(exp);
            
        } else if (isSetter(func)) {
            return context.invoke(exp);
        } 

        
        // setting up starting index
        final int startIndex;    
        if (isApplyToArgs(func)) {
            func = exp.args[0];
            startIndex = 1;
        } else {
            startIndex = 0;           
        }

        Context newContext = new Context() {

            @Override
            Expression invoke(final Expression expr) {
                MultiContext newNewContext = new MultiContext() {

                    @Override
                    Expression invoke(Expression[] exprs, int ind) {
                        if (ind > -1)
                            exprs[ind] = expr;
                        else
                            exp.func = expr;
                        exp.args = exprs;
                        return context.invoke(exp);
                    }
                };
                return normalizeNames(exp.args, startIndex, newNewContext);
            }
        };

        return normalizeName(func, newContext);
    }
    
    private static boolean isApplyToArgs(Expression func) {
        return (func instanceof ReferenceExp)
                && func.getName().equals("applyToArgs");
    }    

    private static Expression getApplyFunc(ApplyExp aexp) {
        return (isApplyToArgs(aexp.getFunction())) 
               ? aexp.args[0] 
               : aexp.getFunction();        
    }
    
    private static boolean isSetter(Expression aexp) {
        return (aexp instanceof ApplyExp) 
                && ((ApplyExp)aexp).getFunction().getType() 
                    == ClassType.make("gnu.kawa.functions.Setter");
    }    
    
    private static boolean isGetNamedPart(Expression exp) {
        if (exp instanceof ApplyExp) {
            ApplyExp aexp = (ApplyExp) exp;
            Expression func = aexp.getFunction();
            return ((func instanceof QuoteExp) 
                      && ((QuoteExp) func).getValue() 
                            == GetNamedPart.getNamedPart);
        } else
            return false;
    }
    
    private static boolean isBracketList(Expression exp) {
        if (exp instanceof ApplyExp) {
            ApplyExp aexp = (ApplyExp) exp;
            Expression func = getApplyFunc(aexp);
            return ((func instanceof ReferenceExp) 
                      && ((ReferenceExp) func).getSymbol()
                            == LispLanguage.bracket_list_sym);
        } else
            return false;
    }    

    private static boolean isTypeSwitch(Expression exp) {
        if (exp instanceof ApplyExp) {
            ApplyExp aexp = (ApplyExp) exp;
            Expression func = getApplyFunc(aexp);
            return ((func instanceof QuoteExp) 
                      && ((QuoteExp) func).getValue() 
                            == TypeSwitch.typeSwitch);
        } else
            return false;
    }       
    
    protected Expression visitBeginExp(BeginExp exp, Context context) {

        // save compilation options
        Vector co = exp.compileOptions;

        LetExp firstLet = new LetExp();
        LetExp previous = firstLet;

        // transform each exp in a let binding
        int i = 0;
        for (; i < exp.exps.length - 1 && exp.exps[i + 1] != null; i++) {
            if (exp.exps[i] == QuoteExp.voidExp)
                continue;

            // create a new declaration in the let, using
            // the new expression value
            genLetDeclaration(exp.exps[i], previous);
            previous.body = new LetExp();
            previous = (LetExp) previous.body;
        }

        // last expression becomes the innermost let body
        previous.body = exp.exps[i];

        // we tranformed the begin to an equivalent let expression (it is not the 
        // actual transformation), now we need to normalize the obtained LetExp
        Expression result = visitLetExp(firstLet, context);

        // restore compilation options
        if (co != null) {
            result = new BeginExp(new Expression[] { result });
            ((BeginExp) result).compileOptions = co;
        }

        return result;
    }

    /** 
     * Besides handling "let" and "fluidlet" normalization, it flattens the 
     * nesting of let expressions.
     */
    protected Expression visitLetExp(final LetExp exp, Context context) {
        final Declaration first = exp.firstDecl();
        if (exp instanceof FluidLetExp && context != identity)
            return context.invoke(normalizeTerm(exp));
        if (first == null) {
            return visit(exp.body, context);
        } else {

            exp.decls = exp.decls.nextDecl();
            // create the inner let
            final Expression innerLet = visitLetExp(exp, context);

            // save the original first binding            
            Expression firstLetVal = first.getInitValue();          

            Context newContext = new Context() {
                @Override
                Expression invoke(Expression expr) {
                    if (expr != QuoteExp.voidExp) {
                        // create a new let
                        LetExp newlet = (exp instanceof FluidLetExp) 
                                        ? new FluidLetExp() 
                                        : new LetExp();

                        // set outer let binding and body
                        genLetDeclaration(expr, newlet, first);

                        newlet.body = innerLet;
                        return newlet;
                    } else
                        return innerLet;
                    
                }
            };
            return visit(firstLetVal, newContext);
        }
    }

    protected Expression visitIfExp(final IfExp exp, final Context context) {
        Context newContext = new Context() {

            @Override
            Expression invoke(Expression expr) {
                exp.then_clause = normalizeTerm(exp.then_clause);
                exp.else_clause = (exp.else_clause != null) 
                                  ? normalizeTerm(exp.else_clause)
                                  : null;
                
                exp.test = expr;
                
                return context.invoke(exp);
            }
        };
        return normalizeName(exp.test, newContext);
    }

    protected Expression visitCaseExp(final CaseExp exp, final Context context) {
        Context newContext = new Context() {

            @Override
            Expression invoke(Expression expr) {
                
                for (int i = 0; i < exp.clauses.length; i++) {
                    exp.clauses[i].exp = normalizeTerm(exp.clauses[i].exp);
                }
                if (exp.elseClause != null)
                    exp.elseClause.exp = normalizeTerm(exp.elseClause.exp);
                
                exp.key = expr;
                
                return context.invoke(exp);
            }
        };
        return normalizeName(exp.key, newContext);
    }

    protected Expression visitLambdaExp(LambdaExp exp, Context context) {
        exp.body = normalizeTerm(exp.body);
        return context.invoke(exp);
    }

    protected Expression visitSetExp(final SetExp exp, final Context context) {

        Declaration bin = exp.getBinding();
        
        if (bin != null) {
            bin.setCanWrite();
        }
        
        if (exp.isDefining()) {      
          
            if (!(exp.new_value instanceof ReferenceExp) 
                && !(exp.new_value instanceof QuoteExp))
                exp.new_value = normalizeTerm(exp.new_value);    
          
            return context.invoke(exp);
        }

        if ((exp.new_value instanceof LambdaExp)
            && bin.getInitValue() == QuoteExp.undefined_exp) {
            bin.noteValueUnknown();
        }        
        
        Context newContext = new Context() {

            @Override
            Expression invoke(Expression expr) {
                // create a new Let
                LetExp newlet = new LetExp();

                // change the binding
                exp.new_value = expr;
               
                // create a new declaration for the let, using
                // the new expression value
                genLetDeclaration(exp, newlet);
                
                newlet.body = context.invoke(QuoteExp.voidExp);
                return newlet;
            }
        };
        return normalizeName(exp.new_value, newContext);
    }
    
    protected Expression visitModuleExp(ModuleExp exp, Context context) {
        if (exp.body instanceof ApplyExp) {
            ApplyExp body = ((ApplyExp)exp.body);
            for (int i = 0; i < body.args.length; i++) {
              body.args[i] = visit(body.args[i], context);
            }
            return exp;          
        } 
        
        return visitExpression(exp, context);
    }    
    
    protected Expression visitTryExp(TryExp exp, Context context) {
        exp.try_clause = normalizeTerm(exp.try_clause);
        if (exp.catch_clauses != null) {
            exp.catch_clauses = toCatchClause((LetExp) normalizeTerm(exp.catch_clauses),
                                              exp.catch_clauses.next);
            for (CatchClause c = exp.catch_clauses; c.next != null; c = c.next)
                c.next = toCatchClause((LetExp) normalizeTerm(c.next), c.next.next);
        }
        if (exp.finally_clause != null)
            exp.finally_clause = normalizeTerm(exp.finally_clause);
        return context.invoke(exp);
    }
    
    protected CatchClause toCatchClause(LetExp exp, CatchClause next) {
        CatchClause clause = new CatchClause(exp.decls, exp.body);
        clause.next = next;
        return clause;
    }
    
    protected Expression visitSynchronizedExp(final SynchronizedExp exp,
                                              final Context context) {
      
        Context newContext = new Context() {

            @Override
            Expression invoke(Expression expr) {
                exp.body = normalizeTerm(exp.body);
                exp.object = expr;
                
                return context.invoke(exp);
            }
        };        
        
        return normalizeName(exp.object, newContext);
    }
    
    protected Expression visitBlockExp(BlockExp exp, Context context) {
        exp.body = normalizeTerm(exp.body);
        if (exp.exitBody != null)
            exp.exitBody = normalizeTerm(exp.exitBody);
        return context.invoke(exp);
    }
    
    protected Expression visitExitExp(final ExitExp exp, final Context context) { 
        exp.result = normalizeTerm(exp.result);
        return context.invoke(exp);
    }
    
    protected Expression visitClassExp(ClassExp exp, Context context) {
        for (Declaration decl = exp.firstDecl();  decl != null;
             decl = decl.nextDecl()) {
            Expression e = decl.getValue();
            if (e != null)
                if (e instanceof LambdaExp)
                  decl.setValue(normalizeClassMethod((LambdaExp)e));
                else
                  decl.setValue(normalizeTerm(e));
        }
        if (exp.firstChild != null) {
            LambdaExp next = exp.firstChild.nextSibling;
            exp.firstChild = (LambdaExp) normalizeClassMethod(exp.firstChild);
            exp.firstChild.nextSibling = next;
            for (LambdaExp child = exp.firstChild; child.nextSibling != null; 
              child = child.nextSibling) {
                next = child.nextSibling.nextSibling;
                child.nextSibling = (LambdaExp) normalizeClassMethod(child.nextSibling);
                child.nextSibling.nextSibling = next;
            }
        }

        return context.invoke(exp);
    }  

    /**
     * Normalize a class method, avoiding problems when the method is a constructor.
     */
    private Expression normalizeClassMethod(LambdaExp exp) {
        if (exp.isClassMethod())
            if ("*init*".equals(exp.getName())) {
                if (exp.body instanceof BeginExp) {
                    Expression bodyFirst = exp.getBodyFirstExpression();
                    ((BeginExp) exp.body).exps[0] = QuoteExp.voidExp;
                    exp.body = new BeginExp(bodyFirst, normalizeTerm(exp.body));
                }
            } else {
                return normalizeTerm(exp); 
            }
        else
            throw new Error();        
        return exp;
    }
    
}
