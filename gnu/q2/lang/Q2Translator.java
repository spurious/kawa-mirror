package gnu.q2.lang;

import gnu.bytecode.Type;
import gnu.kawa.functions.MakeSplice;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.mapping.Procedure;
import gnu.expr.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.mapping.Symbol;
import kawa.lang.*;
import kawa.standard.Scheme;
import kawa.standard.SchemeCompilation;
import java.util.ArrayList;
import java.util.Stack;

public class Q2Translator extends SchemeCompilation
{
  public Q2Translator (Language language, SourceMessages messages, NameLookup lexical)
  {
    super(language, messages, lexical);
  }

  /** Split list according to operator-precedence priorities.
   */
  public static Object partition (Object p, Translator tr) 
  {
    // A stack of: Fence, (arg-list, Pair<Operator>, Operator)*
    // The "value" of each Pair<Operator> is the same as the following Operator.
    // The invariant is that for each i, where i is 0, 3, 7, ..., we have:
    // ((Operator)st.get(i)).rprio < ((Operator)st.get(i+3)).lprio
    Stack st = new Stack();
    st.add(Operator.FENCE);
    Object larg = p;
    Pair prev = null;

    for (;;)
      {
        if (p instanceof SyntaxForm)
          ; // FIXME
        Operator op = null;
        Pair pp;
        if (! (p instanceof Pair))
          {
            op = Operator.FENCE;
            pp = null;
          }
        else
          {
            pp = (Pair) p;
            Object obj = pp.getCar();
            if (obj instanceof Symbol && ! Q2.instance.selfEvaluatingSymbol(obj))
              {
                Expression func = tr.rewrite(obj, true);
                Declaration decl;
                Object value;
                if (func instanceof ReferenceExp
                    && (decl = ((ReferenceExp) func).getBinding()) != null
                    && (value = decl.getConstantValue()) instanceof Operator)
                  {
                    op = (Operator) value;
                  }
              }
          }
        if (op != null)
          {
            if (prev == null)
              larg = LList.Empty;
            else if (p instanceof Pair)
              prev.setCdrBackdoor(LList.Empty);
            int stsz = st.size();
            Operator topop = (Operator) st.get(stsz-1);
            while (op.lprio <= topop.rprio)
              {
                PairWithPosition oppair = (PairWithPosition) st.get(stsz-2);
                if ((topop.flags & Operator.RHS_NEEDED) != 0
                      && larg == LList.Empty)
                    tr.error('e', "missing right operand after "+topop.getName(), oppair);
                larg = topop.combine((LList) st.get(stsz-3), larg,
                                     oppair);
                stsz -= 3;
                st.setSize(stsz);
                topop = (Operator) st.get(stsz-1);
              }
            if (pp == null)
              break;
            st.add(larg);
            st.add(pp);
            st.add(op);
            larg = pp.getCdr();
            prev = null;
          }
        else
          prev = pp;
        p = pp.getCdr();
      }
    return larg;
  }
    public Expression makeBody(Expression[] exps) {
        int nlen = exps.length;
        for (int i = 0; i < nlen-1; i++) {
            Expression exp = exps[i];
            if (exp instanceof IfExp) {
                IfExp iexp = (IfExp) exp;
                if (iexp.getElseClause() == null) {
                    Expression[] rest = new Expression[nlen-i-1];
                    System.arraycopy(exps, i+1, rest, 0, rest.length);
                    iexp = new IfExp(iexp.getTest(), iexp.getThenClause(),
                                     makeBody(rest));
                    iexp.setLine(exp);
                    if (i == 0)
                        return iexp;
                    Expression[] init = new Expression[i+1];
                    System.arraycopy(exps, 0, init, 0, i);
                    init[i] = iexp;
                    return super.makeBody(init);
                }
            }
        }
        return super.makeBody(exps);
    }

  public void scanForm (Object st, ScopeExp defs)
  {
    if (st instanceof LList)
      st = partition(st, this);
    if (st != LList.Empty)
      super.scanForm(st, defs);
  }

  public Expression rewrite (Object exp, boolean function)
  {
    if (exp == LList.Empty)
      return QuoteExp.voidExp;
    return super.rewrite(exp, function);
  }

  public Expression rewrite_pair (Pair p, boolean function)
  {
    Object partitioned = partition(p, this);
    if (partitioned instanceof Pair) {
        Pair pair = (Pair) partitioned;
        Object p_car = pair.getCar();
        if (p_car instanceof Pair
            && ((Pair) p_car).getCar() == LispLanguage.splice_sym)
            return new ApplyExp(MakeSplice.quoteInstance,
                                rewrite_car((Pair)((Pair) p_car).getCdr(), function));
        else {
            Expression exp = super.rewrite_pair(pair, function);
            ApplyExp app;
            if (exp instanceof ApplyExp
                && isApplyFunction((app = (ApplyExp) exp).getFunction())) {
                exp = convertApply(app);
            }
            return exp;
        }
    }
    else
      return rewrite(partitioned, function);
  }

    /** If the argument has zero arguments, should we still apply it? */
    public static boolean applyNullary(Expression exp) {
        if (exp instanceof ReferenceExp) {
            Declaration decl =
                Declaration.followAliases(((ReferenceExp) exp).getBinding());
            if (decl != null) {
                if (decl.isProcedureDecl())
                    return true;
                if (decl.getFlag(Declaration.STATIC_SPECIFIED)
                    && decl.getFlag(Declaration.IS_CONSTANT)) { // kludge
                    Type type = decl.getType();
                    if ("gnu.kawa.lispexpr.LangObjType" == type.getName())
                        return true;
                }
            }
        }
        if (exp instanceof QuoteExp) {
            Object val = exp.valueIfConstant();
            return val instanceof Type || val instanceof Class;
        }
        return false;
    }

    public static Expression convertApply
        (ApplyExp exp) {
 
        Expression[] args = exp.getArgs();
        int nargs = args.length;

        Expression arg0 = args[0];
        if (nargs == 1 && ! applyNullary(arg0)) {
            if (arg0 instanceof IfExp
                && ((IfExp) arg0).getElseClause() == null)
                arg0 = new BeginExp(args);
            return arg0;
        }

        ArrayList<Expression> rargs = new ArrayList<Expression>();

        LetExp let = null;
        for (int i = 0; i < nargs; i++) {
            Expression arg = exp.getArg(i);
            Expression barg;
            if (arg instanceof LetExp && arg.getFlag(LetExp.IS_BODY_SCOPE)
                // Can we get more than one LetExp? FIXME
                && let == null) {
                barg = ((LetExp) arg).getBody();
            } else
                barg = arg;
            if (barg instanceof ApplyExp) {
                ApplyExp aarg = (ApplyExp) barg;
                if (aarg.isAppendValues()) {
                    if (arg != barg)
                        let = (LetExp) arg;
                    int naarg = aarg.getArgCount();
                    for (int j = 0; j < naarg;  j++) {
                        Expression xaarg = aarg.getArg(j);
                        if (xaarg instanceof SetExp) {
                            xaarg = new ApplyExp(MakeSplice.quoteInstance,
                                                 new BeginExp(xaarg, QuoteExp.emptyExp));
                            if (exp.firstSpliceArg == -1
                                || exp.firstSpliceArg > j)
                                exp.firstSpliceArg = j;
                        }
                        rargs.add(xaarg);
                    }
                    continue;
                }
            }
            rargs.add(arg);
        }
        args = rargs.toArray(new Expression[rargs.size()]);
        Procedure proc = Scheme.applyToArgs;
        exp.setFuncArgs(new QuoteExp(proc), args);
        if (let != null) {
            let.setBody(exp);
            return let;
        }
        return exp;
    }
}
