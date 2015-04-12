package gnu.q2.lang;

import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.*;
import java.util.ArrayList;
import static gnu.kawa.functions.CompilationHelpers.validateApplyToArgs;

public class CompileMisc {

    static boolean optimizeSimpleApply = false;

    public static Expression validateQ2Apply
        (ApplyExp exp, InlineCalls visitor,
         Type required, Procedure applyToArgs) {
        if (! optimizeSimpleApply)
            return null;

        Expression[] args = exp.getArgs();
        int nargs = args.length;
        if (nargs == 1) {
            Declaration pdecl; Expression pval;
            Object pvalue = null;
            if (! (args[0] instanceof ReferenceExp)
                || (pdecl = ((ReferenceExp) args[0]).getBinding()) == null
                || (pval = pdecl.getValue()) == null
                || ! ((pval instanceof LambdaExp
                       && ((LambdaExp) pval).min_args == 0)
                      || ((pvalue = pval.valueIfConstant())
                          instanceof Procedure
                          && ((Procedure) pvalue).minArgs() == 0)))
                return visitor.visit(args[0], required);
        }
        ArrayList<Expression> rargs = new ArrayList<Expression>();
        for (int i = 0; i < nargs; i++) {
            Expression arg = exp.getArg(i);
            if (arg instanceof ApplyExp) {
                ApplyExp aarg = (ApplyExp) arg;
                if (aarg.isAppendValues()) {
                    System.err.println("isAppendValue");
                    int naarg = aarg.getArgCount();
                    int vargs = 0;
                    for (int j = 0; j < naarg;  j++) {
                        Expression xaarg = aarg.getArg(j);
                        //System.err.println("xxarg "+xaarg);
                        if (xaarg instanceof SetExp) {
                            //System.err.println("set xxarg");
                            vargs++;
                        }
                        rargs.add(xaarg);
                    }
                    continue;
                }
            }
                 
            rargs.add(arg);
        }
        args = rargs.toArray(new Expression[rargs.size()]);
        ApplyExp nexp = new ApplyExp(applyToArgs, args);
                                       
        return validateApplyToArgs
            (nexp, visitor, required, applyToArgs);
    }
}
