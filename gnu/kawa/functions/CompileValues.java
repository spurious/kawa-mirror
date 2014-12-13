package gnu.kawa.functions;

import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.*;

/** Compile-time support for call-with-values and related code.
 */

public class CompileValues {
     public static Expression validateCallWithValues
        (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc) {
         Expression[] args = exp.getArgs();
         if (args.length == 2) {
             ApplyExp ae = new ApplyExp(ApplyWithValues.applyWithValues,
                                        new ApplyExp(args[0],
                                                     Expression.noExpressions),
                                        args[1]);
             ae.setLine(exp);
             return visitor.visit(ae, required);
         }
         exp.visitArgs(visitor);
         return exp;
     }
 }
