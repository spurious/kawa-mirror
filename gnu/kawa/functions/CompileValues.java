package gnu.kawa.functions;

import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.mapping.*;

/** Compile-time support for call-with-values and related code.
 */

public class CompileValues {
     public static Expression validateCallWithValues
        (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc) {
         Expression[] args = exp.getArgs();
         if (args.length == 2) {
             Expression arg0 = args[0];
             Expression apply = visitor.getCompilation().applyFunction(arg0);
             Expression produce = apply != null ? new ApplyExp(apply, arg0)
                 : new ApplyExp(arg0, Expression.noExpressions);
             ApplyExp ae = new ApplyExp(ApplyWithValues.applyWithValues,
                                        produce, args[1]);
             ae.setLine(exp);
             return visitor.visit(ae, required);
         }
         exp.visitArgs(visitor);
         return exp;
     }

     public static Expression validateApplyWithValues
        (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc) {
         Expression[] args = exp.getArgs();
         if (args.length == 2) {
             Expression producer = args[0];
             Expression consumer = args[1];
             Type prequired = null;
             if (consumer instanceof LambdaExp) {
                 LambdaExp lconsumer = (LambdaExp) consumer;
                 if (lconsumer.min_args == lconsumer.max_args) {
                     Type[] types = new Type[lconsumer.min_args];
                     int i = 0;
                     for (Declaration param = lconsumer.firstDecl();
                          param != null; param = param.nextDecl()) {
                         boolean typeSpecified = param.getFlag(Declaration.TYPE_SPECIFIED);
                         Type type =
                             param.getFlag(Declaration.TYPE_SPECIFIED)
                             ? param.getType()
                             : null;
                         types[i++] = type;
                     }
                     prequired = MultValuesType.create(types);
                 }
             }
             producer = visitor.visit(producer, prequired);
             args[0] = producer;
         }
         exp.visitArgs(visitor);
         return exp;
     }
}
