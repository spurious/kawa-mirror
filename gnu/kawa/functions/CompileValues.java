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
            boolean singleArgConsumer = false;
            Expression cvalue = consumer;
            if (cvalue instanceof ReferenceExp) {
                Declaration d = ((ReferenceExp) cvalue).getBinding();
                cvalue = Declaration.followAliases(d).getValue();
            }
            if (cvalue instanceof LambdaExp) {
                LambdaExp lconsumer = (LambdaExp) cvalue;
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
                    singleArgConsumer = (lconsumer.min_args == 1);
                }
            } else if (cvalue instanceof QuoteExp) {
                Object rconsumer = cvalue.valueIfConstant();
                if (rconsumer instanceof Procedure) {
                    Procedure pconsumer = (Procedure) rconsumer;
                    if (pconsumer.minArgs() == pconsumer.maxArgs()) {
                        Type[] types = new Type[pconsumer.minArgs()];
                        if (pconsumer instanceof MethodProc) {
                            MethodProc mpconsumer = (MethodProc) pconsumer;
                            for (int i = 0; i < types.length; ++i) {
                                types[i] = mpconsumer.getParameterType(i);
                            }
                        } else {
                            for (int i = 0; i < types.length; ++i) {
                                types[i] = Type.objectType;
                            }
                        }
                        prequired = MultValuesType.create(types);
                        singleArgConsumer = (pconsumer.minArgs() == 1);
                    }
                }
            }
            producer = visitor.visit(producer, prequired);
            if (singleArgConsumer) {
                ApplyExp ae = new ApplyExp(consumer, producer);
                ae.setLine(exp);
                return visitor.visit(ae, required);
            }
            if (prequired == null)
                prequired = producer.getType();

            if (prequired instanceof MultValuesType) {
                MultValuesType mprequired = (MultValuesType) prequired;
                int nvalues = mprequired.getValueCount();
                Compilation comp = visitor.getCompilation();
                comp.letStart();
                PrimProcedure incrPosProc =
                    new PrimProcedure(Compilation.typeValues
                                      .getDeclaredMethod("incrPos", 2));
                PrimProcedure getFromPosProc =
                    new PrimProcedure(Compilation.typeValues
                                      .getDeclaredMethod("getFromPos", 2));
                Expression apply = visitor.getCompilation()
                    .applyFunction(consumer);
                int applyAdjust = apply == null ? 0 : 1;
                Expression[] cargs = new Expression[applyAdjust+nvalues];
                Declaration valsDecl = comp.letVariable(null, Type.objectType, producer);
                QuoteExp zero = new QuoteExp(Integer.valueOf(0), Type.intType);
                Declaration iposDecl = nvalues == 0 ? null
                    : comp.letVariable(null,
                                       Type.intType,
                                       zero);
                 comp.letEnter();
                 for (int i = 0; i < nvalues; i++) {
                     SetExp incr =
                         new SetExp(iposDecl,
                                    new ApplyExp(incrPosProc,
                                                 new ReferenceExp(valsDecl),
                                                 new ReferenceExp(iposDecl)));
                     iposDecl.noteValueFromSet(incr);
                     if (i + 1 == nvalues)
                         getFromPosProc =
                             new PrimProcedure(Compilation.typeValues
                                               .getDeclaredMethod("getFromPosFinal", 2));
                     cargs[applyAdjust+i] =
                         new BeginExp(incr,
                                      new ApplyExp(getFromPosProc,
                                                   new ReferenceExp(valsDecl),
                                                   new ReferenceExp(iposDecl)));
                 }
                 Expression callCons;
                 if (applyAdjust == 0)
                     callCons = new ApplyExp(consumer, cargs);
                 else {
                     cargs[0] = consumer;
                     callCons = new ApplyExp(apply, cargs);
                 }
                 if (nvalues == 0) {
                     Method checkFinalPosMethod = Compilation.typeValues
                         .getDeclaredMethod("checkFinalPos", 2);
                     callCons = new BeginExp(new ApplyExp(checkFinalPosMethod,
                                                          new ReferenceExp(valsDecl), zero),
                                             callCons);
                 }
                 return visitor.visit(comp.letDone(callCons), required);
             }
             args[0] = producer;
         }
         exp.visitArgs(visitor);
         return exp;
     }
}
