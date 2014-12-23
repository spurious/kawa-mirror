// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.mapping.*;

/** Implement 'typeswitch' (as in XQuery) or 'typecase'.
 * Usage: (typeswitch SELECTOR CASE-LAMBDA ... DEFAULT-LAMBDA)
 * Each CASE-LAMBDA is a 1-argument MethodProc, while DEFAULT-LAMBDA
 * is a 1-argument Procedure.  Calls the first CASE-LAMBDA such that
 * SELECTOR is a valid argument; if there is none, calls DEFAULT-LAMBDA.
 * In the current implementation, all of CASE-LAMBDA and DEFAULT-LAMBDA
 * must be LambdaExps, and the call must be inlined.
 */

public class TypeSwitch extends MethodProc implements Inlineable {

    public static final TypeSwitch typeSwitch = new TypeSwitch("typeswitch");

    public TypeSwitch(String name) {
        setName(name);
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.reflect.CompileReflect:validateApplyTypeSwitch");
    }

    public int numArgs() { return 0xfffff002; }

    public void apply(CallContext ctx) throws Throwable {
        Object[] args = ctx.getArgs();
        Object selector = args[0];
        int n = args.length-1;
        for (int i = 1;  i < n;  i++) {
            MethodProc caseProc = (MethodProc) args[i];
            int m = caseProc.match1(selector, ctx);
            if (m >= 0)
                return;
        }
        Procedure defaultProc = (Procedure) args[n];
        defaultProc.check1(selector, ctx);
    }

    public void compile(ApplyExp exp, Compilation comp, Target target) {
        Expression[] args = exp.getArgs();

        CodeAttr code = comp.getCode();
        code.pushScope();
        Variable selector = code.addLocal(Type.pointer_type);
        args[0].compile(comp, Target.pushObject);
        code.emitStore(selector);
        int numCondClauses = 0;

        for (int i = 1;  i < args.length;  ) {
            Expression arg = args[i++];

            if (arg instanceof LambdaExp) {
                LambdaExp lambda = (LambdaExp) arg;
                int numConditionsThisLambda = 0;
                for (Declaration param = lambda.firstDecl();
                     param != null;  param = param.nextDecl()) {
                    Type type = param.getType();
                    boolean hasInitExpr =
                        param.getFlag(Declaration.PATTERN_NESTED);
                    Type valType = hasInitExpr ? param.getInitValue().getType()
                        : args[0].getType();
                    // Rather simplistic ...
                    boolean isConditional = type != Type.objectType
                        && type != Type.toStringType
                        && type != valType;
                    if (param.getCanRead() || isConditional)
                        param.allocateVariable(code);
                    if (isConditional) {
                        if (numConditionsThisLambda > 0)
                            code.emitAndThen();
                        numConditionsThisLambda++;
                    }
                    Variable incoming;
                    if (hasInitExpr) {
                        Expression initExpr = param.getInitValue();
                        Target ptarget = isConditional || param.getCanRead()
                            ? Target.pushValue(valType)
                            : Target.Ignore;
                        initExpr.compile(comp, ptarget);
                        if (ptarget == Target.Ignore)
                            incoming = null;
                        else {
                            incoming = param.getContext().getVarScope().addVariable(code, valType.getImplementationType(), null);
                            code.emitStore(incoming);
                        }
                    }
                    else
                        incoming = selector;

                    if (LazyType.maybeLazy(valType)
                        && ! LazyType.maybeLazy(type)) {
                        code.emitLoad(incoming);
                        valType = StackTarget.forceLazy(comp, valType, type);
                        incoming = param.getContext().getVarScope().addVariable(code, valType.getImplementationType(), null);
                        code.emitStore(incoming);
                    }
                    boolean storeNeeded = param.getCanRead();
                    if (isConditional) {
                        if (type instanceof TypeValue) {
                            ((TypeValue) type).emitTestIf(incoming, param, comp);
                            storeNeeded = false;
                        }
                        else {
                            code.emitLoad(incoming);
                            type.emitIsInstance(code);
                            code.emitIfIntNotZero();
                        }
                    }
                    if (storeNeeded) {
                        code.emitLoad(incoming);
                        if (isConditional)
                            type.emitCoerceFromObject(code);
                        param.compileStore(comp);
                    }
                }
                lambda.allocChildClasses(comp);
                lambda.body.compileWithPosition(comp, target);
                if (numConditionsThisLambda == 0)
                    break;
                else if (i < args.length) {
                    numCondClauses++;
                    code.emitElse();
                }

            } else {
                throw new Error("not implemented: typeswitch arg not LambdaExp");
            }
        }
        while (--numCondClauses >= 0)
            code.emitFi();
    
        code.popScope();
    }


    public Type getReturnType(Expression[] args) {
        return Type.pointer_type;
    }
}
