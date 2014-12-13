package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.kawa.lispexpr.LangObjType;

public class CallWithValues extends Procedure2 {
    public static final CallWithValues callWithValues = new CallWithValues();
    static {
        callWithValues.setName("call-with-values");
        callWithValues.setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileValues:validateCallWithValues");
    }

    public static Object callWithValues(Procedure producer, Procedure consumer)
        throws Throwable {
        return ApplyWithValues.applyWithValues(producer.apply0(), consumer);
    }

    public Object apply2(Object producer, Object consumer) throws Throwable {
        return callWithValues(LangObjType.coerceToProcedure(producer),
                              LangObjType.coerceToProcedure(consumer));
    }

    public void apply(CallContext ctx) throws Throwable {
        Procedure.checkArgCount(this, 2);
        Object[] args = ctx.getArgs();
        Object values = LangObjType.coerceToProcedure(args[0]).apply0();
        Procedure consumer = LangObjType.coerceToProcedure(args[1]);
        if (values instanceof Values)
            ((Values) values).check_with(consumer, ctx);
        else
            consumer.check1(values, ctx);
    }
}


