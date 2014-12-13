package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.kawa.lispexpr.LangObjType;

/** This procedure's first argument values are applied as argument list to the second argument.
 * The call: {@code(call-with-values prod cons)}
 * is equivalent to: {@code (apply-with-values (prod) cons)}.
 * (However, the latter isn't expressible in Scheme because you can't
 * bind multiple values to an argument or variable in Scheme.
 * The Kawa compiler supports it internally, due to it's use by XQuery.)
 */

public class ApplyWithValues extends Procedure2 {
    public static final ApplyWithValues applyWithValues = new ApplyWithValues();

     public static Object applyWithValues(Object values, Procedure consumer)
        throws Throwable {
        if (values instanceof Values)
            return ((Values) values).call_with(consumer);
        else
            return consumer.apply1(values);
    }

    public Object apply2(Object values, Object consumer) throws Throwable {
        return applyWithValues(values,
                               LangObjType.coerceToProcedure(consumer));
    }

    public void apply(CallContext ctx) throws Throwable {
        Procedure.checkArgCount(this, 2);
        Object[] args = ctx.getArgs();
        Object values = args[0];
        Procedure consumer = LangObjType.coerceToProcedure(args[1]);
        if (values instanceof Values)
            ((Values) values).check_with(consumer, ctx);
        else
            consumer.check1(values, ctx);
    }
}
