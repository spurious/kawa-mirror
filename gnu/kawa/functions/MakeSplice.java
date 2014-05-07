package gnu.kawa.functions;
import gnu.expr.*;
import gnu.mapping.*;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

/** A pseudo-function whose argument is splice into an outer argument list.
 * Represented by {@code ($splice$ arg)}.
 * If {@code arg} is the list or array {@code [a b c]}
 * then {@code (fun x ($splice$ arg) y)} is {@code (fun x a b c y)}.
 * Processed at compile-time only.
 */

public class MakeSplice extends Procedure1 {
    public static final MakeSplice instance = new MakeSplice();

    public static final QuoteExp quoteInstance = new QuoteExp(instance);

    public static Expression argIfSplice(Expression exp) {
        if (exp instanceof ApplyExp) {
            ApplyExp aexp = (ApplyExp) exp;
            if (aexp.getFunction() == quoteInstance)
                return aexp.getArg(0);
        }
        return null;
    }

    public Object apply1 (Object arg1) throws Throwable {
        throw new UnsupportedOperationException("$splice$ function should not be called");
    }

    /** Helper method called by compiled code. */
    public static void addAll(ArrayList<Object> list, Object values) {
        if (values instanceof Object[]) {
            Object[] arr = (Object[]) values;
            int nlen = arr.length;
            for (int i = 0; i < nlen;  i++)
                list.add(arr[i]);
        } else if (values instanceof List<?>) {
            list.addAll((List<?>) values);
        } else if (values.getClass().isArray()) {
            int nlen = Array.getLength(values);
            for (int i = 0; i < nlen;  i++)
                list.add(Array.get(values, i));
        } else
            throw new ClassCastException("value is neither List or array");
    }
}
