package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.SlotGet;
import java.util.Iterator;

/** Implement the Scheme standard functions "map" and "for-each".
 * @author Per Bothner
 */

public class Map extends gnu.mapping.ProcedureN
{
  /** True if we should collect the result into a list. */
  boolean collect;

  final ApplyToArgs applyToArgs;
  final IsEq isEq;

  public Map (boolean collect, ApplyToArgs applyToArgs, IsEq isEq)
  {
    super (collect ? "map" : "for-each");
    this.collect = collect;
    this.applyToArgs = applyToArgs;
    this.isEq = isEq;
    setProperty(Procedure.validateApplyKey,
                collect ? "kawa.lib.compile_map:listMapValidateApply"
                : "kawa.lib.compile_map:listForEachValidateApply");
  }

    /** An optimized single-list version of map. */
    static public Object map1(Procedure proc, LList list) throws Throwable {
        Object cur = list;
        Object result = LList.Empty;
        Pair last = null;
        while (cur != LList.Empty) {
            Pair pair = (Pair) cur;
            Pair new_pair = new Pair (proc.apply1(pair.getCar()), LList.Empty);
            if (last == null)
                result = new_pair;
            else
                last.setCdr(new_pair);
            last = new_pair;
            cur = pair.getCdr();
        }
        return result;
    }

    static public Object map1(Procedure proc, Object list) throws Throwable {
        if (list instanceof LList)
            return map1(proc, (LList) list);
        else {
            Object result = LList.Empty;
            Pair last = null;
            for (Iterator it = Sequences.getIterator(list); it.hasNext(); ) {
                Pair new_pair = new Pair(proc.apply1(it.next()), LList.Empty);
                if (last == null)
                    result = new_pair;
                else
                    last.setCdr(new_pair);
                last = new_pair;
            }
            return result;
        }
    }

    /** An optimized single-list version of for-each. */
    static public void forEach1(Procedure proc, LList list) throws Throwable {
        for (Object cur = list; cur != LList.Empty; ) {
            Pair pair = (Pair) cur;
            proc.apply1(pair.getCar());
            cur = pair.getCdr();
        }
    }

    /** An optimized single-list version of for-each. */
    static public void forEach1(Procedure proc, Object list) throws Throwable {
        if (list instanceof LList)
            forEach1(proc, (LList) list);
        else 
            for (Iterator it = Sequences.getIterator(list); it.hasNext(); )
                proc.apply1(it.next());
    }

  public Object apply2 (Object arg1, Object arg2) throws Throwable
  {
    if (arg1 instanceof Procedure)
      {
        Procedure proc = (Procedure) arg1;
        if (collect)
          return map1 (proc, arg2);
        forEach1 (proc, arg2);
        return Values.empty;
      }
    return applyN(new Object[] { arg1, arg2 });
  }

    public Object applyN(Object[] args) throws Throwable {
        int arity = args.length - 1;
        if (arity == 1 && args[0] instanceof Procedure) {
            Procedure proc = (Procedure) (args[0]);
            if (collect)
                return map1 (proc, args[1]);
            forEach1 (proc, args[1]);
            return Values.empty;
        }
        Object result;
        Pair last = null;
        if (collect)
            result = LList.Empty;
        else
            result = Values.empty;;
        Procedure proc;
        int need_apply;
        Object[] each_args;
        if (args[0] instanceof Procedure) {
            need_apply = 0;
            each_args = new Object[arity];
            proc = (Procedure) args[0];
        } else {
            need_apply = 1;
            each_args = new Object[arity+1];
            each_args[0] = args[0];
            proc = applyToArgs;
        }
        Iterator[] iterators = new Iterator[arity];
	for (int i = 0;  i < arity;  i++)
            iterators[i] = Sequences.getIterator(args[i+1]);
        for (;;) {
            for (int i = 0;  i < arity;  i++) {
                Iterator it = iterators[i];
                if (! it.hasNext())
                    return result;
                each_args[need_apply+i] = it.next();
            }
            Object value = proc.applyN(each_args);
            if (collect) {
                Pair new_pair = new Pair(value, LList.Empty);
                if (last == null)
                    result = new_pair;
                else
                    last.setCdr(new_pair);
                last = new_pair;
            }
        }
    }

    public int numArgs() { return (-1 << 12) | 2; }
}
