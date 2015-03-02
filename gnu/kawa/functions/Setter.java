package gnu.kawa.functions;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.Language;
import gnu.mapping.Procedure;
import gnu.kawa.reflect.Invoke;
import gnu.lists.Array;

/** Implements Kawa extension function "setter", as in SRFI-17. */

public class Setter extends Procedure1 implements HasSetter {
    public static final Setter setter = new Setter();
    static {
        setter.setName("setter");
        setter.setProperty(Procedure.validateApplyKey,
                           "gnu.kawa.functions.CompilationHelpers:validateSetter");
    }

    public static Object setter(Procedure arg) {
        return arg.getSetter();
    }

    public Object apply1(Object arg){
        if (! (arg instanceof Procedure)) {
            if (arg instanceof java.util.List)
                return new SetList((java.util.List) arg);
            if (arg instanceof Array)
                return new SetGArray((Array) arg);
            Class cl = arg.getClass();
            if (cl.isArray())
                return new SetArray(arg,
                                    Language.getDefaultLanguage()/*FIXME*/);
        }
        return ((Procedure)arg).getSetter();
    }

    public void set1(Object arg1, Object value) throws Throwable {
        ((Procedure) arg1).setSetter((Procedure) value);
    }

    public static class SetArray extends Procedure2 {
        Object array;
        Type elementType;

        public SetArray(Object array, Language language) {
            Class elementClass = array.getClass().getComponentType();
            elementType = language.getTypeFor(elementClass);
            this.array = array;
        }

        public Object apply2(Object index, Object value) {
            value = elementType.coerceFromObject(value);
            java.lang.reflect.Array.set(array,
                                        ((Number) index).intValue(),
                                        value);
            return Values.empty;
        }
    }

    public static class SetList extends Procedure2 {
        java.util.List list;
        public SetList(java.util.List list) {
            this.list = list;
        }

        Type elementType;

        public Object apply2(Object index, Object value) {
            list.set(((Number) index).intValue(), value);
            return Values.empty;
        }
    }
    
    public static class SetGArray extends ProcedureN {
        Array array;

        public SetGArray(Array array) {
            this.array = array;
        }

        public Object applyN(Object[] args) {
            int dim = args.length-1;
            int[] indexes = new int[dim];
            for (int i = dim;  --i >= 0; ) {
                indexes[i] = ((Number) args[i]).intValue();
            }
            array.set(indexes, args[dim]);
            return Values.empty;
        }
    }
}
