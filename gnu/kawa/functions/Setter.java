package gnu.kawa.functions;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.Language;
import gnu.mapping.Procedure;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.lists.AbstractSequence;
import gnu.lists.Array;
import gnu.lists.ComposedArray;
import gnu.lists.FString;
import gnu.lists.Range;
import gnu.lists.Sequence;
import gnu.lists.Sequences;
import gnu.lists.SimpleVector;

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
            if (elementType != null)
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
            if (list instanceof SimpleVector) {
                String tag = ((SimpleVector) list).getTag();
                char tag0 = tag == null || tag.length() == 0 ? 0
                    : tag.charAt(0);
                switch (tag0) {
                case 'c':
                    if (tag.equals("c16"))
                        elementType = LangPrimType.charType;
                    else if (tag.equals("c32"))
                        elementType = LangPrimType.characterType;
                    break;
                case 'f':
                    if (tag.equals("f32"))
                        elementType = LangPrimType.floatType;
                    else if (tag.equals("f64"))
                        elementType = LangPrimType.doubleType;
                    break;
                case 'u':
                    if (tag.equals("u64"))
                        elementType = LangPrimType.unsignedLongType;
                    else if (tag.equals("u32"))
                        elementType = LangPrimType.unsignedIntType;
                    else if (tag.equals("u16"))
                        elementType = LangPrimType.unsignedShortType;
                    else if (tag.equals("u8"))
                        elementType = LangPrimType.unsignedByteType;
                case 's':
                    if (tag.equals("s64"))
                        elementType = LangPrimType.longType;
                    else if (tag.equals("s32"))
                        elementType = LangPrimType.intType;
                    else if (tag.equals("s16"))
                        elementType = LangPrimType.shortType;
                    else if (tag.equals("s8"))
                        elementType = LangPrimType.byteType;
                }
            }
            this.list = list;
        }

        Type elementType;

        public Object apply2(Object index, Object value) {
            if (index instanceof Range.IntRange) {
                Range.IntRange range = (Range.IntRange) index;
                int istart = range.getStartInt();
                int size = range.size();
                if (range.getStepInt() != 1)
                    throw new ClassCastException("step of index range must be 1");
                if (list instanceof FString && value instanceof CharSequence) {
                    CharSequence sval = (CharSequence) value;
                    ((FString) list).replace(sval, 0, sval.length(),
                                             istart, istart+size);
                } else
                    Sequences.replace(list, istart, istart+size,
                                      Sequences.coerceToSequence(value));
            } else {
                if (elementType != null)
                    value = elementType.coerceToObject(value);
                int ind = ((Number) index).intValue();
                if (list instanceof Sequence)
                    ((Sequence) list).set(ind, value);
                else
                    list.set(ind, value);
            }
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
            Array lhs = (Array)
                ComposedArray.generalIndex(array, true, 0, dim, args);
            Object value = args[dim];
            if (lhs.rank() == 0 && ! (value instanceof Array))
                lhs.set(AbstractSequence.noInts, value);
            else
                gnu.lists.Arrays.copy(lhs, (Array) value);
            return Values.empty;
        }
    }
}
