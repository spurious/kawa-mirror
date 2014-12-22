package gnu.kawa.lispexpr;

import gnu.bytecode.*;
import java.util.List;

/** This matches a sequences of a specific length.
 * This is used for {@code [pat1 ... panN} patterns.
 */

public class SeqSizeType extends LangObjType {
    int requiredSize;
    boolean requiredExact;

    public SeqSizeType(String name, String implClass, int requiredSize, boolean requiredExact) {
        super(name, implClass, -1);
        this.requiredSize = requiredSize;
        this.requiredExact = requiredExact;
    }

    @Override
    public Object coerceFromObject (Object obj) {
        List list = (List) obj;
        int size = list.size();
        if (requiredExact ? size == requiredSize : size >= requiredSize)
            return list;
        throw new ClassCastException();
    }

    public static void checkSizeEq (java.util.List list, int requiredSize) {
        int sz = list.size();
        if (sz != requiredSize)
            throw new ClassCastException("sequence has size "+sz+" should be "+requiredSize);
    }

    public static void checkSizeGe (java.util.List list, int requiredSize) {
        int sz = list.size();
        if (sz < requiredSize)
            throw new ClassCastException("sequence has size "+sz+" should be at least "+requiredSize);
    }

    public static java.util.List coerceEqOrNull(Object object, int requiredSize) {
        if (object instanceof List) {
            List list = (List) object;
            if (list.size() == requiredSize)
                return list;
        }
        return null;
    }

    public static java.util.List coerceGeOrNull(Object object, int requiredSize) {
        if (object instanceof List) {
            List list = (List) object;
            if (list.size() >= requiredSize)
                return list;
        }
        return null;
    }
 
    @Override
    public void emitCoerceFromObject (CodeAttr code) {
        code.emitCheckcast(implementationType);
        code.emitDup();
        code.emitPushInt(requiredSize);
        ClassType thisCl = ClassType.make("gnu.kawa.lispexpr.SeqSizeType");
        code.emitInvokeStatic(thisCl.getDeclaredMethod(requiredExact ? "checkSizeEq" : "checkSizeGe", 2));
    }

    public boolean emitCoercionOrNull(CodeAttr code) {
        ClassType thisCl = ClassType.make("gnu.kawa.lispexpr.SeqSizeType");
        code.emitPushInt(requiredSize);
        code.emitInvokeStatic(thisCl.getDeclaredMethod(requiredExact ? "coerceEqOrNull" : "coerceGeOrNull", 2));
        return true;
    }

    @Override
    public int isCompatibleWithValue(Type valueType) {
        return Type.isCompatibleWithValue(this, valueType);
    }
   
    @Override
    public int compare(Type other) {
        if (other instanceof SeqSizeType) {
            SeqSizeType sother = (SeqSizeType) other;
            if (requiredSize != sother.requiredSize) {
                if ((requiredExact && sother.requiredExact)
                    || (requiredExact && requiredSize < sother.requiredSize)
                    || (sother.requiredExact
                        && requiredSize > sother.requiredSize)) {
                    return -3;
                }
            }
            if (getImplementationType() == sother.getImplementationType()) {
                if (requiredSize == sother.requiredSize
                    && requiredExact && sother.requiredExact)
                    return 0;
                if (requiredSize < sother.requiredSize && !requiredExact)
                    return 1;
                if (requiredSize > sother.requiredSize && !sother.requiredExact)
                    return -1;
            }
        }
        int r = getImplementationType().compare(other);
        return r == 0 || r == -1 ? -1 : r == -3 ? -3 : -2;
    }
}
