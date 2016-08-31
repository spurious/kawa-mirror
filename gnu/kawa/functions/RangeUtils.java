package gnu.kawa.functions;

import gnu.lists.*;
import gnu.lists.Range.IntRange;
import gnu.kawa.functions.AddOp;
import gnu.kawa.functions.DivideOp;
import gnu.kawa.functions.MultiplyOp;
import gnu.kawa.lispexpr.LangObjType;
import gnu.math.IntNum;
import gnu.math.RealNum;

public class RangeUtils {

    public static Range<?> valueOfUnbounded(Object start, Object step) {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        IntNum iistep = IntNum.asIntNumOrNull(step);
        if (iistart != null
            && iistart.inIntRange()
            && iistep.inIntRange()) {
             return new IntRange(iistart.intValue(), iistep.intValue());
        }
        return new Range<Object>(start, step, -1);
    }

    public static Range<?> upto(Object start, Object step, Object end, boolean orEqual)
    throws Throwable {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        RealNum rstep = LangObjType.coerceRealNum(step);
        if (rstep.sign() <= 0)
            throw new ClassCastException("step value "+rstep+" is not positive");
        IntNum iistep = rstep instanceof IntNum ? (IntNum) rstep : null;
        IntNum iiend = IntNum.asIntNumOrNull(end);
        if (iistart != null && iiend != null && iistep != null
            && iistart.inIntRange()
            && IntNum.compare(iistep, Integer.MAX_VALUE) <= 0) {
            int istart = iistart.intValue();
            int istep = iistep.intValue();
            IntNum size = IntNum.sub(iiend, iistart);
            if (istep != 1)
                size = IntNum.quotient(size, iistep,
                                       orEqual ? IntNum.TRUNCATE : IntNum.CEILING);
            int isize;
            if (size.sign() < 0)
                isize = 0;
            else if (IntNum.compare(size, Integer.MAX_VALUE - (orEqual ? 1 : 0)) > 0)
                throw new IndexOutOfBoundsException("size too large");
            else
                isize = size.intValue() + (orEqual ? 1 : 0);
            return new IntRange(istart, istep, isize);
        }
        
        Object size = AddOp.$Mn(end, start);
        if (iistep == null || ! iistep.isOne())
            size = (orEqual ? DivideOp.idiv : DivideOp.iceil).apply2(size, step);
        if (size instanceof Number)
            return new Range<Object>(start, step,
                                     ((Number) size).intValue() + (orEqual ? 1 : 0));
        throw new IndexOutOfBoundsException("start index "+start+" is greater than end index "+end);
    }

    public static Range<?> downto(Object start, Object step, Object end, boolean orEqual)
    throws Throwable {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        RealNum rstep = LangObjType.coerceRealNum(step);
        if (rstep.sign() >= 0)
            throw new ClassCastException("step value "+rstep+" is not negative");
        IntNum iistep = rstep instanceof IntNum ? (IntNum) rstep : null;
        IntNum iiend = IntNum.asIntNumOrNull(end);
        if (iistart != null && iiend != null && iistep != null
            && iistart.inIntRange()
            && IntNum.compare(iistep, Integer.MIN_VALUE) >= 0) {
            int istart = iistart.intValue();
            int istep = iistep.intValue();
            IntNum size = IntNum.sub(iistart, iiend);
            if (istep != -1)
                size = IntNum.quotient(size, IntNum.neg(iistep),
                                       orEqual ? IntNum.TRUNCATE : IntNum.CEILING);
            int isize;
            if (size.sign() < 0)
                isize = 0;
            else if (IntNum.compare(size, Integer.MAX_VALUE - (orEqual ? 1 : 0)) > 0)
                throw new IndexOutOfBoundsException("size too large");
            else
                isize = size.intValue() + (orEqual ? 1 : 0);
            return new IntRange(istart, istep, isize);
        }
        
        Object size = AddOp.$Mn(start, end);
        if (iistep == null || ! iistep.isMinusOne())
            size = (orEqual ? DivideOp.idiv : DivideOp.iceil).apply2(size, AddOp.$Mn(step));
        if (size instanceof Number)
            return new Range<Object>(start, step,
                                     ((Number) size).intValue() + (orEqual ? 1 : 0));
        throw new IndexOutOfBoundsException("start index "+start+" is greater than end index "+end);
    }

    public static Range<?> bySize(Object start, Object step, Object size)
    throws Throwable {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        RealNum rstep = LangObjType.coerceRealNum(step);
        IntNum iistep = rstep instanceof IntNum ? (IntNum) rstep : null;
        IntNum iisize = LangObjType.coerceIntNum(size);
        if (! iisize.inRange(0, Integer.MAX_VALUE))
            new IndexOutOfBoundsException("invalid size (negative or too big)");
        int isize = iisize.intValue();
        if (iistart != null && iistep != null
            && iistart.inIntRange() && iistep.inIntRange()) {
            return new IntRange(iistart.intValue(), iistep.intValue(), isize);
        }
        return new Range(start, rstep, isize);
    }

}
