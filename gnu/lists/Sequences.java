package gnu.lists;

import gnu.text.Char;
import gnu.math.ULong;
import java.util.*;
import java.lang.reflect.Array;

public class Sequences {
    public static List asSequenceOrNull(Object value) {
        if (value instanceof List)
            return (List) value;
        if (value instanceof CharSequence) {
            CharSequence cseq = (CharSequence) value;
            return new SubCharSeq(cseq, 0, cseq.length());
        }
        if (value instanceof Object[])
            return new FVector((Object[]) value);
        SimpleVector vec = null;
        if (value.getClass().isArray()) {
            if (value instanceof long[])
                vec = new S64Vector((long[]) value);
            else if (value instanceof int[])
                vec = new S32Vector((int[]) value);
            else if (value instanceof short[])
                vec = new S16Vector((short[]) value);
            else if (value instanceof byte[])
                vec = new S8Vector((byte[]) value);
            else if (value instanceof double[])
                vec = new F64Vector((double[]) value);
            else if (value instanceof float[])
                vec = new F32Vector((float[]) value);
            else if (value instanceof boolean[])
                vec = new BitVector((boolean[]) value);
            else if (value instanceof char[])
                vec = new CharVector((char[]) value);
            if (vec != null) {
                vec.info |= SimpleVector.SHARED_FLAG;
                return vec;
            }
        }
        return null;
    }

    public static IntSequence asIntSequenceOrNull(Object value) {
        List lst = asSequenceOrNull(value);
        if (lst == null)
            return null;
        if (lst instanceof IntSequence)
            return (IntSequence) lst;
        int len = lst.size();
        int[] arr = new int[len];
        int i = 0;
        for (Object el : lst) {
            arr[i++] = ((Number) el).intValue();
        }
        return new S32Vector(arr);
    }

    public static List coerceToSequence(Object value) {
        List lst = asSequenceOrNull(value);
        if (lst == null) {
            String msg;
            if (value == null)
                msg = "null is not a sequence";
            else
                msg = "cannot cast a "+value.getClass().getName()+" to a sequence";
            throw new ClassCastException(msg);
        }
        return lst;
    }

    public static int getSize(Object values) {
        if (values instanceof Object[])
            return ((Object[]) values).length;
        else if (values instanceof CharSequence)
            return ((CharSequence) values).length();
        else if (values instanceof List<?>)
            return ((List<?>) values).size();
        else if (values.getClass().isArray())
            return Array.getLength(values);
        else
            throw new ClassCastException("value is neither List or array");
    }

    /** Get an Iterator for a "sequence-like" object.
     * This handles Iterables, CharSequences, and Java arrays.
     * A CharSequences is treated as a sequence of (20-bit) code-points,
     * not 16-bit char values.
     */
    public static Iterator getIterator(Object object) {
        if (object instanceof CharSequence)
            return new CharacterIterator((CharSequence) object);
        if (! (object instanceof Iterable)) {
            List list = asSequenceOrNull(object);
            if (list != null)
                return list.iterator();
        }
        // Causes ClassCastException if neither Iterable or otherwise handled.
        return ((Iterable) object).iterator();
    }

    /** Iterator subclass to iterate of CharSequences.
     * A CharSequences is treated as a sequence of (20-bit) code-points,
     * not 16-bit char values.
     */
    public static class CharacterIterator implements Iterator<Char> {
        CharSequence cseq;
        int len;
        int pos;
        public CharacterIterator(CharSequence cseq) {
            this.cseq = cseq;
            this.len = cseq.length();
        }
        public boolean hasNext() { return pos < len; }

        public Char next() {
            if (pos >= len)
                throw new NoSuchElementException();
            int ch1 = cseq.charAt(pos++);
            if (ch1 >= 0xD800 && ch1 <= 0xDBFF && pos < len) {
                int ch2 = cseq.charAt(pos);
                if (ch2 >= 0xDC00 && ch2 <= 0xDFFF) {
                    ch1 = ((ch1 - 0xD800) << 10)
                        + (ch2 - 0xDC00)
                        + 0x10000;
                    pos++;
                }
            }
            return Char.make(ch1);
        }

        /* #ifndef JAVA8 */
        public void remove() {
            throw new UnsupportedOperationException("remove");
        }
        /* #endif */
    }

    public static Object subList(Object base, int fromIndex, int toIndex) {
        List<?> lbase = (List<?>) base;
        if (toIndex == -1)
            toIndex = lbase.size();
        return lbase.subList(fromIndex, toIndex);
    }

    public static List indirectIndexed(List lst, IntSequence indexes) {
        /*
        if (lst instanceof SimpleVector)
            return ((SimpleVector) lst).select(indexes);
        */
        return new IndirectIndexedSeq(lst, indexes);
    }

    public static Object drop(Object base, int count) {
        if (count >= 0)
            return subList(base, count, -1);
        else
            return subList(base, 0, -count);
    }
    public static Object drop(Object base, int fromStart, int fromEnd) {
        List<?> lbase = (List<?>) base;
        return subList(base, fromStart, lbase.size() - fromEnd);
    }

    public static SimpleVector copy(SimpleVector base, int start, int end,
                                    boolean writable) {
        int sz = end - start;
        if (base.isVerySimple() || base.isSubRange()) {
            SimpleVector nvec = base.newInstance(-1);
            long flags = SimpleVector.SUBRANGE_FLAG;
            if (! writable)
                flags |= SimpleVector.READ_ONLY_FLAG;
            int baseStart, baseSize;
            if (base.isVerySimple()) {
                baseStart = 0;
                baseSize = base.size();
            } else {
                baseStart = base.getOffsetBits();
                baseSize = base.getSizeBits();
            }
            int off = baseStart + start;
            if (start < 0 || start > end || end > baseSize)
                throw new IndexOutOfBoundsException();
            // buffer[offset <: offset+size]
            nvec.setInfoField(sz, off, flags);
            base.info |= SimpleVector.COPY_ON_WRITE;
            return nvec;
        }
        return copy(base, new Range.IntRange(start, 1, sz), writable);
    }
                                    
    public static SimpleVector copy(List base, Range.IntRange range,
                                    boolean writable) {
        if (base instanceof SimpleVector && range.getStepInt() == 1) {
            SimpleVector svec = (SimpleVector) base;
            if (svec.isVerySimple() || svec.isSubRange()) {
                int start = range.getStartInt();
                int bsize = base.size();
                int end = range.isUnbounded() ? bsize
                    : start + range.getSize();
                if (start < 0 || end > bsize)
                    throw new IndexOutOfBoundsException();
                return copy(svec, start, end, writable);
            }
        }
        return Arrays.flattenCopy(new IndirectIndexedSeq(base, range),
                                  writable);
    }

    private static Object bufferForCopy(Object obj) {
        for (;;) {
            if (obj instanceof SimpleVector)
                return ((SimpleVector) obj).getBuffer();
            if (obj instanceof TransformedArray)
                obj = ((TransformedArray) obj).base;
            else
                return null;
        }
    }

    public static boolean copyInPlaceIsSafe(Object src, Object dst) {
        Object s = bufferForCopy(src);
        Object d = bufferForCopy(dst);
        return s != d && s != null && d != null;
    }

    public static void replace(List lst, int fromStart, int fromEnd,
                               List values) {
        if (lst instanceof SimpleVector && values instanceof SimpleVector) {
            SimpleVector svec = (SimpleVector) values;
            SimpleVector dvec = (SimpleVector) lst;
            if (svec.getTag() == dvec.getTag()) {
                int srcLength = svec.size();
                int dstLength = fromEnd - fromStart;
                int grow = srcLength - dstLength;
                if (grow > 0)
                    dvec.addSpace(fromEnd, grow);
                Object dbuffer = dvec.getBuffer();
                Object sbuffer = svec.getBuffer();
                int sstart;
                int dstart = dvec.getSegment(fromStart, srcLength);
                if (dstart >= 0
                    && (sstart = svec.getSegmentReadOnly(0, srcLength)) >= 0) {
                    System.arraycopy(sbuffer, sstart,
                                     dbuffer, dstart, srcLength);
                } else {
                    int srcStart = 0;
                    boolean copied = dbuffer == sbuffer;
                    if (copied)
                        sbuffer = svec.toDataArray();
                    while (srcLength > 0) {
                        int step = srcLength;
                        long dresult = dvec.getSegment(fromStart);
                        int dwhere = (int) dresult;
                        int dsize = (int) (dresult >> 32);
                        if (dsize < step)
                            step = dsize;
                        int swhere;
                        if (copied) {
                            swhere = srcStart;
                        } else {
                            long sresult = svec.getSegment(srcStart);
                            swhere = (int) sresult;
                            int ssize = (int) (sresult >> 32);
                            if (ssize < step)
                                step = ssize;
                        }
                        if (step == 0)
                            throw new Error("zero step in replace loop!");
                        System.arraycopy(sbuffer, swhere, dbuffer, dwhere,
                                         step);
                        srcLength -= step;
                        srcStart += step;
                        fromStart += step;
                    }
                }
                if (grow < 0)
                    dvec.delete(fromEnd+grow, fromEnd);
                return;
            }
        }
        int oldSize = fromEnd - fromStart;
        int newSize = values.size();
        // Making a copy is unfortunate, but it is hard to handle overlap
        // in the general case.  We do at least optimize SimpleVectors above.
        Object[] varray = values.toArray();
        int i = 0;
        for (Object el : varray) {
            if (i < oldSize)
                lst.set(fromStart+i, el);
            else
                lst.add(fromStart+i, el);
            i++;
        }
        if (i < oldSize) {
            if (lst instanceof AbstractSequence) {
                AbstractSequence alst = (AbstractSequence) lst;
                alst.removePos(alst.createPos(fromStart+i, false), oldSize - i);
            } else {
                while (i < oldSize) {
                    lst.remove(fromStart+i);
                    oldSize--;
                }
            }
        }
    }

    public static void writeUInt(int value, Consumer out) {
        if (value >= 0)
            out.writeInt(value);
        else
            out.writeLong((long) value & 0xffffffffl);
    }

    public static void writeULong(long value, Consumer out) {
        if (value >= 0)
            out.writeLong(value);
        else
            out.writeObject(ULong.valueOf(value));
    }
}
