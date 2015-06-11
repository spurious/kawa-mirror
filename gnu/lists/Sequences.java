package gnu.lists;
import java.util.*;
import gnu.text.Char;

public class Sequences {

    /** Get an Iterator for a "sequence-like" object.
     * This handles Iterables, CharSequences, and Java arrays.
     * A CharSequences is treated as a sequence of (20-bit) code-points,
     * not 16-bit char values.
     */
    public static Iterator getIterator(Object object) {
        if (object instanceof CharSequence)
            return new CharacterIterator((CharSequence) object);
        if (! (object instanceof Iterable)) {
            if (object instanceof Object[])
                return new FVector((Object[]) object).iterator();
            if (object instanceof long[])
                return new S64Vector((long[]) object).iterator();
            if (object instanceof int[])
                return new S32Vector((int[]) object).iterator();
            if (object instanceof short[])
                return new S16Vector((short[]) object).iterator();
            if (object instanceof byte[])
                return new S8Vector((byte[]) object).iterator();
            if (object instanceof double[])
                return new F64Vector((double[]) object).iterator();
            if (object instanceof float[])
                return new F32Vector((float[]) object).iterator();
            if (object instanceof boolean[])
                return new BitVector((boolean[]) object).iterator();
            if (object instanceof char[])
                return new CharVector((char[]) object).iterator();
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
}
