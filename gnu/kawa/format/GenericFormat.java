package gnu.kawa.format;

import gnu.lists.Consumer;
import java.util.*;

/** A format that delegates to a set of type-specific handlers. */

public class GenericFormat extends AbstractFormat {
    protected AbstractFormat next;

    List<Entry> entries = new ArrayList<Entry>();

    Map<Class, Object[]> map = new HashMap<Class,Object[]>();

    public GenericFormat() { }
    public GenericFormat(AbstractFormat next) { this.next = next; }

    /** Add an entry.
     * More recently-added entries take precedence.
     */
    public void add(Entry entry) { entries.add(entry); }

    public void add(Class cls, String mname) {
        add(Entry.valueOf(cls, mname)); }

    /** Same as plain add, but invalidates class-based lookup cache
     * for subtypes of cls.
     */
    public void addInvalidatingCache(Entry entry, Class cls) {
        invalidateCache(cls);
        entries.add(entry); 
    }

    public void invalidateCache(Class cls) {
        for (Iterator<Class> it = map.keySet().iterator();
             it.hasNext(); ) {
            Class key = it.next();
            if (cls.isAssignableFrom(key))
                it.remove();
        }
    }

    public void writeObject(Object value, Consumer out) {
        GenericFormat curFormat = this;
        for (;;) {
            if (curFormat.tryFormat(value, this, out))
                return;
            AbstractFormat next = curFormat.next;
            if (next instanceof GenericFormat)
                curFormat = (GenericFormat) next;
            else {
                if (next != null)
                    next.writeObject(value, out);
                else
                    out.write(value == null ? "(null)" : value.toString());
                return;
            }
        }
    }

    public boolean tryFormat(Object value, AbstractFormat format, Consumer out) {
        Class cls = value == null ? Object.class : value.getClass();
        Object[] cache = map.get(cls);
        // index in cache - increasing from newest entry to oldest
        // The cache is an array of potential entries (a subset of
        // the entries list) in reverse order, followed by an Integer,
        // which is the index of the last entry tested.
        int j = 0;
        // index in entries - oldest entry (lowest index)
        // tested (so far) with tryFormat
        int oldestEntry;
        if (cache != null) {
            for (;;) {
                Object entry = cache[j];
                if (entry instanceof Entry) {
                    TryFormatResult res =
                        ((Entry) entry).tryFormat(value, format, out);
                    if (res == TryFormatResult.HANDLED)
                        return true;
                    j++;
                } else {
                    oldestEntry = (Integer) entry;
                    break;
                }
            }
        } else {
            oldestEntry = entries.size();
            cache = new Object[8];
            j = 0;
        }
        for (int i = oldestEntry; --i >= 0;) {
            Entry entry = entries.get(i);
            TryFormatResult res = entry.tryFormat(value, format, out);
            if (res == TryFormatResult.INVALID_CLASS)
                continue;
            if (j + 2 >= cache.length) {
                Object[] tmp = new Object[(3 * cache.length) >> 1];
                System.arraycopy(cache, 0, tmp, 0, cache.length);
                cache = tmp;
            }
            cache[j++] = entry;
            if (res == TryFormatResult.HANDLED) {
                cache[j++] = i;
                map.put(cls, cache);
                return true;
            }
        }
        cache[j++] = 0;
        map.put(cls, cache);
        return false;
    }

    public enum TryFormatResult {
        /** Returned when the handler does not accept any element of the
         * value's class. */
        INVALID_CLASS,
        INVALID,
        HANDLED
    };

    public static class Entry {
        public static Entry defaultInstance = new Entry();

        /** Try to print value on out.
         * Assume the result only depends of the *class* of value and the format.
         */
        public TryFormatResult tryFormat(Object value, AbstractFormat format, Consumer out) {
            out.write(value == null ? "(null)" : value.toString());
            return TryFormatResult.HANDLED;
        }
        public static Entry valueOf(Class cls, String mname) {
            MethodEntry entry = new MethodEntry();
            try {
                /* #ifdef use:java.lang.invoke */
                entry.method =  java.lang.invoke.MethodHandles.lookup()
                    .findStatic(cls, mname, MethodEntry.mtype);
                /* #else */
                // entry.method = cls.getDeclaredMethod(mname, MethodEntry.mtype);
                /* #endif */
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
            return entry;
        }
    }

    public static class MethodEntry extends Entry {
        /* #ifdef use:java.lang.invoke */
        java.lang.invoke.MethodHandle method;
        static final java.lang.invoke.MethodType mtype =
            java.lang.invoke.MethodType.methodType(TryFormatResult.class,
                                                   Object.class,
                                                   AbstractFormat.class,
                                                   Consumer.class);
        /* #else */
        // java.lang.reflect.Method method;
        // private static final Class[] mtype =
        //     new Class[] { Object.class,
        //                    AbstractFormat.class,
        //                    Consumer.class };
        /* #endif */
        public TryFormatResult tryFormat(Object value, AbstractFormat format, Consumer out) {
            try {
                /* #ifdef use:java.lang.invoke */
                return (TryFormatResult) method.invokeExact(value, format, out);
                /* #else */
                // return (TryFormatResult)
                //     method.invoke(null, new Object[] { value, format, out });
                /* #endif */
            } catch (Throwable ex) {
                throw new RuntimeException(ex);
            }
        }
    }
}
