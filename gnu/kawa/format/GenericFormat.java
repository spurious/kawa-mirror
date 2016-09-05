package gnu.kawa.format;

import gnu.lists.Consumer;
import java.util.*;

/** A format that delegates to a set of type-specific handlers. */

public class GenericFormat extends AbstractFormat {
    protected GenericFormat previous;

    List<Entry> entries = new ArrayList<Entry>();

    Map<Class, Entry> map = new HashMap<Class,Entry>();

    public GenericFormat() { }
    public GenericFormat(GenericFormat previous) { this.previous = previous; }

    /** Add an entry.
     * More recently-added entries take precedence.
     */
    public void add(Entry entry) { entries.add(entry); }

    public void add(Class cls, String mname) {
        add(Entry.valueOf(cls, mname)); }

    /** Same as plain add, but invalidates class-based lookup cache
     * for subtypes of cls.
     * Does not invalidate cache for GenericFormat that dpend on this;
     * i.e. those whose 'previous' chain points to this.
     */
    public void addInvalidatingCache(Entry entry, Class cls) {
        for (Iterator<Class> it = map.keySet().iterator();
             it.hasNext(); ) {
            Class key = it.next();
            if (cls.isAssignableFrom(key))
                it.remove();
        }
        entries.add(entry); 
    }

    public void writeObject(Object v, Consumer out) {
        Class cls = v == null ? Object.class : v.getClass();
        Entry entry = map.get(cls);
        if (entry != null
            && entry.handle(v, this, out)) // Should always be true
            return;
        GenericFormat curFormat = this;
        do {
            int nentries = curFormat.entries.size();
            
            for (int i = nentries; --i >= 0; ) {
                entry = curFormat.entries.get(i);
                if (entry.handle(v, this, out)) {
                    map.put(cls, entry);
                    return;
                }
            }
            curFormat = curFormat.previous;
        } while (curFormat != null);
        map.put(cls, entry);
        entry = Entry.defaultInstance;
        entry.handle(v, this, out);
        map.put(cls, entry);
    }

    public static class Entry {
        public static Entry defaultInstance = new Entry();

        /** Try to print value on out.
         * Assume the result only depends of the *class* of value and the format.
         */
        public boolean handle(Object value, AbstractFormat format, Consumer out) {
            out.write(value == null ? "(null)" : value.toString());
            return true;
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
            java.lang.invoke.MethodType.methodType(boolean.class,
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
        public boolean handle(Object value, AbstractFormat format, Consumer out) {
            try {
                /* #ifdef use:java.lang.invoke */
                return (boolean) method.invokeExact(value, format, out);
                /* #else */
                // return (Boolean)
                //     method.invoke(null, new Object[] { value, format, out });
                /* #endif */
            } catch (Throwable ex) {
                throw new RuntimeException(ex);
            }
        }
    }
}
