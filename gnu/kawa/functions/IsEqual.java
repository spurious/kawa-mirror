package gnu.kawa.functions;
import gnu.expr.Language;
import gnu.lists.*;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.IdentityHashMap;
import gnu.mapping.Promise;

/** Implement the standard Scheme procedure <tt>equal?</tt>
 * and the Lisp <tt>equal</tt>. */

public class IsEqual extends gnu.mapping.Procedure2
{
  Language language;

  public IsEqual(Language language, String name)
  {
    this.language = language;
    setName(name);
  }

  public static boolean numberEquals (Number num1, Number num2)
  {
    boolean exact1 = Arithmetic.isExact(num1);
    boolean exact2 = Arithmetic.isExact(num2);
    if (exact1 && exact2)
      return NumberCompare.$Eq(num1, num2);
    return exact1 == exact2 && num1.equals(num2);
  }

    static boolean arrayEquals (Object arg1, Object arg2,
                                Map<Object,ArrayList<Object>> map) {
    int len1 = Array.getLength(arg1);
    int len2 = Array.getLength(arg2);
    if (len1 != len2)
      return false;

    String tname1 = arg1.getClass().getName();
    String tname2 = arg2.getClass().getName();
    if (!tname1.equals(tname2))
      return false;

    if (tname1.length() == 2)   // matches primitive types
    {
      switch (tname1.charAt(1))
      {
        case 'Z':               // "[Z" ==> boolean[]
          return Arrays.equals((boolean[]) arg1, (boolean[]) arg2);
        case 'B':               // "[B" ==> byte[]
          return Arrays.equals((byte[]) arg1, (byte[]) arg2);
        case 'C':               // "[C" ==> char[]
          return Arrays.equals((char[]) arg1, (char[]) arg2);
        case 'D':               // "[D" ==> double[]
          return Arrays.equals((double[]) arg1, (double[]) arg2);
        case 'F':               // "[F" ==> float[]
          return Arrays.equals((float[]) arg1, (float[]) arg2);
        case 'I':               // "[I" ==> int[]
          return Arrays.equals((int[]) arg1, (int[]) arg2);
        case 'J':               // "[J" ==> long[]
          return Arrays.equals((long[]) arg1, (long[]) arg2);
        case 'S':               // "[S" ==> short[]
          return Arrays.equals((short[]) arg1, (short[]) arg2);
      }
    }
     if (noteEqual(arg1, arg2, map))
        return true;
    for (int i = len1; --i >= 0; )
    {
        if (! apply(Array.get(arg1,i), Array.get(arg2,i), map))
        return false;
    }
    return true;
  }

    /** Register the assumption that arg1 and arg are equal.
     * Consider comparing arg1==(car1 . cdr1) with arg2==(car2 . cdr2).
     * To do that we must recursively compare car1 with car2 and cdr1 with cdr2.
     * The problem is that any of those 4 subobjects might circularly
     * depend on arg1 or arg2, in which case we risk infinite recursion.
     * To avoid that, we tentativly assume that arg1==arg2, and register
     * that assemption in the map.  Then we compare car1 with car2
     * and cdr1 with cdr2.  If we hit a circularity, we will find the
     * tentative assumption that arg1==arg2, so we don't have to 
     * recursively check it.
     * The map maps from an object to a list of objects that
     * have been tentatively registered as equal.  The invariant is that
     * if arr==map.get(obj) then arr.contains(obj) and vice versa.
     * If map.get(obj1)==map.get(obj2) (and is non-null) then we have previously
     * tentatively assumed obj1 and obj2 are equal.
     * @return if arg1 and arg2 have already been registered as equal.
     */
    static boolean noteEqual(Object arg1, Object arg2,
                             Map<Object,ArrayList<Object>> map) {
        ArrayList<Object> set1 = map.get(arg1);
        ArrayList<Object> set2 = map.get(arg2);

        if (set1 == set2) {
            if (set1 != null)
                return true;
            ArrayList<Object> setn = new ArrayList<Object>();
            setn.add(arg1);
            setn.add(arg2);
            map.put(arg1, setn);
            map.put(arg2, setn);
        } else if (set1 == null) {
            set2.add(arg1);
            map.put(arg1, set2);
        } else if (set2 == null) {
            set1.add(arg2);
            map.put(arg2, set1);
        } else if (set1.size() > set2.size()) {
            for (Object x : set2) {
                set1.add(x);
                map.put(x, set1);
            }
        } else {
           for (Object x : set1) {
                set2.add(x);
                map.put(x, set2);
            }
        }
        return false;
    }

    public static boolean apply (Object arg1, Object arg2) {
        Map<Object,ArrayList<Object>> map =
            new IdentityHashMap<Object,ArrayList<Object>>();
        return apply(arg1, arg2, map);
    }

    public static boolean apply (Object arg1, Object arg2,
                                 Map<Object,ArrayList<Object>> map) {
        arg1 = Promise.force(arg1);
        arg2 = Promise.force(arg2);
        if (arg1 == arg2)
            return true;
        if (arg1 == null || arg2 == null)
            return false;
        if (arg1 instanceof Number || arg2 instanceof Number)
            return arg1 instanceof Number && arg2 instanceof Number
                && IsEqual.numberEquals((Number) arg1, (Number) arg2);

        if (arg1 instanceof CharSequence || arg2 instanceof CharSequence) {
            if (! (arg1 instanceof CharSequence
                   && arg2 instanceof CharSequence))
                return false;
            CharSequence seq1 = (CharSequence) arg1;
            CharSequence seq2 = (CharSequence) arg2;
            int len1 = seq1.length();
            int len2 = seq2.length();
            if (len1 != len2)
                return false;
            for (int i = len1;  --i >= 0; ) {
                if (seq1.charAt(i) != seq2.charAt(i))
                    return false;
            }
            return true;
        }

        if (arg1 instanceof LList || arg2 instanceof LList) {
            if (! (arg1 instanceof Pair && arg2 instanceof Pair))
                return false;
            Pair pair1 = (Pair) arg1;
            Pair pair2 = (Pair) arg2;
            for (;;) {
                if (noteEqual(pair1, pair2, map))
                    return true;
                if (! apply(pair1.getCar(), pair2.getCar(), map))
                    return false;
                Object x1 = pair1.getCdr();
                Object x2 = pair2.getCdr();
                if (! (x1 instanceof Pair) || !(x2 instanceof Pair))
                    return apply(x1, x2, map);
                pair1 = (Pair) x1;
                pair2 = (Pair) x2;
            }
        }

        // Compare java.util.List - all implementation types except LList
        // are considered equivalent.
        if (arg1 instanceof List || arg2 instanceof List) {
            if (! (arg1 instanceof List && arg2 instanceof List))
                return false;
            if (noteEqual(arg1, arg2, map))
                return true;
            ListIterator<?> e1 = ((List<?>) arg1).listIterator();
            ListIterator<?> e2 = ((List<?>) arg2).listIterator();
            while (e1.hasNext() && e2.hasNext()) {
                if (! apply(e1.next(), e2.next(), map))
                    return false;
            }
            return ! (e1.hasNext() || e2.hasNext());
        }

        boolean is1Array = arg1.getClass().isArray();
        boolean is2Array = arg2.getClass().isArray();
        if (is1Array || is2Array)
            return is1Array && is2Array &&
                IsEqual.arrayEquals(arg1, arg2, map);

        return arg1.equals(arg2);
    }

  public Object apply2 (Object arg1, Object arg2)
  {
    return language.booleanObject(apply(arg1, arg2));
  }
}
