package gnu.kawa.util;

import java.util.List;
import gnu.lists.Array;

/** Various static helper methods for calculating hash-codes.
 * These are designed to tolerate cyclic data structrues, by using
 * a recursive/iteration limit.
 *
 * The actual hash functions used (unless we fall back to Object#hashCode)
 * is the MurmurHash3 algorithm:
 * http://en.wikipedia.org/wiki/MurmurHash
 * https://code.google.com/p/smhasher/wiki/MurmurHash3
 */

public class HashUtils {

    public static int boundedHash(Object object) {
        return boundedHash(object, 0, 50);
    }

    /** Generic hash method.
     * See BoundedHashable for a discussion of the parameters.
     */
    public static int boundedHash(Object object, int seed, int limit) {
        // Check CharSequence first, because we want FString to use the
        // CharSequence algorithm, not the AbstractSequence or List one.
        // We want to use the String.hashCode implementation for String
        // (because at least in OpenJDK the hash is cashed), and for
        // consistency we want the same algorithm for all CharSequences.
        // We do want to apply to Murmur3 hashInt algorithm for better
        // distribution of the bits produced by hashCode.
        if (object == null)
            return hashInt(seed, 0);
        if (object instanceof CharSequence)
            ; // fall through to default
        else if (object instanceof BoundedHashable)
            return ((BoundedHashable) object).boundedHash(seed, limit);
        else if (object instanceof List)
            return boundedHash((List) object, seed, limit);
        else if (object.getClass().isArray())
            return boundedHashArray(object, seed, limit);
        else if (object instanceof Array)
            return boundedHash((Array) object, seed, limit);
        return hashInt(seed, object.hashCode());
    }

    public static int hashInt(int seed, int value) {
        return murmur3finish(murmur3step(seed, value), 4);
    }

    /*
    public static int boundedHash(CharSequence seq, int seed, int limit) {
        int len = seq.length();
        // i0 counts even-numbered chars from start.
        // i1 counts odd-numbered chars from end.
        // This is non-optimial for caching, but helps sample better,
        // especially if we stop before sampling all the characters.
        int i0 = 0, i1 = (len&1) != 0 ? len - 2 : len - 1;
        for (; --limit >= 0 && i1 > 0; i0 += 2, i1 -= 2) {
            int v = (seq.charAt(i0) << 16) | seq.charAt(i1);
            seed = murmur3step(seed, v);
        }
        if (limit > 0 && i0 < len)
            seed = murmur3step(seed, seq.charAt(i0) << 16);
        return murmur3finish(seed, len);
    }
    */

    public static int boundedHash(List object, int seed, int limit) {
        int count = 0;
        int sublimit = limit >> 1;
        for (Object obj : object) {
            if (++count > limit)
                break;
            seed = murmur3step(seed, boundedHash(obj, 0, sublimit));
        }
        return murmur3finish(seed, count);
    }

    public static int boundedHashArray(Object object, int seed, int limit) {
        int count = 0;
        int sublimit = limit >> 1;
        int length = java.lang.reflect.Array.getLength(object);
        for (int i = 0; i < length; i++) {
            if (++count > limit)
                break;
            Object element = java.lang.reflect.Array.get(object, i);
            seed = murmur3step(seed, boundedHash(element, 0, sublimit));
        }
        return murmur3finish(seed, count);
    }

    public static int boundedHash(Array arr, int seed, int limit) {
        int rank = arr.rank();
        int[] indexes = new int[rank];
        int size = 1;
        for (int r = 0; r < rank; r++) {
            indexes[r] = arr.getLowBound(r);
            size *= arr.getSize(r);
        }
        int count = 0;
        int sublimit = limit >> 1;
        for (int i = 0; i < size; i++) {
            if (++count > limit)
                break;
            Object element = arr.get(indexes);
            seed = murmur3step(seed, boundedHash(element, 0, sublimit));
            gnu.lists.Arrays.incrementIndexes(indexes, arr);
        }
       return murmur3finish(seed, count);
    }

    public static int murmur3step(int h1/*seed*/, int k1/*datum*/) {
        k1 *= 0xcc9e2d51;
        h1 ^= ((k1 << 15) | (k1 >>> 17)) * 0x1b873593;
        return ((h1 << 13) | (h1 >>> 19)) * 5 + 0xe6546b64;
    }

    public static int murmur3finish(int hash, int length) {
        hash ^= length;
        hash = (hash ^ (hash >>> 16)) * 0x85ebca6b;
        hash = (hash ^ (hash >>> 13)) * 0xc2b2ae35;
        return hash ^ (hash >>> 16);
    }
}
