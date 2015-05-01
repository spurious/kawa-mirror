package gnu.kawa.util;

/**Supports computing a hash-code while protecting against cycles. 
 */
public interface BoundedHashable {
    /** Calculate a hash code for this object.
     * @param seed The seed is an initial value, or the accumulated
     *    hash code from previous elements in a containing object.
     *    Using zero as the seed is fine.
     * @param limit A limit on the number of sub-elements whose hash we
     *   should calculate.  This guards against cycles.  Any recursive calls
     *   should be done with a smaller value of limit, and no recursive
     *   calls must be done when the limit is zero.
     * @return A well-dispersed hash code.
     * The result is not compatible with {@code Object#hashCode}.
     */
    public int boundedHash(int seed, int limit);
}
