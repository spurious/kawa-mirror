package kawa.math;

/** This contains various low-level routines for unsigned bigints.
 * The interfaces match the mpn interfaces in gmp,
 * so it should be easy to replace them with fast native functions
 * that are trivial wrappers around the mpn_ functions in gmp
 * (at least on platforms that use 32-bit "limbs").
 */

class MPN
{
  /** Add x[0:size-1] and y, and write the size least
   * significant words of the result to dest.
   * Return carry, either 0 or 1.
   * All values are unsigned.
   * This is basically the same as gmp's mpn_add_1. */
  public static int add_1 (int[] dest, int[] x, int size, int y)
  {
    long carry = (long) y & 0xffffffffL;
    for (int i = 0;  i < size;  i++)
      {
	carry += ((long) x[i] & 0xffffffffL);
	dest[i] = (int) carry;
	carry >>= 32;
      }
    return (int) carry;
  }

  /** Add x[0:len-1] and y[0:len-1] and write the len least
   * significant words of the result to dest[0:len-1].
   * All words are traeted as unsigned.
   * @return the carry, either 0 or 1
   * This function is basically the same as gmp's mpn_add_n.
   */
  public static int add_n (int dest[], int[] x, int[] y, int len)
  {
    long carry = 0;
    for (int i = 0; i < len;  i++)
      {
	carry = ((long) x[i] & (long) 0xffffffffL) +
	  ((long) y[i] & (long) 0xffffffffL) + carry;
	dest[i] = (int) carry;
	carry >>= 32;
      }
    return (int) carry;
  }

  /** Multiply x[0:len-1] by y, and write the len least
   * significant words of the product to dest[0:len-1].
   * Return the most significant words of the product.
   * All values are treated as if they were unsigned
   * (i.e. masked with 0xffffffffL).
   * This function is basically the same as gmp's mpn_mul_1.
   */

  public static int mul_1 (int[] dest, int[] x, int len, int y)
  {
    long yword = (long) y & 0xffffffffL;
    long carry = 0;
    for (int j = 0;  j < len; j++)
      {
        carry += ((long) x[j] & 0xffffffffL) * yword;
        dest[j] = (int) carry;
        carry >>= 32;
      }
    return (int) carry;
  }

  /**
   * Multiply x[0:xlen-1] and y[0:ylen-1], and
   * write the result to dest[0:xlen+ylen-1].
   * The destination has to have space for xlen+ylen words,
   * even if the result might be one limb smaller.
   * This function requires that xlen >= ylen.
   * The destination must be distinct from either input operands.
   * All operands are unsigned.
   * This function is basically the same gmp's mpn_mul. */

  public static void mul (int[] dest,
			  int[] x, int xlen,
			  int[] y, int ylen)
  {
    dest[xlen] = MPN.mul_1 (dest, x, xlen, y[0]);

    for (int i = 1;  i < ylen; i++)
      {
	long yword = (long) y[i] & 0xffffffffL;
	long carry = 0;
	for (int j = 0;  j < xlen; j++)
	  {
	    carry += ((long) x[j] & 0xffffffffL) * yword;
	    dest[i+j] += (int) carry;
	    carry >>= 32;
	  }
	dest[i+xlen] = (int) carry;
      }
  }

  /** Number of digits in the conversion base that always fits in a word.
   * For example, for base 10 this is 9, since 10**9 is the
   * largest number that fits into a words (assuming 32-bit words).
   * This is the same as gmp's __mp_bases[radix].chars_per_limb.
   * @param radix the base
   * @return number of digits */
  public static int chars_per_word (int radix)
  {
    if (radix < 10)
      {
	if (radix < 8)
	  {
	    if (radix <= 2)
	      return 32;
	    else if (radix == 3)
	      return 20;
	    else if (radix == 4)
	      return 16;
	    else
	      return 18 - radix;
	  }
	else
	  return 10;
      }
    else if (radix < 12)
      return 9;
    else if (radix <= 16)
      return 8;
    else if (radix <= 23)
      return 7;
    else if (radix <= 40)
      return 6;
    // The following are conservative, but we don't care.
    else if (radix <= 256)
      return 4;	     
    else
      return 1;
  }

  /** radix**chars_per_word, i.e. the biggest number that fits a word,
   * built by factors of base.  Exception: For 2, 4, 8, etc,
   * big_base is log2(base), i.e. the number of bits used to represent
   * each digit in the base.
   * Same as gmp's __mp_bases[radix].big_base.
   */
  static public int big_base (int radix)
  {
    if (radix == 10)
      return 0x3b9aca00;
    else if (radix == 16)
      return 4;
    else if ((radix & (radix - 1)) == 0)
      {
	for (int i = 0; ; i++)
	  {
	    radix >>= 1;
	    if (radix == 0)
	      return i;
	  }
      }
    else
      {
	int power = 1;
	for (int i = radix;  --i >= 0; )
	  power *= radix;
	return power;
      }
  }

  public static int set_str (int dest[], byte[] str, int str_len, int base)
  {
    int big_base = MPN.big_base (base);
    int size = 0;
    if ((base & (base - 1)) == 0)
      {
	// The base is a power of 2.  Read the input string from
	// least to most significant character/digit.  */
 
	int next_bitpos = 0;
	int bits_per_indigit = big_base;
	int res_digit = 0;
 
	for (int i = str_len;  --i >= 0; )
	  {
	    int inp_digit = str[i];
	    res_digit |= inp_digit << next_bitpos;
	    next_bitpos += bits_per_indigit;
	    if (next_bitpos >= 32)
	      {
		dest[size++] = res_digit;
		next_bitpos -= 32;
		res_digit = inp_digit >> (bits_per_indigit - next_bitpos);
	      }
	  }
 
	if (res_digit != 0)
	  dest[size++] = res_digit;
      }
    else
      {
	// General case.  The base is not a power of 2.
	int indigits_per_limb = MPN.chars_per_word (base);
	int j;
	int cy_limb;
	int str_pos = 0;
	int res_digit;
	int i;
 
	for (i = indigits_per_limb ; i < str_len; i += indigits_per_limb)
	  {
	    res_digit = str[str_pos++];
	    for (j = 1; j < indigits_per_limb; j++)
	      res_digit = res_digit * base + str[str_pos++];
 
	    if (size == 0)
	      {
		if (res_digit != 0)
		  {
		    dest[0] = res_digit;
		    size = 1;
		  }
	      }
	    else
	      {
		cy_limb = MPN.mul_1 (dest, dest, size, big_base);
		cy_limb += MPN.add_1 (dest, dest, size, res_digit);
		if (cy_limb != 0)
		  dest[size++] = cy_limb;
	      }
	  }
 
        big_base = base;
	res_digit = str[str_pos++];
	for (j = 1; j < str_len - (i - indigits_per_limb); j++)
	  {
	    res_digit = res_digit * base + str[str_pos++];
	    big_base *= base;
	  }
      
	if (size == 0)
	  {
	    if (res_digit != 0)
	      {
		dest[0] = res_digit;
		size = 1;
	      }
	  }
	else
	  {
	    cy_limb = MPN.mul_1 (dest, dest, size, big_base);
	    cy_limb += MPN.add_1 (dest, dest, size, res_digit);
	    if (cy_limb != 0)
	      dest[size++] = cy_limb;
	  }
      }
    return size;
  }

  /** Compare x[0:size-1] with y[0:size-1], treating them as unsigned integers.
   * @result -1, 0, or 1 depending on if x<y, x==y, or x>y.
   * This is basically the same as gmp's mpn_cmp function.
   */
  public static int cmp (int[] x, int[] y, int size)
  {
System.err.print("MPN.comp(");
kawa.lang.print.print (x, System.err);
System.err.print(", ");
kawa.lang.print.print (y, System.err);
System.err.print(", "+size+")");
    while (--size >= 0)
      {
	int x_word = x[size];
	int y_word = y[size];
	if (x_word != y_word)
	  {
	    // Invert the high-order bit, because:
	    // (unsigned) X > (unsigned) Y iff
	    // (int) (X^0x80000000) > (int) (Y^0x80000000).
	    return (x_word ^ 0x80000000) > (y_word ^0x80000000) ? 1 : -1;
	  }
      }
    return 0;
  }
}
