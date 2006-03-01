// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;
import java.text.FieldPosition;

/** Format a real number using a floating-point format.
 * However, if `general' is true, and the number "fits",
 * use a fixed-point format (like printf %g).
 * Used for Common Lisp specs ~E and ~G;  also C-style %e and %g.
 */

public class ExponentialFormat extends java.text.Format
{
  /** Number of fractional digits to show.
   *  This is `d' in the CommonLisp spec. */
  public int fracDigits = -1;

  /** Number of digits to show in the integer part of the result.
   * If positive, The number of digits before the decimal point.
   * If negative, the -intDigits zeros are emitted after the decimal point.
  *  This is `k' in the CommonLisp spec. */
  public int intDigits;

  /** Number of digits to show in the exponent.
   * Zero means unspecified - show as many as needed. */
  public int expDigits;

  public char overflowChar;
  public char padChar;
  public char exponentChar = 'E';
  /** Display sign of exponent even when it is non-negative. */
  public boolean exponentShowSign;

  /** True if '+' should be printed for non-negative number. */
  public boolean showPlus;

  public int width;
  public boolean general;

  static final double LOG10 = Math.log(10);

  /** Add 1 to the integer in sbuf from digStart to digEnd.
   * @return if we overflowed. */
  static boolean addOne(StringBuffer sbuf, int digStart, int digEnd)
  {
    for (int j = digEnd ;  ; )
      {
	if (j == digStart)
	  {
	    sbuf.insert(j, '1');
	    return true;
	  }
	char ch = sbuf.charAt(--j);
	if (ch != '9')
	  {
	    sbuf.setCharAt(j, (char)((int) ch+1));
	    return false;
	  }
	sbuf.setCharAt(j, '0');
      }
  }


  public StringBuffer format(double value,
			     StringBuffer sbuf, FieldPosition fpos)
  {
    int k = intDigits;
    int d = fracDigits;
    boolean negative = value < 0;
    if (negative)
      value = -value;
    int oldLen = sbuf.length();
    int signLen = 1;
    if (negative)
      sbuf.append('-');
    else if (showPlus)
      sbuf.append('+');
    else
      signLen = 0;
    int log = (int) (Math.log(value) / LOG10);
    // Number of significant digits.
    int digits = d < 0 ? 17 : d + (k > 0 ? 1 : k);
    boolean showExponent = true;

    int digStart = sbuf.length();
    if (log == 0x80000000)
      log = digits + 1;
    int scale = digits - log + 1;
    IntNum ii = RealNum.toScaledInt(value, scale);
    ii.format(10, sbuf);
    int exponent = sbuf.length() - digStart - k - scale;
    int exponentAbs = exponent < 0 ? -exponent : exponent;
    int exponentLen = exponentAbs >= 1000 ? 4 : exponentAbs >= 100 ? 3
      : exponentAbs >= 10 ? 2 : 1;
    if (expDigits > exponentLen)
      exponentLen = expDigits;
    int ee = !general ? 0 : expDigits > 0 ? expDigits + 2 : 4;
    boolean fracUnspecified = d < 0;
    if (general || fracUnspecified)
      {
	int n = sbuf.length() - oldLen - scale;
	if (fracUnspecified)
	  {
	    d = n < 7 ? n : 7;
	    int q = sbuf.length();
	    char skip = '0';
	    int max_digits = width <= 0 ? 16
	      : width - signLen - exponentLen - 3;
	    if (max_digits > 0 && max_digits < q - digStart)
	      {
		if (sbuf.charAt(digStart + max_digits) >= '5')
		  skip = '9';
		q = digStart + max_digits;
	      }
	    while (q > digStart && sbuf.charAt(--q) == skip) ;
	    q = q + 1 - digStart;  // Number of digits needed.
	    if (q > d)
	      d = q;
	  }
	int dd = d - n;
	if (general && (n >= 0 && dd >= 0)) // FIXME || value == 0.0))
	  {
	    // "arg is printed as if by the format directives 
	    //    ~ww,dd,0,overflowchar,padcharF~ee@T "

	    // The following is not correct according to the letter
	    // of the CommonLisp spec, but seems to be in the spirit.
	    // If the fraction is zero, and there is room, add a '0'.
	    // (This also matches the Slib.)
	    if (dd == 0 && fracUnspecified
		&& (width <= 0 || n+signLen+1+ee < width))
	      d++;

	    digits = d;
	    k = n;
	    showExponent = false; 
	  }
	else if (fracUnspecified)
	  {
	    int avail = width - signLen - exponentLen - 3;
	    if (width <= 0)
	      digits = d;
	    else
	      {
		digits = avail;
		if (k < 0)
		  digits -= k;
		if (digits > d)
		  digits = d;
	      }
	    if (digits <= 0)
	      digits = 1;
	    if (digits == k && (width <= 0 || digits < avail))
	      digits++;
	  }
      }

    int digEnd = digStart + digits;
    while (sbuf.length() < digEnd)
      sbuf.insert(digStart, '0');

    // Now round to specified digits.
    char nextDigit = digEnd == sbuf.length() ? '0' : sbuf.charAt(digEnd);
    boolean addOne = nextDigit >= '5';
    //      || (nextDigit == '5'
    //	  && (Character.digit(sbuf.charAt(digEnd-1), 10) & 1) == 0);
    if (addOne && addOne(sbuf, digStart, digEnd))
      scale++;
    // Truncate excess digits, after adjusting scale accordingly.
    scale -= sbuf.length() - digEnd;
    sbuf.setLength(digEnd);

    if (k < 0)
      {
	// Insert extra zeros after '.'.
	for (int j = k;  ++j <= 0; )
	  sbuf.insert(digStart, '0');
      }
    else
      {
	// Insert extra zeros before '.', if needed.
	for (;  digStart+k > digEnd;  digEnd++)
	  sbuf.append('0');
      }
    sbuf.insert(k >= 0 ? digStart+k : digStart, '.');

    int newLen, i;
    if (showExponent)
      {
	// Append the exponent.
	sbuf.append(exponentChar);
        if (exponentShowSign || exponent < 0)
          sbuf.append(exponent >= 0 ? '+' : '-');
	i = sbuf.length();
	sbuf.append(exponentAbs);
	newLen = sbuf.length();
	int j = expDigits - (newLen - i);
	if (j > 0)
	  { // Insert extra exponent digits.
	    newLen += j;
	    while (--j >= 0)
	      sbuf.insert(i, '0');
	  }
      }
    else
      {
	if (true) // FIXME
	  { //  Pedantic according to the Common Lisp spec.
	    while (--ee >= 0 && (width <= 0 || sbuf.length() < oldLen + width))
	      sbuf.append(' ');
	  }
	else
	  { // This seems more reaonable:  Don't pad if width not specified!
	    if (width > 0)
	      while (--ee >= 0 && sbuf.length() < oldLen + width)
		sbuf.append(' ');
	  }
	newLen = sbuf.length();
      }
    int used = newLen - oldLen;
    i = width - used;
    if ((i >= 0 || width <= 0)
	&& ! (showExponent && exponentLen > expDigits
	      && expDigits > 0 && overflowChar != '\0'))
      {
	// Insert optional '0' before '.' if there is space.
	if (k <= 0 && (i > 0 || width <= 0))
	  {
	    sbuf.insert(digStart, '0');
	    --i;
	  }
	// Insert padding:
	while (--i >= 0)
	  sbuf.insert(oldLen, padChar);
      }
    else if (overflowChar != '\0')
      {
	sbuf.setLength(oldLen);
	for (i = width;  --i >= 0; )
	  sbuf.append(overflowChar);
     }
    return sbuf;
  }

  public StringBuffer format(long num, StringBuffer sbuf, FieldPosition fpos)
  {
    return format((double) num, sbuf, fpos);
  }

  public StringBuffer format(Object num, StringBuffer sbuf, FieldPosition fpos)
  {
    // Common Lisp says if value is non-real, print as if with ~wD.  FIXME.
    return format(((RealNum) num).doubleValue(), sbuf, fpos);
  }

  public java.lang.Number parse(String text, java.text.ParsePosition status)
  {
    throw new Error("ExponentialFormat.parse - not implemented");
  }
  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error("ExponentialFormat.parseObject - not implemented");
  }

}
