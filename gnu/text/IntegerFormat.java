// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.text.FieldPosition;
import java.text.Format;
import java.io.Writer;

/** Handle formatting of integers.
 * Used to implement the Common Lisp ~D (Decimal), ~X (Hexadecimal),
 * ~O (Octal), ~B (Binary), and ~R (Radix) Common Lisp formats operators. */

public class IntegerFormat extends ReportFormat
{
  public int base;
  public int minWidth;
  public int padChar;
  public int commaChar;
  public int commaInterval;

  public int flags;
  /** Do groups (for example thousands, using commas). */
  public static final int SHOW_GROUPS = 1;

  /** If value is non-negative, emit a '+'. */
  public static final int SHOW_PLUS = 2;

  /** If value is non-negative, emit an initial ' '. */
  public static final int SHOW_SPACE = 4;

  /** Add "0x" (hex) or "0" (octal) prefix. */
  public static final int SHOW_BASE = 8;

  public static final int PAD_RIGHT = 16;

  public static final int UPPERCASE = 32;

  public IntegerFormat ()
  {
    base = 10;
    minWidth = 1;
    padChar = (int) ' ';
    commaChar = (int) ',';
    commaInterval = 3;
    flags = 0;
  }

  public int format(Object[] args, int start, 
		    Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    return format((Object) args, start, dst, fpos);
  }

  public int format(Object arg, int start, 
		    Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    Object[] args = arg instanceof Object[] ? (Object[]) arg : null;
    int minWidth =  getParam(this.minWidth, 1, args, start);
    if (this.minWidth == PARAM_FROM_LIST)  start++;
    char padChar = getParam(this.padChar, ' ', args, start);
    if (this.padChar == PARAM_FROM_LIST)  start++;
    char commaChar = getParam(this.commaChar, ',', args, start);
    if (this.commaChar == PARAM_FROM_LIST)  start++;
    int commaInterval = getParam(this.commaInterval, 3, args,start);
    if (this.commaInterval == PARAM_FROM_LIST)  start++;
    boolean printCommas = (flags & SHOW_GROUPS) != 0;
    boolean padRight = (flags & PAD_RIGHT) != 0;
    boolean padInternal = padChar == '0';
    if (args != null)
      arg = args[start];
    String sarg = convertToIntegerString(arg, base);
    if (sarg != null)
      {
        char sarg0 = sarg.charAt(0);
	boolean neg = sarg0 == '-';
	int slen = sarg.length();
	int ndigits = neg ? slen - 1 : slen;
	int numCommas = printCommas ? (ndigits-1)/commaInterval : 0;
	int unpadded_len = ndigits + numCommas;
	if (neg || (flags & (SHOW_PLUS|SHOW_SPACE)) != 0)
	  unpadded_len++;
        if ((flags & SHOW_BASE) != 0)
          {
            if (base == 16)
              unpadded_len += 2;
            else if (base == 8 && sarg0 != '0')
              unpadded_len += 1;
          }
        if (! padRight && ! padInternal)
          for (; minWidth > unpadded_len;  --minWidth)
            dst.write(padChar);
	int i = 0;
	if (neg)
	  {
	    dst.write('-');
	    i++;
	    slen--;
	  }
	else if ((flags & SHOW_PLUS) != 0)
	  dst.write('+');
	else if ((flags & SHOW_SPACE) != 0)
	  dst.write(' ');
        boolean uppercase = base > 10 && (flags & UPPERCASE) != 0;
        if ((flags & SHOW_BASE) != 0)
          {
            if (base == 16)
              {
                dst.write('0');
                dst.write(uppercase ? 'X' : 'x');
              }
            else if (base == 8 && sarg0 != '0')
              dst.write('0');
          }
        if (padInternal)
          for (; minWidth > unpadded_len;  --minWidth)
            dst.write(padChar);
	for (;;)
	  {
            char ch = sarg.charAt(i++);
            if (uppercase)
              ch = Character.toUpperCase(ch);
	    dst.write(ch);
	    if (--slen == 0)
	      break;
	    if (printCommas && (slen % commaInterval) == 0)
	      dst.write(commaChar);
	  }
        if (padRight)
          for (; minWidth > unpadded_len;  --minWidth)
            dst.write(padChar);
      }
    else
      print(dst, arg.toString());
    return start + 1;
  }

  public String convertToIntegerString(Object x, int radix)
  {
    if (! (x instanceof Number))
      return null;
    else if (x instanceof java.math.BigInteger)
      return ((java.math.BigInteger) x).toString(radix);
    else
      return Long.toString(((Number) x).longValue(), radix);
  }
}
