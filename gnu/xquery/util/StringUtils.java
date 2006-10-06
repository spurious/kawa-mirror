// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.xml.StringValue;
import gnu.kawa.xml.UntypedAtomic;
import java.util.regex.Pattern;

public class StringUtils
{
  public static Object lowerCase (Object node)
  {
    return StringValue.stringValue(node).toLowerCase();
  }

  public static Object upperCase (Object node)
  {
    return StringValue.stringValue(node).toUpperCase();
  }

  static double asDouble (Object value)
  {
    if (! (value instanceof Number))
      value = NumberValue.numberValue(value);
    return ((Number) value).doubleValue();
  }

  public static Object substring (Object str, Object start)
  {
    double d1 = asDouble(start);
    if (Double.isNaN(d1))
      return "";
    int i = (int) (d1 - 0.5);
    if (i < 0)
      i = 0;
    return StringValue.stringValue(str).substring(i);
  }

  public static Object substring (Object str, Object start, Object length)
  {
    double d1 = asDouble(start);
    double d2 = asDouble(length);
    if (Double.isNaN(d1) || Double.isNaN(d2))
      return "";
    int i = (int) (d1 - 0.5);
    int j = i + (int) (d2 + 0.5);
    if (i < 0)
      i = 0;
    String s = StringValue.stringValue(str);
    int len = s.length();
    if (j > len)
      j = len;
    else if (j <= i)
      return "";
    return s.substring(i, j);
  }

  public static Object stringLength (Object str)
  {
    return IntNum.make(StringValue.stringValue(str).length());
  }

  public static Object substringBefore (Object str, Object find)
  {
    String s = StringValue.stringValue(str);
    String f = StringValue.stringValue(find);
    int flen = f.length();

    if (flen==0)
      return "";
    int start = s.indexOf(f);
    return start >= 0 ? s.substring(0,start) : "";
  }

  public static Object substringAfter (Object str, Object find)
  {
    String s = StringValue.stringValue(str);
    String f = StringValue.stringValue(find);
    int flen = f.length();

    if (flen==0)
      return s;

    int start = s.indexOf(f);
    return start >= 0 ? s.substring(start+flen) : "";
  }

  public static Object translate (Object str, Object map, Object trans)
  {
    String m = StringValue.stringValue(map);
    int mlen = m.length();
    String sv = StringValue.stringValue(str);

    if (mlen==0) return sv;

    int slen = sv.length();
    StringBuffer s = new StringBuffer(slen);
    String t = StringValue.stringValue(trans);
    int tlen = t.length();

    for (int i=0;i < slen;i++)
      {
        char c = sv.charAt(i);
        int j = m.indexOf(c);
        if (j >= 0)
          {
            if (j>=tlen)
              continue;
            c = t.charAt(j);
	  }
        s.append(c);
      }

    return s.toString();
  }

  public static Object stringPad (Object str, Object padcount)
  {
    int count = ((Number) NumberValue.numberValue(padcount)).intValue();
    if (count <= 0)
      {
        if (count == 0)
          return "";
	throw new IndexOutOfBoundsException("Invalid string-pad count");
      }

    String sv = StringValue.stringValue(str);
    int slen = sv.length();
    StringBuffer s = new StringBuffer(count*slen);
    for (int i=0; i<count; i++) s.append(sv);

    return s.toString();
  }

  public static Object contains (Object str, Object contain)
  {
    String s = StringValue.stringValue(str);
    String c = StringValue.stringValue(contain);

    return s.indexOf(c) <0 ? Boolean.FALSE : Boolean.TRUE;
  }

  public static Object startsWith (Object str, Object with)
  {
    String s = StringValue.stringValue(str);
    String w = StringValue.stringValue(with);

    return s.startsWith(w) ? Boolean.TRUE : Boolean.FALSE;
  }

  public static Object endsWith (Object str, Object with)
  {
    String s = StringValue.stringValue(str);
    String w = StringValue.stringValue(with);
    return s.endsWith(w) ? Boolean.TRUE : Boolean.FALSE;
  }

  public static Object stringJoin (Object strseq, Object join)
  {
    StringBuffer s = new StringBuffer();
    String glue = StringValue.stringValue(join);
    int glen = glue.length();
    int index=0;
    boolean started = false;

    while((index=Values.nextIndex(strseq, index)) >= 0)
      {
	Object obj = Values.nextValue(strseq, index-1);
	if (obj == Values.empty) continue;

	if (started && glen > 0)
          s.append(glue);
        s.append(StringValue.stringValue(obj));
	started=true;
      }

    return s.toString();
  }

  public static String concat$V (Object[] args)
  {
    int count = args.length;
    java.lang.StringBuffer result = new java.lang.StringBuffer();

    for (int i = 0; i < count; i++)
      result.append(StringValue.stringValue(args[i]));
    return result.toString();
  }

  /** This implements the XQuery <code>fn:compare</code> function. */
  public static Object compare (Object val1, Object val2, NamedCollator coll)
  {
    if (val1 == Values.empty || val1 == null
        || val2 == Values.empty || val2 == null)
      return Values.empty;
    if (coll == null)
      coll = NamedCollator.codepointCollation;
    int ret = coll.compare(val1.toString(), val2.toString());
    return ret < 0 ? IntNum.minusOne() : ret > 0 ? IntNum.one() : IntNum.zero();
  }

  public static void stringToCodepoints$X (Object arg, CallContext ctx)
  {
    if (arg == Values.empty || arg == null)
      return;
    String str = StringValue.stringValue(arg);
    int len = str.length();
    Consumer out = ctx.consumer;
    for (int i = 0;  i < len;  i++)
      {
        char ch = str.charAt(i);
        // Should handle surrogates - but then we should do so generally. FIXME.
        out.writeInt((int) ch);
      }
  }

  private static void appendCodepoint (Object code, StringBuffer sbuf)
  {
    IntNum I = (IntNum)  gnu.kawa.xml.XIntegerType.integerType.cast(code);
    int i = I.intValue();
    if (i <= 0
        || (i > 0xD7FF
            && (i < 0xE000 || (i > 0xFFFD || i < 0x10000) || i > 0x10FFFF)))
      throw new IllegalArgumentException("codepoints-to-string: "+i+" is not a valid XML character [FOCH0001]");
    // FIXME - handle surrugates
    sbuf.append((char) i);
  }

  public static String codepointsToString (Object arg)
  {
    if (arg == null)
      return "";
    StringBuffer sbuf = new StringBuffer();
    if (arg instanceof Values)
      {
        Values vals = (Values) arg;
        int ipos = vals.startPos();
        while ((ipos = vals.nextPos(ipos)) != 0)
          appendCodepoint(vals.getPosPrevious(ipos), sbuf);
      }
    else
      appendCodepoint(arg, sbuf);
    return sbuf.toString();
  }

  public static String encodeForUri (Object arg)
  {
    return encodeForUri(arg, 'U');
  }

  public static String iriToUri (Object arg)
  {
    return encodeForUri(arg, 'I');
  }

  public static String escapeHtmlUri (Object arg)
  {
    return encodeForUri(arg, 'H');
  }

  static String encodeForUri (Object arg, char mode)
  {
    StringBuffer sbuf = new StringBuffer();
    String str;
    if (arg instanceof String || arg instanceof UntypedAtomic)
      str = arg.toString();
    else if (arg == null || arg == Values.empty)
      str = "";
    else
      throw new ClassCastException();
    int len = str.length();
    for (int i = 0; i <len;  i++)
      {
        int ch = str.charAt(i);
        // FIXME: Check for surrogate.
        if (mode == 'H' ? ch >= 32 && ch <= 126
            : ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
               || (ch >= '0' && ch <= '9')
               || ch == '-' || ch == '_' || ch == '.' || ch == '~'
               || (mode == 'I'
                   && (ch == ';' || ch == '/' || ch == '?' || ch == ':'
                       || ch == '*' || ch == '\'' || ch == '(' || ch == ')'
                       || ch == '@' || ch == '&' || ch == '=' || ch == '+'
                       || ch == '$' || ch == ',' || ch == '[' || ch == ']'
                       || ch == '#' || ch == '!' || ch == '%'))))
          sbuf.append((char) ch);
        else
          {
            int pos = sbuf.length();
            int nbytes = 0;
            int needed = ch < (1 << 7) ? 1
              : ch < (1 << 11) ? 2
              : ch < (1 << 16) ? 3
              : 4;
            do
              {
                // We insert encodings for the bytes in right-to-left order.
                int availbits = nbytes == 0 ? 7 : 6 - nbytes;
                int b;
                if (ch < (1 << availbits))
                  {
                    // The rest fits: handling first bytes.
                    b = ch;
                    if (nbytes > 0)
                      b |= (0xff80 >> nbytes) & 0xff;
                    ch = 0;
                  }
                else
                  {
                    b = 0x80 | (ch & 0x3f);
                    ch >>= 6;
                  }
                nbytes++;
                for (int j = 0; j <= 1; j++)
                  {
                    int hex = b & 15;
                    sbuf.insert(pos,
                                (char) (hex <= 9 ? hex + '0' : hex - 10 + 'A'));
                    b >>= 4;
                  }
                sbuf.insert(pos, '%');
              }
            while (ch != 0);
          }
      }
    return sbuf.toString();
  }

  public static String normalizeSpace (Object arg)
  {
    if (arg == Values.empty || arg == null)
      return "";
    String str = StringValue.stringValue(arg);
    int len = str.length();
    StringBuffer sbuf = null;
    int skipped = 0;
    for (int i = 0;  i < len;  i++)
      {
        char ch = str.charAt(i);
        if (Character.isWhitespace(ch))
          {
            if (sbuf == null && skipped == 0 && i > 0)
              sbuf = new StringBuffer(str.substring(0, i));
            skipped++;
          }
        else
          {
            if (skipped > 0)
              {
                if (sbuf != null)
                  sbuf.append(' ');
                else if (skipped > 1 || i == 1 || str.charAt(i-1) != ' ')
                  sbuf = new StringBuffer();
                skipped = 0;
              }
            if (sbuf != null)
              sbuf.append(ch);
          }
      }
    return sbuf != null ? sbuf.toString() : skipped > 0 ? "" : str;
  }

  /* #ifdef use:java.util.regex */
  public static Pattern makePattern (String pattern, String flags)
  {
    int fl = 0;
    for (int i = flags.length();  --i >= 0; )
      {
        char ch = flags.charAt(i);
        switch (ch)
          {
          case 'i':
            fl |= Pattern.CASE_INSENSITIVE|Pattern.UNICODE_CASE;
            break;
          case 's':
            fl |= Pattern.DOTALL;
            break;
          case 'x':
            StringBuffer sbuf = new StringBuffer();
            int plen = pattern.length();
            for (int j = 0; j < plen;  j++)
              {
                char pch = pattern.charAt(j);
                if (! Character.isWhitespace(pch))
                  sbuf.append(pch);
              }
            pattern = sbuf.toString();
            break;
          case 'm':
            fl |= Pattern.MULTILINE;
            break;
          default:
            throw new IllegalArgumentException("unknown 'replace' flag");
          }
      }
    return Pattern.compile(pattern, fl);
  }
  /* #endif */

  public static boolean matches (Object input, String pattern)
  {
    return matches(input, pattern, "");
  }

  public static boolean matches (Object arg, String pattern, String flags)
  {
    /* #ifdef use:java.util.regex */
    String str;
    if (arg instanceof String || arg instanceof UntypedAtomic)
      str = arg.toString();
    else if (arg == null || arg == Values.empty)
      str = "";
    else
      throw new ClassCastException();
    return makePattern(pattern, flags).matcher(str).find();
    /* #else */
    // throw new Error("fn:matches requires java.util.regex (JDK 1.4 or equivalent)");
    /* #endif */
  }

  public static String replace (Object input, String pattern,
                                 String replacement)
  {
    return replace(input, pattern, replacement, "");
  }

  public static String replace (Object arg, String pattern,
                                 String replacement, String flags)
  {
    /* #ifdef use:java.util.regex */
    String str;
    if (arg instanceof String || arg instanceof UntypedAtomic)
      str = arg.toString();
    else if (arg == null || arg == Values.empty)
      str = "";
    else
      throw new ClassCastException();
    return makePattern(pattern, flags).matcher(str).replaceAll(replacement);
    /* #else */
    // throw new Error("fn:replace requires java.util.regex (JDK 1.4 or equivalent)");
    /* #endif */
  }

  public static Object codepointEqual (Object arg1, Object arg2)
  {
    if (arg1 == null || arg1 == Values.empty)
      return arg1;
    if (arg2 == null || arg2 == Values.empty)
      return arg2;
    String str1 = StringValue.stringValue(arg1);
    String str2 = StringValue.stringValue(arg2);
    return str1.equals(str2) ? Boolean.TRUE : Boolean.FALSE;
  }
}
