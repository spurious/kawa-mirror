// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.xml.StringValue;

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

  public static Object substring (Object str, Object start)
  {
    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;

    return StringValue.stringValue(str).substring(i);
  }

  public static Object substring (Object str, Object start, Object length)
  {
    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;
    int len = ((Number) NumberValue.numberValue(length)).intValue();

    return StringValue.stringValue(str).substring(i, i + len);
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

    while((index=Values.nextIndex(((Values)strseq),index)) >= 0)
      {
	Object obj = Values.nextValue( ((Values)strseq) ,index-1);
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
    IntNum i =(IntNum)  gnu.kawa.xml.XIntegerType.integerType.cast(code);
    sbuf.append((char) i.intValue());
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
}
