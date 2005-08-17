// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.kawa.xml.KNode;
import gnu.math.*;

public class StringValue extends Procedure1
{
  public static final StringValue stringValue
  = new StringValue("string-value");
  public static final StringValue string = new StringValue("string");

  public StringValue(String name)
  {
    super(name);
  }

  public static String stringValue (Object node)
  {
    StringBuffer sbuf = new StringBuffer();
    stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static void stringValue (Object node, StringBuffer sbuf)
  {
    if (node instanceof KNode)
      {
	KNode pos = (KNode) node;
	NodeTree tlist = (NodeTree) pos.sequence;
	tlist.stringValue(tlist.posToDataIndex(pos.ipos), sbuf);
	return;
      }
    if (node != null && node != Values.empty)
      sbuf.append(node);
  }

  public Object apply1 (Object node)
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      stringValue(tlist.getPosNext(index), sbuf);
	    else
	      tlist.stringValue(tlist.posToDataIndex(index), sbuf);
	    index = tlist.nextPos(index);
	  }
      }
    else
      stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static Object lowerCase (Object node)
  {
    return stringValue(node).toLowerCase();
  }

  public static Object upperCase (Object node)
  {
    return stringValue(node).toUpperCase();
  }

  public static Object substring (Object str, Object start)
  {
    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;

    return stringValue(str).substring(i);
  }

  public static Object substring (Object str, Object start, Object length)
  {
    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;
    int len = ((Number) NumberValue.numberValue(length)).intValue();

    return stringValue(str).substring(i, i + len);
  }

  public static Object stringLength (Object str)
  {
    return IntNum.make(stringValue(str).length());
  }

  public static Object substringBefore (Object str, Object find)
  {
    String s = stringValue(str);
    String f = stringValue(find);
    int flen = f.length();

    if (flen==0)
      return "";
    int start = s.indexOf(f);
    return start >= 0 ? s.substring(0,start) : "";
  }

  public static Object substringAfter (Object str, Object find)
  {
    String s = stringValue(str);
    String f = stringValue(find);
    int flen = f.length();

    if (flen==0)
      return s;

    int start = s.indexOf(f);
    return start >= 0 ? s.substring(start+flen) : "";
  }

  public static Object translate (Object str, Object map, Object trans)
  {
    String m = stringValue(map);
    int mlen = m.length();
    String sv = stringValue(str);

    if (mlen==0) return sv;

    int slen = sv.length();
    StringBuffer s = new StringBuffer(slen);
    String t = stringValue(trans);
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

    String sv = stringValue(str);
    int slen = sv.length();
    StringBuffer s = new StringBuffer(count*slen);
    for (int i=0; i<count; i++) s.append(sv);

    return s.toString();
  }

  public static Object contains (Object str, Object contain)
  {
    String s = stringValue(str);
    String c = stringValue(contain);

    return s.indexOf(c) <0 ? Boolean.FALSE : Boolean.TRUE;
  }

  public static Object startsWith (Object str, Object with)
  {
    String s = stringValue(str);
    String w = stringValue(with);

    return s.startsWith(w) ? Boolean.TRUE : Boolean.FALSE;
  }

  public static Object endsWith (Object str, Object with)
  {
    String s = stringValue(str);
    String w = stringValue(with);
    return s.endsWith(w) ? Boolean.TRUE : Boolean.FALSE;
  }

  public static Object stringJoin (Object strseq, Object join)
  {
    StringBuffer s = new StringBuffer();
    String glue = stringValue(join);
    int glen = glue.length();
    int index=0;
    boolean started = false;

    while((index=Values.nextIndex(((Values)strseq),index)) >= 0)
      {
	Object obj = Values.nextValue( ((Values)strseq) ,index-1);
	if (obj == Values.empty) continue;

	if (started && glen > 0)
          s.append(glue);
        s.append(stringValue(obj));
	started=true;
      }

    return s.toString();
  }

  public static String concat$V (Object[] args)
  {
    int count = args.length;
    java.lang.StringBuffer result = new java.lang.StringBuffer();

    for (int i = 0; i < count; i++)
      result.append(stringValue(args[i]));
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
}
