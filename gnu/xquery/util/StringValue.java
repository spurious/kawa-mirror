// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
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
    if (node instanceof TreeList)
      {
	((TreeList) node).stringValue(0, sbuf);
	return;
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  {
	    TreeList tlist = (TreeList) pos.sequence;
	    tlist.stringValue(tlist.posToDataIndex(pos.ipos), sbuf);
	    return;
	  }
      }
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
    if (node == Values.empty || node == null)
      return Values.empty;
    return stringValue(node).toLowerCase();
  }

  public static Object upperCase (Object node)
  {
    if (node == Values.empty)
      return Values.empty;
    return stringValue(node).toUpperCase();
  }

  public static Object substring (Object str, Object start)
  {
    if (str == Values.empty || start == Values.empty)
      return Values.empty;

    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;

    return str.toString().substring(i);
  }

  public static Object substring (Object str, Object start, Object length)
  {
    if (str == Values.empty || start == Values.empty
	|| length == Values.empty)
      return Values.empty;

    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;
    int len = ((Number) NumberValue.numberValue(length)).intValue();

    return str.toString().substring(i, i + len);
  }

  public static Object stringLength (Object str)
  {
    if (str == Values.empty)
      return Values.empty;

    return IntNum.make(str.toString().length());
  }

  public static Object substringBefore (Object str, Object find)
  {
    if (str == Values.empty || find == Values.empty)
      return Values.empty;

    if (find.toString().length()==0)
      return str.toString();

    int start = str.toString().indexOf(find.toString());
    return (start>0)?str.toString().substring(0,start):"";
  }

  public static Object substringAfter (Object str, Object find)
  {
    if (str == Values.empty || find == Values.empty)
      return Values.empty;

    if (find.toString().length()==0)
      return str.toString();

    int start = str.toString().indexOf(find.toString());
    int end = find.toString().length()+start;
    return (start!=-1 && end<str.toString().length())?str.toString().substring(end):"";
  }

  public static Object translate (Object str, Object map, Object trans)
  {
    if (str == Values.empty || map == Values.empty || trans == Values.empty)
      return Values.empty;

    String m = map.toString();
    int mlen = m.length();

    if (mlen==0) return str.toString();

    String t = trans.toString();
    StringBuffer s = new StringBuffer(str.toString());
    int slen = s.length();
    int tlen = t.length();

    for (int i=0;i < slen;i++)
      {
	for (int j=0;j<mlen;j++)
	  {
	    if (s.charAt(i)==m.charAt(j))
	      {
		if (j<tlen) { s.setCharAt(i,t.charAt(j)); }
		else { s.deleteCharAt(i--); slen--; }
		continue;
	      }
	  }
      }

    return s.toString();
  }

  public static Object stringPad (Object str, Object padcount)
  {
    if (str == Values.empty)
      return Values.empty;

    int count =  ((Number) NumberValue.numberValue(padcount)).intValue();
    if (count==0) return "";
    if (count<0)
      {
	OutPort err = OutPort.errDefault();
	err.println("Invalid string-pad count");
	return "";
      }

    StringBuffer s = new StringBuffer(str.toString());
    for (int i=1; i<count; i++) s.append(str.toString());

    return s.toString();
  }

  public static Object contains (Object str, Object contain)
  {
    if (str == Values.empty || contain == Values.empty)
      return Values.empty;

    String s = str.toString();
    String c = contain.toString();

    if (c.length()==0) return Boolean.TRUE;
    if (s.length()==0 && c.length()>0) return Boolean.FALSE;

    return s.indexOf(c)<0? Boolean.FALSE : Boolean.TRUE;
  }

  public static Object startsWith (Object str, Object with)
  {
    if (str == Values.empty || with == Values.empty)
      return Values.empty;

    String s = str.toString();
    String w = with.toString();

    if (w.length()==0) return Boolean.TRUE;
    if (s.length()==0 && w.length()>0) return Boolean.FALSE;

    return s.startsWith(w)? Boolean.TRUE : Boolean.FALSE;
  }

  public static Object endsWith (Object str, Object with)
  {
    if (str == Values.empty || with == Values.empty)
      return Values.empty;

    String s = str.toString();
    String w = with.toString();

    if (w.length()==0) return Boolean.TRUE;
    if (s.length()==0 && w.length()>0) return Boolean.FALSE;

    return s.endsWith(w)? Boolean.TRUE : Boolean.FALSE;
  }

  public static Object stringJoin (Object strseq, Object join)
  {
    if (strseq == Values.empty)
      return Values.empty;

    StringBuffer s = new StringBuffer();
    String glue = join.toString();
    int glen = glue.length();
    int index=0;
    boolean started = false;

    while((index=Values.nextIndex(((Values)strseq),index)) >= 0)
      {
	Object obj = Values.nextValue( ((Values)strseq) ,index-1);
	if (obj == Values.empty) continue;

	if (started && glen > 0) s.append(glue);
	if (obj.toString().length() > 0) s.append(obj.toString());
	started=true;
      }

    return s.toString();
  }

}
