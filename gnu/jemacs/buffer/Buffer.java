package gnu.jemacs.buffer;

public class Buffer
{
  String name;
  String filename;

  static javax.swing.text.StyleContext styles
  = new javax.swing.text.StyleContext();

  /** Value of point (0-orgin), when curPosition is null. */
  int point;
  javax.swing.text.Caret curPosition = null;

  javax.swing.text.GapContent content;
  javax.swing.text.DefaultStyledDocument document;

  public static java.util.Hashtable buffers
  = new java.util.Hashtable(100);

  public String getName() { return name; }

  public static Buffer getBuffer(String name)
  {
    return (Buffer) buffers.get(name);
  }

  public static Buffer coerceBuffer(Object buf)
  {
    if (buf instanceof Buffer)
      return (Buffer) buf;
    return getBuffer(buf.toString());
  }

  public static String generateNewBufferName(String start)
  {
    Buffer buf = getBuffer(start);
    if (buf == null)
      return start;
    int len = start.length();
    StringBuffer sbuf = new StringBuffer(len + 5);
    sbuf.append(start);
    sbuf.append('<');
    for (int i = 2;  ;  i++)
      {
	sbuf.append(i);
	sbuf.append('>');
	String name = sbuf.toString();
	buf = getBuffer(name);
	if (buf == null)
	  return name;
	sbuf.setLength(len+1);
      }
  }

  public Buffer(String name)
  {
    this.name = name;
    content = new javax.swing.text.GapContent();
    document = new javax.swing.text.DefaultStyledDocument(content, styles);
  }

  public final int getDot()
  {
    return curPosition == null ? point : curPosition.getDot();
  }

  public int getPoint()
  {
    return 1 + getDot();
  }

  public final void setDot(int i)
  {
    point = i;
    if (curPosition != null)
      curPosition.setDot(i);
  }

  public final void setPoint(int i)
  {
    setDot(i - 1);
  }

  public void forwardChar(int i)
  {
    if (curPosition != null)
      point = curPosition.getDot();
    point += i;
    if (curPosition != null)
      curPosition.setDot(point);
  }

  public void backwardChar(int i)
  {
    if (curPosition != null)
      point = curPosition.getDot();
    point -= i;
    if (point < 0)
      Signal.signal("Beginning of buffer");
    if (curPosition != null)
      curPosition.setDot(point);
  }

  public String toString()
  {
    return "#<buffer \"" + name + "\">";
  }

  /** Insert count copies of ch at point. */
  public void insert (char ch, int count)
  {
    if (count < 0)
      return;
    int todo = count > 500 ? 500 : count;
    StringBuffer sbuf = new StringBuffer(todo);
    for (int i = todo;  --i >= 0; )
      sbuf.append(ch);
    String str = sbuf.toString();
    if (curPosition != null)
      point = curPosition.getDot();
    for (;;)
      {
	try
	  {
	    document.insertString(point, str, null);
	  }
	catch (javax.swing.text.BadLocationException ex)
	  {
	    throw new Error("bad location: "+ex);
	  }
	point += todo;
	count -= todo;
	if (count == 0)
	  break;
	if (count < 500)
	  {
	    todo = count;
	    sbuf.setLength(todo);
	    str = sbuf.toString();
	  }
      }
    if (curPosition != null)
      curPosition.setDot(point);
  }

  /*
  public insertFileContents(String name)
  {
  }
  */
}
