// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;
import java.io.*;
import java.awt.Color;
import gnu.mapping.InPort;
import gnu.lists.*;
import gnu.text.Char;
import gnu.commonlisp.lang.Symbol; // FIXME
import javax.swing.text.*; // FIXME

public abstract class Buffer extends AbstractSequence implements CharSeq
{
  String name;
  String filename;
  String encoding;
  //boolean modified;

  static Buffer current;

  public Marker pointMarker;
  public Marker markMarker;

  /** List of modes active for this buffer, mahor mode first. */
  Mode modes;

  /** Map buffer names to buffers. */
  public static java.util.Hashtable buffers
  = new java.util.Hashtable(100);

  /** Map file names to buffer.s */
  public static java.util.Hashtable fileBuffers
  = new java.util.Hashtable(100);

  EKeymap localKeymap;
  public EKeymap[] activeKeymaps;
  int activeLength;
  // private EKeymap actual;
  /* Count of initial Keymaps in activeKeymaps that have been eliminated,
   * because of previous prefix keys. */
  int eliminated = 0;

  public String getName() { return name; }

  public String getFileName() { return filename; }

  public void setFileName(String fname)
  {
    if (filename != null && fileBuffers.get(filename) == this)
      fileBuffers.remove(filename);
    if (name != null && buffers.get(name) == this)
      buffers.remove(name);
    filename = fname;
    name = generateNewBufferName(new java.io.File(fname).getName());
    buffers.put(name, this);
    fileBuffers.put(filename, this);
    redrawModeline();
  }

  /*
  public CharSeq getStringContent ()
  {
    return content;
  }
  */

  public static Buffer findFile(String fname)
  {
    Buffer buffer = (Buffer) fileBuffers.get(fname);
    if (buffer == null)
      {
        buffer = EToolkit.getInstance().newBuffer(null);
        buffer.setFileName(fname);
	buffer.encoding = System.getProperty("file.encoding", "UTF8");
        try
          {
	    Reader in = new InputStreamReader(new FileInputStream(fname),
					      buffer.encoding);
            buffer.insertFile(in);
            in.close();
          }
        catch (java.io.FileNotFoundException ex)
          {
            Signal.message("New file");
          }
        catch (Exception ex)
          {
            throw new RuntimeException("error reading file \"" + fname
                                       + "\": " + ex);
          }
      }
    return buffer;
  }

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


  public abstract void redrawModeline ();

  public Buffer (String name)
  {
    this.name = name;

    activeKeymaps = new EKeymap[6];
    activeLength = 1;
    activeKeymaps[0] = EKeymap.globalKeymap;
  }

  public int checkMark()
  {
    return markMarker.getOffset();
  }

  public static Buffer getCurrent()
  {
    return current;
  }

  public static void setCurrent(Buffer buffer)
  {
    current = buffer;
  }

  public int getDot()
  {
    return pointMarker.getOffset();
  }

  public int getPoint()
  {
    return 1 + getDot();
  }

  public void setDot(int i)
  {
    if (i > maxDot())
      throw new Error("set dot to "+i+ " max:"+maxDot());
    pointMarker.set(this, i);
  }

  public final void setPoint(int i)
  {
    setDot(i - 1);
  }

  public int minDot()
  {
    return 0;
  }

  public abstract int getLength();

  public final int length() { return getLength(); }

  public abstract int maxDot();

  public void forwardChar(int i)
  {
    pointMarker.forwardChar(i);
  }

  public void backwardChar(int i)
  {
    pointMarker.backwardChar(i);
  }

  public String toString()
  {
    return "#<buffer \"" + name + "\">";
  }

  /** Insert count copies of ch at position of (ipos, xpos). */
  /*
  public void insert (char ch, int count, Object style, int ipos, Object xpos)
  {
  }
  */

  /** Insert string with given style at position pair. */
  public abstract void insert (String string, Object style,
			       int ipos, Object xpos);

  /** Insert character with given style at position pair. */
  public void insert (char[] chars, int offset, int count, Object style,
		      int ipos, Object xpos)
  {
    insert(new String(chars, offset, count), style, ipos, xpos);
  }

  public void insertAll (Object[] values, Object style)
  {
    int len = values.length;
    for (int i = 0;  i < len;  i++)
      {
	Object value = values[i];
	if (value instanceof Char)
	  insert(((Char) value).charValue(), 1, style);
	else
	  pointMarker.insert(value.toString(), style);
      }
  }

  public void insert (String string, Object style)
  {
    pointMarker.insert(string, style);
  }

  public void insert (Object value, Object style)
  {
    if (value instanceof Char)
      insert(((Char) value).charValue(), 1, style);
    else
      pointMarker.insert(value.toString(), style);
  }

  /** Insert count copies of ch at point. */
  public void insert (char ch, int count)
  {
    pointMarker.insert(ch, count, null);
  }

  /** Insert count copies of ch at point. */
  public void insert (char ch, int count, Object style)
  {
    pointMarker.insert(ch, count, style);
  }

  public void removeChar (int count)
  {
    pointMarker.removeChar(count);
  }

  public abstract void removeAll ();

  public Marker getPointMarker (boolean share)
  {
    return share ? pointMarker : new Marker(pointMarker);
  }

  public Marker getMarkMarker (boolean force)
  {
    return markMarker;
  }

  /** Convert an Emacs position (Marker, or 1-origin integer)
   * to a (0-origin) buffer offset. */
  public int positionToOffset (Object position)
  {
    if (position instanceof Number)
      {
	int min = minDot();
	int max = maxDot();
	int goal = ((Number) position).intValue() - 1;
	return goal < min ? min : goal > max ? max : goal;
      }
    return ((Marker) position).getOffset();
  }

  public void writeTo(java.io.Writer str) throws java.io.IOException
  {
    writeTo(0, length(), str);
  }

  public abstract void insertFile(Reader in) throws Exception;

  public abstract void save(Writer out) throws Exception;

  public void save()
  {
    try
      {
	if (encoding == null)
	  encoding = System.getProperty("file.encoding", "UTF8");
	Writer out = new OutputStreamWriter(new FileOutputStream(filename),
					    encoding);
        save(out);
        out.close();
      }
    catch (Exception ex)
      {
        throw new RuntimeException("error save-buffer: "+ex);
      }
  }

  public void insertFile(String filename)
  {
    try
      {
	if (encoding == null)
	  encoding = System.getProperty("file.encoding", "UTF8");
        Reader in = new InputStreamReader(new FileInputStream(filename),
					  encoding);
        insertFile(in);
        in.close();
      }
    catch (Exception ex)
      {
        throw new RuntimeException("error reading file \""+filename+"\": "+ex);
      }
  }

  int tabWidth = 8;

  public int charWidth (char ch, int column)
  {
    if (ch < 0x3000)
      {
	// Combining forma should probably be 0.
	if (ch < ' ')
	  {
	    if (ch == '\t')
	      return (((column + tabWidth) / tabWidth) * tabWidth) - column;
	    return 0;
	  }
      }
    else
      {
	if (ch < 0xD800 // CJK Ideographs
	    || (ch >= 0xFF01 && ch <= 0xFF5E)  // Fullwidth ASCII.
	    || (ch >= 0xFFe0 && ch <= 0xFFE6)) // Fullwidth punctuation.
	  return 2;
	if (ch < 0xE000)
	  return 0;  // Surrogates.
      }
    return 1;
  }

  public int countColumns(char[] chars, int start, int count, int initial)
  {
    while (--count >= 0)
      initial += charWidth (chars[start++], initial);
    return initial;
  }

  public int currentColumn()
  {
    return currentColumn(getDot());
  }

  /** Return the column number at a specified offset. */
  public int currentColumn(int offset)
  {
    int lineStart = lineStartOffset(offset);
    BufferReader port = new BufferReader(this, lineStart, offset - lineStart);
    int column = 0;
    while (port.read() >= 0)
      {
	// Subtract one from pos, to undo the read we just did.
	int start = port.pos - 1;
	column = countColumns(port.buffer, start, port.limit - start, column);
	port.pos = port.limit;
      }
    return column;
  }

  public int moveToColumn(int column, boolean force)
  { 
    return pointMarker.moveToColumn(column, force);
  }

  public abstract int lineStartOffset(int offset);

  public int lineStartOffset()
  {
    return lineStartOffset(getDot());
  }

  /** Search in BUF for COUNT instances of the character TARGET between START and END.
   * If COUNT is positive, search forwards; END must be >= START.
   * If COUNT is negative, search backwards for the -COUNTth instance;
   *   END must be <= START.
   * If COUNT is zero, do anything you please; run rogue, for all I care.
   * If END is zero, use beginning or end of (FIXME: accessible part of)
   * the buffer, as appropriate for the direction indicated by COUNT.
   *
   * If we find COUNT instances, SHORTAGE is zero, and return the
   * position after the COUNTth match.  Note that for reverse motion
   * this is not the same as the usual convention for Emacs motion commands.

   * If we don't find COUNT instances before reaching END, set SHORTAGE
   * to the number of TARGETs left unfound, and return (shortage<<32|END).
   * @return (SHORTAGE<<32|POS)
  */
  public abstract long scan(char target, int start, int end,
			    int count, boolean allowQuit);
  
  /** Find the position a give number of lines forward or backward.
   * A side-effect-free version of Emacs's forward-line function.
   * @param lines number of lines forward (or backward if negative)
   * @param start initial position (buffer offset)
   * @return (SHORTAGE<<32|POS)
   */
  public final long forwardLine(int lines, int start)
  {
    boolean neg = lines <= 0;
    long scanned = scan('\n', start, 0, lines - (neg ? 1 : 0), true);
    int shortage = (int) (scanned >> 32);
    int pos = (int) scanned;
    if (shortage > 0
	&& (neg
	    || (maxDot() > minDot() && pos != start
		&& charAt(pos - 1) != '\n')))
      shortage--;
    return ((long) (neg ? -shortage : shortage) << 32) | (long) pos;
  }

  public int forwardLine(int lines)
  {
    long value = forwardLine(lines, getDot());
    setDot((int) value);
    return (int) (value >> 32);
  }

  public EWindow display(boolean notThisWindow, EFrame frame)
  {
    if (frame == null)
      frame = EFrame.getSelectedFrame();
    EWindow selected = frame.getSelectedWindow();
    EWindow window = frame.otherWindow(1);
    if (selected == window && notThisWindow)
      window = selected.split(-1, false);
    window.setBuffer(this);
    return window;
  }

  /*
  public Element createLeafElement(Element parent, AttributeSet attributes,
                                   int p0, int p1)
  {
    p0 = content.createPosition(p0, p0!=0);
    p1 = content.createPosition(p1, true);
    return new Leaf(this, parent, attributes, p0, p1);
  }
  */

  /**
   * @param all true if make-variable-buffer-local,
   *  false if make-local-variable FIXME
   */
  public static void makeBufferLocal(Object symbol, boolean all)
  {
    BufferLocalConstraint.make(Symbol.getBinding(symbol), all);
  }

  public abstract char charAt(int index);

  public EKeymap getLocalKeymap() { return localKeymap; }

  public void setLocalKeymap(EKeymap map)
  {
    // First remove the old local map.
    if (localKeymap != null)
      {
        activeKeymaps[activeLength-2] = activeKeymaps[activeLength-1];
        activeLength--;
        localKeymap = null;
      }
    if (map != null)
      {
        activeKeymaps[activeLength] = activeKeymaps[activeLength-1];
        activeKeymaps[activeLength-1]= map;
        activeLength++;
        localKeymap = map;
      }
  }

  public abstract long savePointMark ();

  public abstract void restorePointMark (long pointMark);
}
