package gnu.jemacs.buffer;
import javax.swing.text.*;
import java.io.*;
import java.awt.Color;
import gnu.mapping.InPort;

public class Buffer
{
  String name;
  String filename;
  //boolean modified;

  static Buffer current;

  static javax.swing.text.StyleContext styles
  = new javax.swing.text.StyleContext();
  Style inputStyle = styles.addStyle("input", null);
  static Style redStyle = styles.addStyle("red", null);
  static Style blueStyle = styles.addStyle("blue", null);
  static
  {
    StyleConstants.setForeground(redStyle, Color.red);
    StyleConstants.setForeground(blueStyle, Color.blue);
  }

  Marker pointMarker;

  Caret curPosition = null;

  BufferContent content;
  DefaultStyledDocument document;
  StyledDocument modelineDocument;
  public final BufferKeymap keymap = new BufferKeymap(this);

  /** Map buffer names to buffer.s */
  public static java.util.Hashtable buffers
  = new java.util.Hashtable(100);

  /** Map file names to buffer.s */
  public static java.util.Hashtable fileBuffers
  = new java.util.Hashtable(100);

  public String getName() { return name; }

  public String getFileName() { return filename; }

  public BufferContent getContent() { return content; }

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

  public static Buffer findFile(String fname)
  {
    Buffer buffer = (Buffer) fileBuffers.get(fname);
    if (buffer == null)
      {
        buffer = new Buffer(null);
        buffer.setFileName(fname);
        try
          {
            Reader in = new FileReader(fname);
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

  public void redrawModeline()
  {
    try
      {
        modelineDocument.remove(0, modelineDocument.getLength());
        
        modelineDocument.insertString(0, "-----", redStyle);
        modelineDocument.insertString(modelineDocument.getLength(),
                                      "JEmacs: " + getName(),
                                      blueStyle);
        modelineDocument.insertString(modelineDocument.getLength(),
                                      " ---",
                                      redStyle);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("internal error in redraw-modeline- "+ex);
      }
  }

  public Buffer(String name)
  {
    this.name = name;
    content = new BufferContent();

    pointMarker = new Marker();
    pointMarker.buffer = this;
    pointMarker.index
      = content.allocatePosition(0, BufferContent.AFTER_MARK_KIND);

    document = new javax.swing.text.DefaultStyledDocument(content, styles);

    modelineDocument
      = new javax.swing.text.DefaultStyledDocument(new javax.swing.text.StringContent(), styles);
    redrawModeline();
  }

  public static Buffer getCurrent()
  {
    return current;
  }

  public static void setCurrent(Buffer buffer)
  {
    current = buffer;
  }

  public final int getDot()
  {
    return pointMarker.getOffset();
  }

  public int getPoint()
  {
    return 1 + getDot();
  }

  public final void setDot(int i)
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

  public int maxDot()
  {
    // Subtract 1 for the content's final "\n".
    return content.length() - 1;
  }

  public void forwardChar(int i)
  {
    int point = getDot();
    int max = maxDot();
    if (point + i > max)
      {
	point = max;
	Signal.signal("End of buffer");
      }
    point += i;
    setDot(point);
  }

  public void backwardChar(int i)
  {
    int point = getDot();
    if (point < i)
      {
	point = 0;
	Signal.signal("Beginning of buffer");
      }
    point -= i;
    setDot(point);
  }

  public String toString()
  {
    return "#<buffer \"" + name + "\">";
  }

  public void insert (String string, Style style)
  {
    pointMarker.insert(string, style);
  }

  /** Insert count copies of ch at point. */
  public void insert (char ch, int count, Style style)
  {
    pointMarker.insert(ch, count, style);
  }

  public void deleteChar (int count)
  {
    pointMarker.deleteChar(count);
  }

  public void remove (int start, int count)
    throws javax.swing.text.BadLocationException
  {
    document.remove(start, count);
  }

  public void removeRegion (int start, int end)
    throws javax.swing.text.BadLocationException
  {
    document.remove(start, end - start);
  }

  public void removeAll ()
  {
    try
      {
	document.remove(0, maxDot());
      }
    catch (javax.swing.text.BadLocationException ex)
      {
	throw new gnu.mapping.WrappedException(ex);
      }
  }

  public Marker getPointMarker (boolean share)
  {
    return share ? pointMarker : new Marker(pointMarker);
  }

  /** Convert an Emacs position (Marker, Position, or 1-origin integer)
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
    return ((Position) position).getOffset();
  }

  public void save(Writer out)
    throws java.io.IOException, javax.swing.text.BadLocationException
  {
    int length = document.getLength();
    int todo = length;
    Segment segment = new Segment();
    int offset = 0;
    while (offset < length)
      {
        int count = length;
        if (count > 4096)
          count = 4096;
        document.getText(offset, count, segment);
        out.write(segment.array, segment.offset, segment.count);
        offset += count;
      }
  }

  public void save()
  {
    try
      {
        Writer out = new FileWriter(filename);
        save(out);
        out.close();
      }
    catch (Exception ex)
      {
        throw new RuntimeException("error save-buffer: "+ex);
      }
  }

  public void insertFile(Reader in)
    throws java.io.IOException, javax.swing.text.BadLocationException
  {
    char[] buffer = new char[2048];
    int offset = getDot();
    for (;;)
      {
        int count = in.read(buffer, 0, buffer.length);
        if (count <= 0)
          break;
        document.insertString(offset, new String(buffer, 0, count), null);
        offset += count;
      }
  }

  public void insertFile(String filename)
  {
    try
      {
        Reader in = new FileReader(filename);
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

  // force is currently ignored FIXME
  public int moveToColumn(int column, boolean force)
  { 
    int lineStart = lineStartOffset(getDot());
    BufferReader port
      = new BufferReader(this, lineStart, maxDot() - lineStart);
    int resultColumn = 0;
    int offset = lineStart;
    for (;;)
      {
	int ch = port.read();
	if (ch < 0 || ch == '\n')
	  {
	    if (force)
	      {
		// FIXME
	      }
	    break;
	  }
	int width = charWidth((char) ch, resultColumn);
	offset++;
	resultColumn += width;
	if (resultColumn >= column)
	  {
	    if (resultColumn > column && force)
	      {
		// FIXME
	      }
	    break;
	  }
      }
    setDot(offset);
    return resultColumn;
  }

  public int lineStartOffset(int offset)
  {
    return (int) content.scan('\n', offset, minDot(), -1, true);
  }

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
  public final long scan(char target, int start, int end,
                   int count, boolean allowQuit)
  {
    if (end == 0)
      end = count > 0 ? content.length() - 1 : 0;
    return content.scan(target, start, end, count, allowQuit);
  }

  public Window display(boolean notThisWindow, Frame frame)
  {
    if (frame == null)
      frame = Frame.getSelectedFrame();
    Window selected = frame.getSelectedWindow();
    Window window = frame.otherWindow(1);
    if (selected == window && notThisWindow)
      window = selected.split(-1, false);
    window.setBuffer(this);
    return window;
  }
}
