//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import gnu.lists.Consumer;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import gnu.lists.CharSeq;

import org.eclipse.swt.custom.StyledTextContent;
import org.eclipse.swt.custom.TextChangeListener;
import org.eclipse.swt.custom.TextChangedEvent;
import org.eclipse.swt.custom.TextChangingEvent;

/**
 * @author Christian Surlykke
 *         12-07-2004
 */
public class BufferContent extends SwtCharBuffer
  implements StyledTextContent, CharSeq
{
  public BufferContent()
  {
    this(1000);
  }

  /**
   * @param initialSize
   */
  public BufferContent(int initialSize)
  {
    super(initialSize);
  }


  private Set textChangeListeners = new HashSet();
  
  /**
   * @see org.eclipse.swt.custom.StyledTextContent#addTextChangeListener(org.eclipse.swt.custom.TextChangeListener)
   */
  public void addTextChangeListener(TextChangeListener textChangeListener)
  {
    if (textChangeListener != null) 
    {
      textChangeListeners.add(textChangeListener);
    }

  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#removeTextChangeListener(org.eclipse.swt.custom.TextChangeListener)
   */
  public void removeTextChangeListener(TextChangeListener textChangeListener)
  {
    textChangeListeners.remove(textChangeListener);
  }

  
  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getCharCount()
   */
  public int getCharCount()
  {
    return size();
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getLine(int)
   */
  public String getLine(int lineIndex)
  {
    int startPos = offset2pos(lineOffsets.index2offset(lineIndex));
    int endPos;
    if (lineIndex + 1 == lineOffsets.size()) 
    {
      endPos = size();
    }
    else 
    {
      endPos = offset2pos(lineOffsets.index2offset(lineIndex + 1));
      // Strip linedelimiters
      while (endPos > startPos && lineOffsets.isLineDelimiter(charAt(endPos - 1)))
      {
        endPos--;
      }
    }
    
    char[] tmp = new char[endPos - startPos];
    getChars(startPos, endPos, tmp, 0);
    return new String(tmp);
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getLineAtOffset(int)
   */
  public int getLineAtOffset(int pos)
  {
    return (lineOffsets.offset2index(pos2offset(pos)));
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getLineCount()
   */
  public int getLineCount()
  {
    return lineOffsets.size();
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getLineDelimiter()
   */
  public String getLineDelimiter()
  {
    return "\n";
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getOffsetAtLine(int)
   */
  public int getOffsetAtLine(int lineIndex)
  {
    int result = offset2pos(lineOffsets.index2offset(lineIndex));
    return result;
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#getTextRange(int, int)
   */
  public String getTextRange(int start, int length)
  {
    char[] tmp = new char[length];
    getChars(start, start + length, tmp, 0);
    return new String(tmp);
  }
  
  /**
   * @see org.eclipse.swt.custom.StyledTextContent#replaceTextRange(int, int, java.lang.String)
   */
  public void replaceTextRange(int start, int length, String newText)
  {
    newText = newText == null ? "" : newText; 
    notifyListeners(makeTextChangingEvent(start, length, newText));
    delete(start, length);
    insert(start, newText);
    notifyListeners(new TextChangedEvent(this));
  }

  private void show(TextChangingEvent changingEvent)
  {
    System.out.println("start: " + changingEvent.start + "\n" +
    "newCharCount: " + changingEvent.newCharCount + "\n" +
    "newLineCount: " + changingEvent.newLineCount + "\n" +
    "replaceCharCount: " + changingEvent.replaceCharCount + "\n" +
    "replaceLineCount: " + changingEvent.replaceLineCount + "\n" +
    "text: " + printable(changingEvent.newText) + "\n");
  }
  /**
   * @param start
   * @param length
   * @param newText
   * @return
   */
  private TextChangingEvent makeTextChangingEvent(int start, int length, String newText)
  {
    TextChangingEvent result = new TextChangingEvent(this);
    result.start =  start;
    result.newCharCount = newText.length();
    result.newLineCount = lineOffsets.countLines(newText);
    result.newText = newText;
    result.replaceCharCount = length;
    result.replaceLineCount = lineOffsets.linesInRange(pos2offset(start), pos2offset(start + length));
    
    return result;
  }

  /**
   * @see org.eclipse.swt.custom.StyledTextContent#setText(java.lang.String)
   */
  public void setText(String newText)
  {
    delete(0, size());
    insert(0, newText);
    TextChangedEvent evt = new TextChangedEvent(this);
    for (Iterator iter = textChangeListeners.iterator(); iter.hasNext();)
    {
     ((TextChangeListener) iter.next()).textSet(evt);
    }
  }
  
  /**
   * @param changedEvent
   */
  private void notifyListeners(TextChangedEvent changedEvent)
  {
    for (Iterator iter = textChangeListeners.iterator(); iter.hasNext();)
    {
      TextChangeListener element = (TextChangeListener) iter.next();
      element.textChanged(changedEvent);
    }
    
  }

  /**
   * @param changingEvent
   */
  private void notifyListeners(TextChangingEvent changingEvent)
  {
    for (Iterator iter = textChangeListeners.iterator(); iter.hasNext();)
    {
      TextChangeListener element = (TextChangeListener) iter.next();
      element.textChanging(changingEvent);
    }
    
  }

  
  /**
   * For testing purposes
   * @param args
   */
  public static void main(String[] args) 
  {
  }

  public int lineStartPos(int pos)
  {
    int offset = pos2offset(pos);
    int lineStartOffset =  lineOffsets.index2offset(lineOffsets.offset2index(offset));
    return offset2pos(lineStartOffset);
  }

  /**
   * @param start
   * @param count
   * @param out
   */
  public void consume(int start, int count, Consumer out)
  {
    // TODO Auto-generated method stub
    
  }

  /**
   * @param in
   * @throws IOException
   */
  public void insertFile(Reader in, int pos) throws IOException
  {
    char[] buf = new char[65536];
    for (;;)
    {
      int charsRead = in.read(buf);
      if (charsRead < 0) 
      {
        break;
      }
      replaceTextRange(pos, 0, new String(buf, 0, charsRead));
    } 
  }

  public void setCharAt (int index, char value)
  {
    char[] arr = { value };
    replaceTextRange(index, index+1, new String(arr));
  }

  public void fill(char value)
  {
    fill(0, size(), value);
  }

  public void fill(int fromIndex, int toIndex, char value)
  {
    for (int i = fromIndex;  i < toIndex;  i++)
      setCharAt(i, value);
  }

  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end)
  {
    throw new UnsupportedOperationException("subSequence not implemented");
  }
  /* #endif */

  /* #ifdef JAVA5 */
  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException
  {
    dest.append(this, start, start+count);
  }

  public void writeTo(Appendable dest)
    throws java.io.IOException
  {
    dest.append(this, 0, size());
  }
  /* #else */
  // public void writeTo(int start, int count, java.io.Writer dest)
  //   throws java.io.IOException
  // {
  //   while (--count >= 0)
  //     dest.write(charAt(start++));
  // }

  // public void writeTo(java.io.Writer dest)
  //   throws java.io.IOException
  // {
  //   writeTo(0, size(), dest);
  // }
  /* #endif */

  /**
   * @param out
   * @throws IOException
   */
  public void save(Writer out) throws IOException
  {
    out.write(chars.data, 0, gapStart);
    out.write(chars.data, gapEnd, chars.data.length - gapEnd);
  }
}
