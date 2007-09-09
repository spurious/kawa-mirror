package kawa;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import gnu.text.Path;
import gnu.mapping.*;
import gnu.kawa.swingviews.SwingContent;

/** Simple TextArea that always scrolls to the bottom.  Also creates an
 * out and err PrintWriter so that you can redirect stdout/stderr to
 * these streams, using the System.setOut/setErr methods.
 *
 * @author 	Albert Ting
 */
public class MessageArea extends JTextPane
  implements KeyListener, DocumentListener {
  private kawa.TextAreaWriter out_stream, err_stream;
  gnu.text.QueueReader in;
  StyledDocument document;

  public int outputMark = 0;
  public int endMark = -1;
  int length = 0;

  public static javax.swing.text.StyleContext styles
  = new javax.swing.text.StyleContext();
  static public Style defaultStyle = styles.addStyle("default",null);
  public static Style inputStyle = styles.addStyle("input", null);
  public static Style redStyle = styles.addStyle("red", null);
  static Style blueStyle = styles.addStyle("blue", null);
  static Style promptStyle = styles.addStyle("prompt", null);
  static {
    StyleConstants.setForeground(redStyle, Color.red);
    StyleConstants.setForeground(blueStyle, Color.blue);
    StyleConstants.setForeground(promptStyle, Color.green);
    StyleConstants.setBold(inputStyle, true);
  }

  private MessageArea(gnu.text.QueueReader in, SwingContent content)
  {
    super(new DefaultStyledDocument(content, styles));
    document = (StyledDocument) getDocument();

    this.in = in;

    out_stream = new kawa.TextAreaWriter(this, "/dev/stdout", defaultStyle);
    err_stream = new kawa.TextAreaWriter(this, "/dev/stderr", redStyle);

    addKeyListener(this);
    document.addDocumentListener(this);
  }

  /**
   * simple TextArea that always scrolls to the bottom.  Also creates an
   * out and err PrintWriter so that you can redirect stdout/stderr to
   * these streams, using the System.setOut/setErr methods.
   */
  public MessageArea(gnu.text.QueueReader in) {
    this(in, new gnu.kawa.swingviews.SwingContent());
  }

  void enter ()
  {
	int pos = getCaretPosition();
        // FIXME getText is wasteful.  We can do something smarter
        // since we have access to SwingContent internals.
	String str = getText();
	int len = str.length();
	if (len != length) {
	  System.err.println("(actual) len:"+len + " (saved) length:"+length);
	}
	endMark = -1;
	if (pos >= outputMark) {
	  int lineAfter = str.indexOf('\n', outputMark);
	  if (lineAfter < 0) {
            insertString(len, "\n", null);
            str = str.substring(outputMark, len)+'\n';
            lineAfter = len;
	  }
	  else {
	    endMark = len;
            str = str.substring(outputMark, lineAfter+1);
          }
	  outputMark = lineAfter+1;
          setCaretPosition(outputMark);
	}
	else {
          int lineBefore = pos == 0 ? 0 : 1 + str.lastIndexOf('\n', pos-1);
          Element el = document.getCharacterElement(lineBefore);
          int lineAfter = str.indexOf('\n', pos);
          // Strip initial prompt:
          if (el.getAttributes().isEqual(promptStyle))
            lineBefore = el.getEndOffset();
          if (lineAfter < 0)
            str = str.substring(lineBefore, len)+'\n';
          else
            str = str.substring(lineBefore, lineAfter+1);
          setCaretPosition(outputMark);
          write(str, inputStyle);
	}

	if (in != null) {
	  in.append(str);
	}
  }

  public void keyPressed(KeyEvent e) {
    int code = e.getKeyCode();
    if (code == KeyEvent.VK_ENTER)
      {
	enter();
	e.consume();
      }
  }
  public void keyReleased(KeyEvent e) {
  }

  public MutableAttributeSet getInputAttributes() {
    return inputStyle;
  }

  protected void insertString(int pos, String str, AttributeSet style)
  {
    try
      {
        document.insertString(pos, str, style);
      }
    catch (BadLocationException ex)
      {
        /* #ifdef use:java.lang.Throwable.getCause */
        throw new Error(ex);
        /* #else */
        // throw new WrappedException(ex);
        /* #endif */
      }
  }

  public void keyTyped(KeyEvent e) {
    char ch = e.getKeyChar();
    /* // Perhaps cleaner than using keyPressed for enter?
    if (ch == KeyEvent.VK_ENTER)
      {
        enter();
        e.consume();
      }
    else
    */
    if (ch >= ' ' && ch != 127)
      {
        int pos = getCaretPosition();
        char[] chs = { ch };
        insertString(pos, new String(chs), inputStyle);
        setCaretPosition(pos+1);
        e.consume();
      }
  }

  public synchronized void write (String str, AttributeSet style) {
    boolean moveCaret = getCaretPosition() == outputMark;
    insertString(outputMark, str, style);
    int len = str.length();
    outputMark += len;
    if (moveCaret)
      setCaretPosition(outputMark);
    if (endMark >= 0)
      endMark += len;
  }

  public synchronized void write (Component c)
  {
    MutableAttributeSet style = new SimpleAttributeSet();
    StyleConstants.setComponent(style, c);
    write(" ", style);
  }

  /** Delete old text, prior to line containing outputMark. */

  public synchronized void deleteOldText ()
  {
    String str = getText();
    int lineBefore = (outputMark <= 0 ? 0
		      : (str.lastIndexOf('\n', outputMark-1)) + 1);
    try
      {
        document.remove(0, lineBefore);
        outputMark -= lineBefore;
        setCaretPosition(outputMark);
        if (endMark > 0)
          endMark -= lineBefore;
      }
    catch (BadLocationException ex)
      {
        /* #ifdef use:java.lang.Throwable.getCause */
        throw new Error(ex);
        /* #else */
        // throw new WrappedException(ex);
        /* #endif */
      }
  }

  public void changedUpdate (DocumentEvent e) { textValueChanged(e); }
  public void insertUpdate (DocumentEvent e) { textValueChanged(e); }
  public void removeUpdate (DocumentEvent e) { textValueChanged(e); }

  public synchronized void textValueChanged (DocumentEvent e) {
    int pos = getCaretPosition();
    int delta = document.getLength() - length;
    length += delta;
    if (pos < outputMark)
      outputMark += delta;
    else if (pos - delta < outputMark)
      outputMark = pos;
    if (endMark >= 0)
      {
	if (pos < endMark)
	  endMark += delta;
	else if (pos - delta < endMark)
	  endMark = pos;
      }
  }

  public OutPort getStdout() {
    return out_stream;
  }
  public OutPort getStderr() {
    return err_stream;
  }

}
