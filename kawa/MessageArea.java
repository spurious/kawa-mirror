package kawa;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.text.*;
import java.util.*;

/** Simple TextArea that always scrolls to the bottom.  Also creates an
 * out and err PrintWriter so that you can redirect stdout/stderr to
 * these streams, using the System.setOut/setErr methods.
 *
 * @author 	Albert Ting
 */
public class MessageArea extends TextArea
  implements KeyListener, TextListener {
  private boolean doFocus = false;
  private kawa.TextAreaWriter out_stream;
  private PrintWriter out;
  private PrintWriter err;
  gnu.text.QueueReader in;

  public int outputMark = 0;
  public int endMark = -1;
  int length = 0;

  /**
   * simple TextArea that always scrolls to the bottom.  Also creates an
   * out and err PrintWriter so that you can redirect stdout/stderr to
   * these streams, using the System.setOut/setErr methods.
   *
   * @param     focus    specifies if this is focus traversable
   */
  public MessageArea(boolean focus, gnu.text.QueueReader in) {
    super();

    this.doFocus = focus;
    this.in = in;

    out_stream = new kawa.TextAreaWriter(this);
    kawa.TextAreaWriter err_stream = new kawa.TextAreaWriter(this);
    out = new PrintWriter(out_stream);
    err = new PrintWriter(err_stream);

    addKeyListener(this);
    addTextListener(this);
  }

  public boolean isFocusTraversable() {
    return(doFocus && super.isFocusTraversable());
  }

  void enter () {
	int pos = getCaretPosition();
	String str = getText();
	int len = str.length();
	if (len != length) {
	  System.err.println("(actual) len:"+len + " (saved) length:"+length);
	}
	int lineAfter = str.indexOf('\n', pos);
	endMark = -1;
	if (lineAfter < 0)
	  lineAfter = len;
	int lineBefore = pos == 0 ? 0 : 1 + str.lastIndexOf('\n', pos-1);
	if (pos >= outputMark || outputMark <= lineAfter) {
	  lineAfter = str.indexOf('\n', outputMark);
	  if (lineAfter < 0) {
	    lineAfter = len;
	    append("\n");
	  }
	  else
	    endMark = len;
	  str = str.substring(outputMark, lineAfter);
	  str = str + '\n';
	  outputMark = lineAfter+1;
	}
	else {
	  str = str.substring(lineBefore, lineAfter+1);
	  out_stream.write(str);
	}

	setCaretPosition(outputMark);

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

  public void keyTyped(KeyEvent e) {
  }

  public synchronized void write (String str) {
    insert(str, outputMark);
    int len = str.length();
    outputMark += len;
    if (endMark >= 0)
      endMark += len;
  }

  /** Delete old text, prior to line containing outputMark. */

  public synchronized void deleteOldText () {
    String str = getText();
    int lineBefore = (outputMark <= 0 ? 0
		      : (str.lastIndexOf('\n', outputMark-1)) + 1);
    setCaretPosition(outputMark);
    replaceRange("", 0, lineBefore);
  }

  public synchronized void textValueChanged (TextEvent e) {
    int pos = getCaretPosition();
    String text = getText();
    int delta = text.length() - length;
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

  public PrintWriter getStdout() {
    return out;
  }
  public PrintWriter getStderr() {
    return err;
  }

}
