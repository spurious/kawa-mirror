package kawa;

import java.io.*;
import java.lang.*;
import java.awt.*;
import java.awt.event.*;
import java.text.*;
import java.util.*;

/** Simple TextArea that always scrolls to the bottom.  Also creates an
 * out and err PrintWriter so that you can redirect stdout/stderr to
 * these streams, using the System.setOut/setErr methods.
 *
 * @version 	$Revision$
 * @author 	Albert Ting
 */
public class MessageArea extends TextArea implements KeyListener {
  private boolean doFocus = false;
  private PrintWriter out;
  private PrintWriter err;
  kawa.lang.QueueReader in;

  public int outputMark = 0;

  /**
   * simple TextArea that always scrolls to the bottom.  Also creates an
   * out and err PrintWriter so that you can redirect stdout/stderr to
   * these streams, using the System.setOut/setErr methods.
   *
   * @param     focus    specifies if this is focus traversable
   */
  public MessageArea(boolean focus, kawa.lang.QueueReader in) {
    super();

    this.doFocus = focus;
    this.in = in;

    // we use a modified version of MessageOutputStream since we need to call
    // the textarea append function
    kawa.TextAreaWriter out_stream = new kawa.TextAreaWriter(this);
    kawa.TextAreaWriter err_stream = new kawa.TextAreaWriter(this);
    out = new PrintWriter(out_stream);
    err = new PrintWriter(err_stream);

    addKeyListener(this);
  }

  public boolean isFocusTraversable() {
    return(doFocus && super.isFocusTraversable());
  }

  void enter (KeyEvent e) {
	int pos = getCaretPosition();
	String str = getText();
	int len = str.length();
	int lineAfter = str.indexOf('\n', pos);
	if (lineAfter < 0)
	  lineAfter = len;
	int lineBefore = pos == 0 ? 0 : 1 + str.lastIndexOf('\n', pos-1);
	if (lineBefore < outputMark && lineAfter >= outputMark)
	  lineBefore = outputMark;
	str = str.substring(lineBefore, lineAfter);
	if (lineAfter < len) {
	  append(str);
	  len += lineAfter - lineBefore;
	}
	append("\n");
	outputMark = len+1;
	setCaretPosition(outputMark);
	e.consume();
	if (in != null) {
	  in.append(str);
	  in.append('\n');
	}
	  
  }

  public void keyPressed(KeyEvent e) {
    int code = e.getKeyCode();
    if (code == KeyEvent.VK_ENTER)
      {
	enter(e);
      }
  }
  public void keyReleased(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
  }

  public PrintWriter getStdout() {
    return out;
  }
  public PrintWriter getStderr() {
    return err;
  }

}
