package kawa;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import gnu.kawa.swingviews.SwingContent;
import gnu.text.Path;
import gnu.expr.Language;
import gnu.mapping.*;

/** A Swing document that implements a read-eval-print-loop. */

public class ReplDocument extends DefaultStyledDocument
  implements DocumentListener, FocusListener
{
  //public static javax.swing.text.html.StyleSheet styles
  // = new javax.swing.text.html.StyleSheet();
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

  JTextPane pane;

  SwingContent content;

  final gnu.text.QueueReader in_r;
  final GuiInPort in_p;
  final ReplPaneOutPort out_stream, err_stream;

  Language language;
  Environment environment;
  Future thread;

  /** The offset where output from process is inserted. */
  public int outputMark = 0;

  /** End of pending input.
   * If {@code endMark > 0} then the area between outputMark and endMark
   * is pending input that hasn't been sent to the process yet. */
  public int endMark = -1;

  int length = 0;

  public ReplDocument (Language language, Environment penvironment, boolean shared)
  {
    this(new SwingContent(), language, penvironment, shared);
  }

  private ReplDocument (SwingContent content, Language language, Environment penvironment, boolean shared)
  {
    super(content, styles);
    this.content = content;
    addDocumentListener(this);

    this.language = language;

    in_r = new gnu.text.QueueReader() {
        public void checkAvailable() {
          checkingPendingInput(); };
    };
    out_stream = new ReplPaneOutPort(this, "/dev/stdout", defaultStyle);
    err_stream = new ReplPaneOutPort(this, "/dev/stderr", redStyle);
    in_p = new GuiInPort(in_r, Path.valueOf("/dev/stdin"),
                         out_stream, this);
    thread = new Future (new kawa.repl(language),
			 penvironment, in_p, out_stream, err_stream);

    Environment env = thread.getEnvironment();
    if (shared)
      env.setIndirectDefines();
    this.environment = env;
    thread.start();
  }

  /** Delete old text, prior to line containing outputMark. */

  public synchronized void deleteOldText ()
  {
    try
      {
        String str = getText(0, outputMark);
        int lineBefore = (outputMark <= 0 ? 0
                          : (str.lastIndexOf('\n', outputMark-1)) + 1);
        remove(0, lineBefore);
        // Changelistener updates outputMark and endMark.
        if (pane != null)
          pane.setCaretPosition(outputMark);
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

  public void insertString(int pos, String str, AttributeSet style)
  {
    try
      {
        super.insertString(pos, str, style);
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

  /** Insert output from the client at the outputMark.
   * Done indirectly, using SwingUtilities.invokeLater, so it can and should
   * should be called by user threads, not the event thread.
   */
  public void write (final String str, final AttributeSet style)
  {
    SwingUtilities.invokeLater(new Runnable() {
      public void run()
      {
        boolean moveCaret
          = pane != null && pane.getCaretPosition() == outputMark;
        insertString(outputMark, str, style);
        int len = str.length();
        outputMark += len;
        if (moveCaret)
         pane.setCaretPosition(outputMark);
      }
    });
  }
  
  /** Check if there is any pending input.
   * Can and should be called by user threads, not the event thread.
   * The tricky aspect is supporting type-ahead and other multi-line input,
   * since we want prompts and other output to be properly interleaved.
   */
  public void checkingPendingInput ()
  {
    SwingUtilities.invokeLater(new Runnable() {
      public void run()
      {
        // For now, we don't support moving the outputMark using escape
        // sequences, so inputStart is always just outputMark.
        int inputStart = outputMark;
        if (inputStart <= endMark)
          {
	    gnu.lists.CharBuffer b = content.buffer;
            int lineAfter = b.indexOf('\n', inputStart);
            if (lineAfter == endMark)
              endMark = -1;
            if (inputStart == outputMark) // Currently, always true.
               outputMark = lineAfter+1;
            if (in_r != null) {
              synchronized (in_r) {
                in_r.append(b.substring(inputStart, lineAfter+1));
                in_r.notifyAll();
              }
	}
        }
      }
    });
  }

  public void focusGained(FocusEvent e)
  {
    Object source = e.getSource();
    if (source instanceof ReplPane)
      {
        pane = (ReplPane) source;
        //pane.getCaret().setDot(outputMark);
      }
    else
      pane = null;
    pane = source instanceof ReplPane ? (ReplPane) source : null;
  }

  public void focusLost(FocusEvent e)
  {
    pane = null;
  }

  public void changedUpdate (DocumentEvent e) { textValueChanged(e); }
  public void insertUpdate (DocumentEvent e) { textValueChanged(e); }
  public void removeUpdate (DocumentEvent e) { textValueChanged(e); }

  public synchronized void textValueChanged (DocumentEvent e) {
    // Update outputMark and endMark
    // Note that inserting at outputMark doesn't change outputMark,
    // That is done by the write method.
    int pos = e.getOffset();
    int delta = getLength() - length;
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

  void close () {
    in_r.appendEOF();
    // Give thread chance to finish and clean up
    try {
      Thread.sleep(100);
    } catch (InterruptedException ex) {
    }
    // Thread.stop is deprecated in JDK 1.2, but I see no good
    // alternative.  (Thread.destroy is not implemented!)
    thread.stop(); 
    kawa.repl.exitDecrement();
 }
}
