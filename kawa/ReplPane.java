package kawa;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import gnu.text.Path;
import gnu.mapping.*;
import gnu.lists.CharBuffer;
import gnu.kawa.models.Viewable;
import gnu.kawa.models.Paintable;
import gnu.kawa.swingviews.SwingDisplay;
import gnu.kawa.swingviews.SwingContent;

/** A JTextPane for a read-eval-print-loop.  Also creates an
 * out and err PrintWriter so that you can redirect stdout/stderr to
 * these streams, using the System.setOut/setErr methods.
 *
 * @author 	Albert Ting
 * @author      Per Bothner
 */
public class ReplPane extends JTextPane
  implements KeyListener
{
  ReplDocument document;

  /**
   * simple TextArea that always scrolls to the bottom.  Also creates an
   * out and err PrintWriter so that you can redirect stdout/stderr to
   * these streams, using the System.setOut/setErr methods.
   */
  public ReplPane(ReplDocument document)
  {
    super(document);
    this.document = document;
    document.pane = this;
    document.paneCount++;

    addKeyListener(this);
    addFocusListener(document);

    EditorKit kit = getEditorKit();
    setCaretPosition(document.outputMark);
  }

  protected EditorKit createDefaultEditorKit() {
    return new ReplEditorKit(this);
  }

  /** This method is called by the toolkit when the component is removed.
   * Used as a hook to decrement ReplDocument's reference count,
   * and maybe close the document.
   */
  public void removeNotify()
  {
    super.removeNotify();
    if (--document.paneCount == 0)
      document.close();
  }

  void enter () // FIXME - move to document
  {
    // In the future we might handle curses-like applicatons, which
    // outputMark may be moved backwards.  In that case the normal
    // case is that caret position >= outputMark and all the intervening
    // text has style inputStyle, in which case we do:
    // Find the first character following outputMark that is either
    // - the final newline in the buffer (in which case insert a newline
    //   at the end and treat as the following case);
    // - a non-final newline (in which case pass the intervening text
    //   and the following newline are stuffed into the in queue,
    //   and the outputMark is set after the newline);
    // - a character whose style is not inputStyle is seen (in which
    //   case the intervening text plus a final newline are stuffed
    //   into the 'in' queue, and the outputMark is moved to just
    //   before the non-inputStyle character).
    // In the second case, if there is more input following the newline,
    // we defer the rest until the inferior requests a new line.  This
    // is so any output and prompt can get properly interleaved.
    // For now, since we don't support backwards movement, we don't
    // check for inputStyle, in this case.

    // Otherwise, we do similar to Emacs shell mode:
    // Select the line containing the caret, stripping of any
    // characters that have prompt style, and add a newline.
    // That should be sent to the inferior, and copied it before outputMark.

    int pos = getCaretPosition();
    CharBuffer b = document.content.buffer;
    int len = b.length() - 1; // Ignore final newline.
    document.endMark = -1;
    if (pos >= document.outputMark)
      {
        int lineAfterCaret = b.indexOf('\n', pos);
        if (lineAfterCaret == len)
          {
            if (len > document.outputMark && b.charAt(len-1) == '\n')
              lineAfterCaret--;
            else
              document.insertString(len, "\n", null);
          }
        document.endMark = lineAfterCaret;
        // Note we don't actually send the input line to the inferior
        // directly.  That happens in ReplDocument.checkingPendingInput,
        // which is invoked by the inferior thread when it is woken up here.
        // We do it this way to handle interleaving prompts and other output
        // with multi-line input, including type-ahead.
        synchronized (document.in_r)
          {
            document.in_r.notifyAll();
          }
        if (pos <= lineAfterCaret)
          setCaretPosition(lineAfterCaret+1);
      }
    else
      {
        int lineBefore = pos == 0 ? 0 : 1 + b.lastIndexOf('\n', pos-1);
        Element el = document.getCharacterElement(lineBefore);
        int lineAfter = b.indexOf('\n', pos);
        // Strip initial prompt:
        if (el.getAttributes().isEqual(ReplDocument.promptStyle))
          lineBefore = el.getEndOffset();
        String str;
        if (lineAfter < 0)
          str = b.substring(lineBefore, len)+'\n';
        else
          str = b.substring(lineBefore, lineAfter+1);
        setCaretPosition(document.outputMark);
        document.write(str, ReplDocument.inputStyle);

	if (document.in_r != null) {
	  document.in_r.append(str, 0, str.length());
	}
      }
  }

  public MutableAttributeSet getInputAttributes()
  {
    return ReplDocument.inputStyle;
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

  public OutPort getStdout() {
    return document.out_stream;
  }
  public OutPort getStderr() {
    return document.err_stream;
  }

  public static final String ViewableElementName = "Viewable";
  public static final String PaintableElementName = "Paintable";
  public static final Object ViewableAttribute =
    new String(ViewableElementName);
  public static final Object PaintableAttribute =
    new String(PaintableElementName);
}

class ReplEditorKit extends StyledEditorKit {
  ViewFactory styledFactory;
  ViewFactory factory;
  final ReplPane pane;

  public ReplEditorKit(final ReplPane pane)
  {
    this.pane = pane;
    styledFactory = super.getViewFactory();
    factory = new ViewFactory ()
      {
        public View create(Element elem)
        {
          String kind = elem.getName();
          if (kind == ReplPane.ViewableElementName)
            {
              return (new ComponentView(elem)
                {
                  protected Component createComponent()
                  {
                    AttributeSet attr = getElement().getAttributes();
                    JPanel panel = new JPanel();
                    Viewable v = (Viewable) attr.getAttribute(ReplPane.ViewableAttribute);
                    Component comp;
                    // A kludge: We create a panel, and then since all current
                    // Viewables just create a Component and put it in the
                    // panel, we get rid of the useless JPanel.
                    v.makeView(SwingDisplay.getInstance(), panel);
                    if (panel.getComponentCount() == 1)
                      {
                        comp = panel.getComponent(0);
                        panel.removeAll();
                      }
                    else
                      {
                        panel.setBackground(pane.getBackground());
                        comp = panel;
                      }
                    return comp;
                  }
                });
            }
          else if (kind == ReplPane.PaintableElementName)
            {
              AttributeSet attr = elem.getAttributes();
              return new PaintableView(elem, (Paintable) attr.getAttribute(ReplPane.PaintableAttribute));
            }
          return styledFactory.create(elem);
        }
      };
  }

  public ViewFactory getViewFactory ()
  {
    return factory;
  }
}

class PaintableView extends View
{
  Paintable p;
  Rectangle2D bounds;
  public PaintableView (Element elem, Paintable paintable)
  {
    super(elem);
    this.p = paintable;
    this.bounds = paintable.getBounds2D();
  }

  public void paint(Graphics g, Shape a)
  {
    Graphics2D g2 = (Graphics2D) g;
    Rectangle bounds = a.getBounds();
    AffineTransform saveTransform = g2.getTransform();
    Paint savePaint = g2.getPaint();
    try
      {
        g2.translate(bounds.x, bounds.y);
        g2.setPaint(Color.BLACK); // FIXME
        p.paint(g2);
      }
    finally
      {
        g2.setTransform(saveTransform);
        g2.setPaint(savePaint);
      }
  }

  public float getAlignment(int axis)
  {
    switch (axis)
      {
      case View.Y_AXIS:
        return 1;
      default:
        return super.getAlignment(axis);
      }
  }

   public float getPreferredSpan(int axis) {
        switch (axis) {
        case View.X_AXIS:
          return (float) bounds.getWidth();
        case View.Y_AXIS:
          return (float) bounds.getHeight();
        default:
            throw new IllegalArgumentException("Invalid axis: " + axis);
        }
    }

    public Shape modelToView(int pos, Shape a, Position.Bias b) throws BadLocationException {
        int p0 = getStartOffset();
        int p1 = getEndOffset();
        if ((pos >= p0) && (pos <= p1)) {
            Rectangle r = a.getBounds();
            if (pos == p1) {
                r.x += r.width;
            }
            r.width = 0;
            return r;
        }
        throw new BadLocationException(pos + " not in range " + p0 + "," + p1, pos);
    }

   public int viewToModel(float x, float y, Shape a, Position.Bias[] bias) {
        Rectangle alloc = (Rectangle) a;
        if (x < alloc.x + (alloc.width / 2)) {
            bias[0] = Position.Bias.Forward;
            return getStartOffset();
        }
        bias[0] = Position.Bias.Backward;
        return getEndOffset();
    }
}
