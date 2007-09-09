package kawa;
import java.awt.Component;
import javax.swing.*;
import javax.swing.text.*;
import gnu.mapping.*;
import gnu.text.Path;
import gnu.kawa.models.Paintable;
import gnu.kawa.models.Viewable;
import gnu.kawa.swingviews.SwingDisplay;

/** A Writer that appends its output to a ReplPane.
  * Based on code from Albert L. Ting" <alt@artisan.com>.
  */

public class ReplPaneOutPort extends OutPort
{
  ReplPane pane;
  AttributeSet style;
  String str="";

  public ReplPaneOutPort (ReplPane pane, String path, AttributeSet style)
  {
    super(new TextPaneWriter(pane, style), true, true, Path.valueOf(path));
    this.pane = pane;
    this.style = style;
  }

  public synchronized void write (Component c)
  {
    MutableAttributeSet style = new SimpleAttributeSet();
    StyleConstants.setComponent(style, c);
    pane.write(" ", style);
    setColumnNumber(1); // So freshline will Do The Right Thing.
  }

  public void print(Object v)
  {
    if (v instanceof Component)
      {
        flush();
        write((Component) v);
      }
    else if (v instanceof Paintable)
      {
        flush();
        JPanel panel = new gnu.kawa.swingviews.SwingPaintable((Paintable) v);
        write((java.awt.Component) panel);
      }
    else if (v instanceof Viewable)
      {
        flush();
        JPanel panel = new JPanel();
        panel.setBackground(pane.getBackground());
        write((java.awt.Component) panel);
        ((Viewable) v).makeView(SwingDisplay.getInstance(), panel);
      }
    else
      super.print(v);
  }
}

class TextPaneWriter extends java.io.Writer
{
  ReplPane pane;
  AttributeSet style;
  String str="";

  public TextPaneWriter (ReplPane pane, AttributeSet style)
  {
    this.pane = pane;
    this.style = style;
  }

  public synchronized void write (int x)
  {
    str = str + (char) x;
    if (x == '\n')
      flush();
  }

  public void write (String str)
  {
    pane.write(str, style);
  }

  public synchronized void write (char[] data, int off, int len)
  {
    flush();
    write(new String(data, off, len));
  }

  public synchronized void flush()
  {
    String s = str;
    if (! s.equals(""))
      {
        str = "";
	write(s);
      }
  }

  public void close ()
  {
    flush();
  }
}
