package kawa;
import javax.swing.*;
import javax.swing.text.*;
import gnu.mapping.*;
import gnu.text.Path;

/** A Writer that appends its output to a ReplPane.
  * Based on code from Albert L. Ting" <alt@artisan.com>.
  */

public class ReplPaneOutPort extends OutPort
{
  ReplPane area;
  AttributeSet style;
  String str="";

  public ReplPaneOutPort (ReplPane area, String path, AttributeSet style)
  {
    super(new TextPaneWriter(area, style), true, true, Path.valueOf(path));
    this.area = area;
    this.style = style;
  }

  public void print(Object v)
  {
    if (v instanceof java.awt.Component)
      {
        flush();
        area.write((java.awt.Component) v);
        setColumnNumber(1); // So freshline will Do The Right Thing.
      }
    else
      super.print(v);
  }
}

class TextPaneWriter extends java.io.Writer
{
  ReplPane area;
  AttributeSet style;
  String str="";

  public TextPaneWriter (ReplPane area, AttributeSet style)
  {
    this.area = area;
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
    area.write(str, style);
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
