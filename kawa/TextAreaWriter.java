package kawa;

/** A Writer that appends its output to a TextArea.
  * Based on code from Albert L. Ting" <alt@artisan.com>.
  */

public class TextAreaWriter extends java.io. Writer
{
  java.awt.TextArea area;
  String str="";

  public TextAreaWriter (java.awt.TextArea area)
  {
    this.area = area;
  }

  public synchronized void write(int x)
  {
    str = str + x;
    if (x == '\n')
      flush();
  }

  public synchronized void write (char[] data, int off, int len)
  {
    flush();
    area.append(new String(data, off, len));
  }

  public synchronized void flush()
  {
    if (! str.equals(""))
      {
	area.append(str);
	str = "";
      }
  }

  public void close ()
  {
    flush();
  }
}
