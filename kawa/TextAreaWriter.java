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
    str = str + (char) x;
    if (x == '\n')
      flush();
  }

  public void write (String str)
  {
    if (area instanceof MessageArea)
      {
	MessageArea msg = (MessageArea) area;
	msg.write(str);
      }
    else
      area.append(str);
  }

  public synchronized void write (char[] data, int off, int len)
  {
    flush();
    write(new String(data, off, len));
  }

  public synchronized void flush()
  {
    if (! str.equals(""))
      {
	write(str);
	str = "";
      }
  }

  public void close ()
  {
    flush();
  }
}
