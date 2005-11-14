package gnu.lists;
import gnu.mapping.*;
import java.text.FieldPosition;

public abstract class AbstractFormat extends java.text.Format
{
  protected void write(String str, Consumer out)
  {
    if (out instanceof OutPort)
      ((OutPort) out).write(str);
    else
      out.writeChars(str);
  }

  public void writeChar(int v, Consumer out)
  {
    out.writeChar(v);
  }

  /** Write a long.
   * The default is to call writeLong on teh Consumer. */
  public void writeLong(long v, Consumer out)
  {
    out.writeLong(v);
  }

  /** Write an int.
   * The default is to call writeLong, so sub-classes only need to
   * override the latter. */
  public void writeInt(int i, Consumer out)
  {
    writeLong(i, out);
  }

  public void writeBoolean(boolean v, Consumer out)
  {
    out.writeBoolean(v);
  }

  public void beginGroup(String typeName, Object type, Consumer out)
  {
    write("(", out);
    write(typeName, out);
  }

  public void endGroup(String typeName, Consumer out)
  {
    write(")", out);
  }

  public void beginAttribute(String attrName, Object attrType, Consumer out)
  {
    write(" ", out);
    write(attrName, out);
    write(": ", out);
  }

  public void endAttribute(Consumer out)
  {
    write(" ", out);  // FIXME
  }

  public abstract void writeObject(Object v, Consumer out);

  public void format (Object value, Consumer out)
  {
    if (out instanceof OutPort)
      {
	OutPort pout = (OutPort) out;
	AbstractFormat saveFormat = pout.objectFormat;
	try
	  {
	    pout.objectFormat = this;
	    out.writeObject(value);
	  }
	finally
	  {
	    pout.objectFormat = saveFormat;
	  }
      }
    else
      out.writeObject(value);
  }

  public final void writeObject (Object obj, PrintConsumer out)
  {
    writeObject(obj, (Consumer) out);
  }

  public final void writeObject (Object obj, java.io.Writer out)
  {
    if (out instanceof Consumer)
      writeObject(obj, (Consumer) out);
    else
      {
	OutPort port = new OutPort(out, false, true, "<unknown>");
	writeObject(obj, (Consumer) out);
	port.close();
      }
  }

  public StringBuffer format(Object val, StringBuffer sbuf, FieldPosition fpos)
  {
    CharArrayOutPort out = new CharArrayOutPort();
    writeObject(val, out);
    sbuf.append(out.toCharArray());
    out.close();
    return sbuf;
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error(this.getClass().getName()
                    + ".parseObject - not implemented");
  }
}
