package gnu.kawa.util;
import gnu.mapping.*;
import gnu.lists.*;

public abstract class AbstractFormat extends Procedure1or2 implements FormatToConsumer
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

  public void writeBoolean(boolean v, Consumer out)
  {
    write(v ? "true" : "false", out);
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

  public Object apply1 (Object arg1)
  {
    format (arg1, OutPort.outDefault());
    return Values.empty;
  }

  public Object apply2 (Object arg1,Object arg2)
  {
    format (arg1, (Consumer) arg2); 
    return Values.empty;
  }

  public void format (Object value, Consumer out)
  {
    if (out instanceof OutPort)
      {
	OutPort pout = (OutPort) out;
	FormatToConsumer saveFormat = pout.objectFormat;
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
}
