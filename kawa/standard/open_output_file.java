package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.OutPort;
import gnu.mapping.Environment;
             
public class open_output_file extends Procedure1
{
  public Object apply1 (Object arg1) 
  {
    String fname = arg1.toString();

    try {
      Object conv = Environment.user().get("port-char-encoding");
      java.io.OutputStream strm = new java.io.FileOutputStream(fname);
      strm = new java.io.BufferedOutputStream(strm);
      java.io.Writer wr;
      if (conv == null || conv == Boolean.TRUE)
	wr = new java.io.OutputStreamWriter(strm);
      else
	{
	  if (conv == Boolean.FALSE)
	    conv = "8859_1";
	  wr = new java.io.OutputStreamWriter(strm, conv.toString());
	}
      return new OutPort(wr, fname);
    } catch (java.io.IOException e) {
      throw new RuntimeException(e.getMessage());
    }
  }
}
