package kawa.standard;
import kawa.lang.*;
             
public class open_input_file extends Procedure1
{
  public Object apply1 (Object arg1)
     throws GenericError
  {
    String fname = arg1.toString();

    try {
      Object conv = Environment.user().get("port-char-encoding");
      java.io.InputStream strm = new java.io.FileInputStream(fname);
      strm = new java.io.BufferedInputStream(strm);
      java.io.Reader rd;
      if (conv == null || conv == Boolean.TRUE)
	rd = new java.io.InputStreamReader(strm);
      else
	{
	  String converter;
	  if (conv == Boolean.FALSE)
	    converter = "8859_1";
	  else
	    converter = conv.toString();
	  rd = new java.io.InputStreamReader(strm, converter);
	}

      InPort port = new InPort(rd, fname);
      if (conv == Boolean.FALSE)
	port.setConvertCR(false);
      return port;
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    } catch (java.io.IOException e) {
      throw new GenericError(e.getMessage());
    }
  }
}
