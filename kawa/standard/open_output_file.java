package kawa.standard;
import kawa.lang.*;
             
public class open_output_file extends Procedure1
{
  public open_output_file()
  {
    super("open-output-file");
  }

  public Object apply1 (Object arg1) 
    throws GenericError
  {
    String fname = ((StringBuffer)arg1).toString();

    try {
      java.io.OutputStream os = new java.io.FileOutputStream(fname);
      return new OutPort(os, fname);
    } catch (java.io.IOException e) {
      throw new GenericError(e.getMessage());
    }
  }
}
