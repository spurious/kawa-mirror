package kawa.standard;
import kawa.lang.*;
             
public class open_output_file extends Procedure1
{
  public Object apply1 (Object arg1) 
    throws GenericError
  {
    String fname = arg1.toString();

    try {
      java.io.Writer os = new java.io.FileWriter(fname);
      return new OutPort(os, fname);
    } catch (java.io.IOException e) {
      throw new GenericError(e.getMessage());
    }
  }
}
