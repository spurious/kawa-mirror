package kawa.standard;
import kawa.lang.*;
             
public class open_input_file extends Procedure1
{
  public Object apply1 (Object arg1)
     throws GenericError
  {
    String fname = arg1.toString();

    try {
      return new InPort(new java.io.FileReader(fname), fname);
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    }
  }
}
