package kawa.standard;
import kawa.lang.*;
             
public class open_input_file extends Procedure1
{
  public open_input_file()
  {
    super("open-input-file");
  }

  public Object apply1 (Object arg1)
     throws GenericError
  {
    String fname = ((StringBuffer)arg1).toString();

    try {
      java.io.InputStream is = new java.io.FileInputStream(fname);
      return new InPort(is, fname);
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    }
  }
}
