package kawa.standard;
import kawa.lang.*;
             
public class open_input_file extends Procedure1
{
  public Object apply1 (Object arg1)
     throws GenericError
  {
    return openInputFile(arg1.toString());
  }

  public static InPort openInputFile(String fname)
  {
    try {
      return InPort.openFile(fname);
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    } catch (java.io.IOException e) {
      throw new GenericError(e.getMessage());
    }
  }
}
