package kawa.standard;
import gnu.kawa.util.FString;
import gnu.mapping.*;
             
public class call_with_output_string extends Procedure1
{
  public Object apply1 (Object proc)
  {
    CharArrayOutPort port = new CharArrayOutPort();
    ((Procedure)proc).apply1 (port);
    char[] chars = port.toCharArray();
    port.close ();
    return new FString(chars);
  }
}
