package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
             
public class call_with_output_string extends Procedure1
{
  public Object apply1 (Object proc)
  {
    java.io.CharArrayWriter wr = new java.io.CharArrayWriter ();
    OutPort port = new OutPort(wr, "<string>");
    ((Procedure)proc).apply1 (port);
    port.close ();
    return new FString(wr.toCharArray());
  }
}
