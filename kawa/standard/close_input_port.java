package kawa.standard;
import gnu.mapping.*;
             
public class close_input_port extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    try {
       ((InPort)arg1).close();
    } catch (java.io.IOException e) {
      throw new RuntimeException(e.getMessage());
    }
    return Values.empty;
  }
}
