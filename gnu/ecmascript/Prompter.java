package gnu.ecmascript;
import gnu.mapping.*;

class Prompter extends Procedure1
{
  String prompt(InPort port)
  {
    return "(EcmaScript:" + (port.getLineNumber()+1) + ") ";
  }

  public Object apply1(Object arg)
  {
    return prompt((InPort) arg);
  }
}
