package kawa.standard;
import kawa.lang.*;

public class vector extends ProcedureN
{
  public vector()
  {
    super("vector");
  }

  public Object applyN (Object[] args)
  {
    return new Vector (args);
  }
}
