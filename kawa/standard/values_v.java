package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/**
 * Implement the Scheme R5RS function "values".
 * @author Per Bothner
 */

public class values_v extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    return Values.make(args);
  }
}
