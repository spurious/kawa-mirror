package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/** Implements the R5RS procedure "interaction-environment". */

public class user_env extends Procedure0
{
  public Object apply0 ()
  {
    return Environment.user();
  }
}
