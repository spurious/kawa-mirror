package kawa.standard;
import gnu.mapping.*;

/** Implements the R5RS procedure "null-environment". */

public class null_env extends Procedure0
{
  public Object apply0 ()
  {
    return Scheme.null_environment;
  }
}
