package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;

/** Implements the R5RS procedure "scheme-report-environment". */

public class scheme_env extends Procedure1
{
  public Object apply1 (Object arg1)
       throws GenericError
  {
    int version = IntNum.intValue (arg1);
    if (version == 5)
      return Scheme.r5_environment;
    if (version == 4)
      return Scheme.r4_environment;
    throw new GenericError ("scheme-report-environment version must be 4 or 5");
  }
}
