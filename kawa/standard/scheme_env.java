package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;
import gnu.mapping.*;

/** Implements the R5RS procedure "scheme-report-environment". */

public class scheme_env extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    int version = IntNum.intValue (arg1);
    if (version == 5)
      return Scheme.r5Environment;
    if (version == 4)
      return Scheme.r4Environment;
    throw new GenericError ("scheme-report-environment version must be 4 or 5");
  }
}
