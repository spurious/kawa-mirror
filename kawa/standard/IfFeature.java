package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

public class IfFeature extends Syntax implements Printable
{
  public static boolean hasFeature (String name)
  {
    boolean b = hasFeaturex(name);
    System.err.println("hasFeature("+name+")->"+b);
    return b;
  }

  public static boolean hasFeaturex (String name)
  {
    if (name == "srfi-0") // cond-expand
      return true;
    //if (name == "srfi-1") return true; // lists - only if require used.
    if (name == "srfi-4") // Homogeneous numeric vector datatypes
      return true;
    if (name == "srfi-6") // Basic String Ports
      return true;
    if (name == "srfi-8") // receive: Binding to multiple values
      return true;
    if (name == "srfi-11") // let-values, let*-values
      return true;
    if (name == "srfi-17") // Generalized set!
      return true;
    if (name == "srfi-23") // Error reporting mechanism
      return true;
    return false;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = ListPat.match(3, 3, null, obj);
    if (match == null || ! (match[0] instanceof String))
      return tr.syntaxError ("invalid syntax for if");
    return tr.rewrite(match[hasFeature((String) match[0]) ? 1 : 2]);
  }
}
