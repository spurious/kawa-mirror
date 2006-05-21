package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

public class IfFeature
{
  public static boolean testFeature (Object form)
  {
    if (form instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) form;
	form = sf.form;
      }
    if (form instanceof String)
      return hasFeature((String) form);
    return false;  // FIXME - return error
  }

  public static boolean hasFeature (String name)
  {
    if (name == "kawa")
      return true;
    if (name == "srfi-0") // cond-expand
      return true;
    //if (name == "srfi-1") return true; // lists - only if require used.
    if (name == "srfi-4") // Homogeneous numeric vector datatypes
      return true;
    if (name == "srfi-6") // Basic String Ports
      return true;
    if (name == "srfi-8") // receive: Binding to multiple values
      return true;
    if (name == "srfi-9") // Defining Record Types
      return true;
    if (name == "srfi-11") // let-values, let*-values
      return true;
    if (name == "srfi-16") // case-lambda
      return true;
    if (name == "srfi-17") // Generalized set!
      return true;
    if (name == "srfi-23") // Error reporting mechanism
      return true;
    if (name == "srfi-25") // Multi-dimensional Array Primitives
      return true;
    if (name == "srfi-26") // Notation for Specializing Parameters
      return true;
    if (name == "srfi-28") // Basic Format Strings
      return true;
    if (name == "srfi-30") // Nested Multi-line Comments.
      return true;
    if (name == "srfi-39") // Parameter objects
      return true;

    String provide_name = ("%provide%"+name).intern();
    Compilation comp = Compilation.getCurrent();
    Declaration decl = comp.lookup(provide_name, -1);
    if (decl!=null && ! decl.getFlag(Declaration.IS_UNKNOWN))
      return true;
    return false;
  }
}
