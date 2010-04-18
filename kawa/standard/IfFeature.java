package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.mapping.SimpleSymbol;

public class IfFeature
{
  public static boolean testFeature (Object form)
  {
    if (form instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) form;
	form = sf.getDatum();
      }
    if (form instanceof String || form instanceof SimpleSymbol)
      return hasFeature(form.toString());
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
    /* #ifdef use:java.text.Normalizer */
    if (name == "string-normalize-unicode")
      {
        /* #ifdef JAVA6COMPAT5 */
        // try
        //   {
        //     Class.forName("java.text.Normalizer");
        //     return true;
        //   }
        // catch (ClassNotFoundException ex)
        //   {
        //     return false;
        //   }
        /* #else */
        return true;
        /* #endif */
      }
    /* #endif */
    if (name == "in-http-server" || name == "in-servlet")
      {
        int mflags = ModuleContext.getContext().getFlags();
        if (name == "in-http-server")
          return (mflags & ModuleContext.IN_HTTP_SERVER) != 0;
        if (name == "in-servlet")
          return (mflags & ModuleContext.IN_SERVLET) != 0;
      }

    String provide_name = ("%provide%"+name).intern();
    Compilation comp = Compilation.getCurrent();
    Declaration decl = comp.lookup(provide_name, -1);
    if (decl!=null && ! decl.getFlag(Declaration.IS_UNKNOWN))
      return true;
    return false;
  }
}
