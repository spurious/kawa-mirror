package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

public class IfFeature extends Syntax implements Printable
{
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
    return false;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                    ScopeExp defs, Translator tr)
  {
    Object [] match = ListPat.match(3, 3, null, st.cdr);
    if (match == null || ! (match[0] instanceof String))
      {
	forms.addElement(tr.syntaxError("invalid syntax for cond-expand"));
	return false;
      }
    return tr.scan_form(match[hasFeature((String) match[0]) ? 1 : 2],
			forms, defs);
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = ListPat.match(3, 3, null, obj);
    if (match == null || ! (match[0] instanceof String))
      return tr.syntaxError ("invalid syntax for cond-expand");
    return tr.rewrite(match[hasFeature((String) match[0]) ? 1 : 2]);
  }
}
