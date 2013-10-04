package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.ImmutablePair;
import gnu.lists.LList;
import gnu.mapping.Symbol;
import gnu.mapping.SimpleSymbol;
import java.util.ArrayList;
import java.util.List;

public class IfFeature
{
  public static boolean testFeature (Object form)
  {
    if (form instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) form;
	form = sf.getDatum();
      }
    form = ((Translator) Compilation.getCurrent()).namespaceResolve(form);
    if (form instanceof String || form instanceof SimpleSymbol)
      return hasFeature(form.toString());
    return false;  // FIXME - return error
  }

    private static List<String> coreFeatures = new ArrayList<String>();
    static {
        coreFeatures.add("complex");
        coreFeatures.add("exact-complex");
        coreFeatures.add("full-unicode");

        /* #ifdef JAVA6 */
        coreFeatures.add("java-6");
        /* #endif */

        /* #ifdef JAVA7 */
        // coreFeatures.add("java-7");
        /* #endif */

        coreFeatures.add("kawa");
        coreFeatures.add("ratios");

        // coreFeatures.add("r7rs"); // Later, when more is implemented

        coreFeatures.add("srfi-0"); // cond-expand
        // coreFeatures.add("srfi-1"); // lists - only if require used.
        //if (name == "srfi-1") return true; // lists
        coreFeatures.add("srfi-4"); // Homogeneous numeric vector datatypes
        coreFeatures.add("srfi-6"); // Basic String Ports
        coreFeatures.add("srfi-8"); // receive: Binding to multiple values
        coreFeatures.add("srfi-9"); // Defining Record Types
        coreFeatures.add("srfi-11"); // let-values, let*-values
        coreFeatures.add("srfi-16"); // case-lambda
        coreFeatures.add("srfi-17"); // Generalized set!
        coreFeatures.add("srfi-23"); // Error reporting mechanism
        coreFeatures.add("srfi-25"); // Multi-dimensional Array Primitives
        coreFeatures.add("srfi-26"); // Notation for Specializing Parameters
        coreFeatures.add("srfi-28"); // Basic Format Strings
        coreFeatures.add("srfi-30"); // Nested Multi-line Comments.
        coreFeatures.add("srfi-39"); // Parameter objects

        /* #ifdef use:java.text.Normalizer */
        /* #ifdef JAVA6COMPAT5 */
        // try {
        //     Class.forName("java.text.Normalizer");
        //     coreFeatures.add("string-normalize-unicode");
        // }
        // catch (ClassNotFoundException ex) {
        // }
        /* #else */
        coreFeatures.add("string-normalize-unicode");
        /* #endif */
        /* #endif */

        coreFeatures.add("threads");
    }

    /** Check if we implement a named feature.
     * @param name an interned feature name
     */
    public static boolean hasFeature (String name) {
        for (int i = coreFeatures.size();  --i>= 0; ) {
            if (name == coreFeatures.get(i))
                return true;
        }
        if (name == "in-http-server" || name == "in-servlet") {
            int mflags = ModuleContext.getContext().getFlags();
            if (name == "in-http-server")
                return (mflags & ModuleContext.IN_HTTP_SERVER) != 0;
            if (name == "in-servlet")
                return (mflags & ModuleContext.IN_SERVLET) != 0;
        }
    
        String classExistsPrefix = "class-exists:";
        if (name.startsWith(classExistsPrefix)) {
            name = name.substring(classExistsPrefix.length());
            try {
                Class.forName(name, false, IfFeature.class.getClassLoader());
                return true;
            } catch (ClassNotFoundException ex) {
                return false;
            }
        }

        Symbol provide_symbol = Symbol.valueOf(PROVIDE_PREFIX+name);
        Declaration decl = Compilation.getCurrent().lookup(provide_symbol, -1);
        if (decl!=null && ! decl.getFlag(Declaration.IS_UNKNOWN))
            return true;
        return false;
    }

    /** Return a (partial) list of features,
     * The result does not include "provide" feature names - though it should.
     * Feature names of the form class:CLASSNAME are not returned.
     */
    public static LList featureList() {
        LList result = LList.Empty;
        for (int i = coreFeatures.size();  --i>= 0; ) {
            String item = coreFeatures.get(i);
            result = new ImmutablePair(Symbol.valueOf(item), result);
        }
        return result;
    }

  public static final String PROVIDE_PREFIX = "%provide%";

  public static boolean isProvide (Declaration decl)
  {
    return decl.getName().startsWith(PROVIDE_PREFIX);
  }
}
