package gnu.expr;

import gnu.mapping.*;
import gnu.lists.*;

/** Run-time support for "main" method, inclding command-line arguments. */

public class ApplicationMainSupport
{
  public static boolean processCommandLinePropertyAssignments;

  public static String[] commandLineArgArray;

  public static FVector commandLineArguments;

  public static void processSetProperties ()
  {
    String[] args = commandLineArgArray;
    if (args == null)
      processCommandLinePropertyAssignments = true;
    else
      {
        int iarg = 0;
        while (iarg < args.length && processSetProperty(args[iarg]))
          iarg++;
        if (iarg != 0)
          setArgs(args, iarg);
      }
  }

  /** This is invoked by main when ModuleBody is compiled with --main. */
  public static void processArgs (String[] args)
  {
    int iarg = 0;
    if (processCommandLinePropertyAssignments)
      {
        while (iarg < args.length && processSetProperty(args[iarg]))
          iarg++;
      }
    setArgs(args, iarg);
  }

  public static void setArgs (String[] args, int arg_start)
  {
    int nargs = args.length - arg_start;
    Object[] array = new Object[nargs];
    if (arg_start == 0)
     commandLineArgArray = args;
    else
      {
	String[] strings = new String[nargs];
	for (int i = nargs;  --i >= 0; )
	  strings[i] = args[i+arg_start];
	commandLineArgArray = strings;
      }
    for (int i = nargs;  --i >= 0; )
      array[i] = new FString (args[i + arg_start]);
    commandLineArguments = new FVector (array);  // FIXME scsh has list
    // FIXME scsh also has command-line proc
    Environment.getCurrent().put("command-line-arguments",
                                 commandLineArguments);
  }

  public static boolean processSetProperty (String arg)
  {
    int ci = arg.indexOf('=');
    if (ci <= 0)
      return false;
    String key = arg.substring(0, ci);
    String value = arg.substring(ci+1);
    for (int i = 0; ; i++)
      {
        String[] propertyField = propertyFields[i];
        if (propertyField == null)
          break;
        if (key.equals(propertyField[0]))
          {
            String cname = propertyField[1];
            String fname = propertyField[2];
            try
              {
                Class clas = Class.forName(cname);
                ThreadLocation loc = (ThreadLocation)
                  clas.getDeclaredField(fname).get(null);
                loc.setGlobal(value);
                break;
              }
            catch (Throwable ex)
              {
                System.err.println("error setting property " + key
                                   +" field "+cname+'.'+fname+": "+ex);
                System.exit(-1);
              }
          }
      }
    Symbol symbol = Symbol.parse(key);
    // Run Language's static initializer.
    Language.getDefaultLanguage();
    Environment current = Environment.getCurrent();
    current.define(symbol, null, value);
    return true;
  }

  /** A list of standard command-line fluid names to map to static fields.
   * For each entry:
   * element 0 is a property name (before the '=' in the comamnd-line);
   * element 1 is the name of a class;
   * element 2 is the name of a static ThreadLocation field. */
  static String[][] propertyFields =
    {
      { "out:doctype-system", "gnu.xml.XMLPrinter", "doctypeSystem" },
      { "out:doctype-public", "gnu.xml.XMLPrinter", "doctypePublic" },
      { "out:base", "gnu.kawa.functions.DisplayFormat", "outBase" },
      { "out:radix", "gnu.kawa.functions.DisplayFormat", "outRadix" },
      { "out:line-length", "gnu.text.PrettyWriter", "lineLengthLoc" },
      { "out:right-margin", "gnu.text.PrettyWriter", "lineLengthLoc" },
      { "out:miser-width", "gnu.text.PrettyWriter", "miserWidthLoc" },
      { "out:xml-indent", "gnu.xml.XMLPrinter", "indentLoc" },
      { "display:toolkit", "gnu.kawa.models.Display", "myDisplay" },
      null
    };
}
