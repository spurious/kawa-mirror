package kawa;

import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.bytecode.ArrayClassLoader;

/** Utility functions (static methods) for kawa.repl.
 * Should probably be merged with kawa.repl.  FIXME. */

public class Shell
{
  private static Class[] noClasses = { };
  private static  Class[] boolClasses = { Boolean.TYPE };
  private static  Class[] xmlPrinterClasses
    = {gnu.mapping.OutPort.class,  java.lang.Object.class };
  private static  Class[] httpPrinterClasses
    = {gnu.mapping.OutPort.class };
  private static Object portArg = "(port)";

  /** A table of names of known output formats.
   * For each entry, the first Object is the format name.
   * The next entries are a class name, the name of a static method in that
   * class, and the parameter types (as a Class[] suitable for getMethod).
   * The remain values are arguments (passed to invoke), except that if an
   * argument is the spacial value portArg, it is replaced by the
   * destination OutPort. */
   
  static Object[][] formats =
    {
      { "scheme", "gnu.kawa.functions.DisplayFormat",
	"getSchemeFormat", boolClasses,
	Boolean.FALSE },
      { "readable-scheme", "gnu.kawa.functions.DisplayFormat", 
	"getSchemeFormat", boolClasses,
	Boolean.TRUE },
      { "elisp", "gnu.kawa.functions.DisplayFormat",
	"getEmacsLispFormat", boolClasses,
	Boolean.FALSE },
      { "readable-elisp", "gnu.kawa.functions.DisplayFormat",
	"getEmacsLispFormat", boolClasses,
	Boolean.TRUE },
      { "clisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.FALSE },
      { "readable-clisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.TRUE },
      { "commonlisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.FALSE },
      { "readable-commonlisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.TRUE },
      { "xml", "gnu.xml.XMLPrinter",
	"make", xmlPrinterClasses,
	portArg, null },
      { "html", "gnu.xml.XMLPrinter",
	"make", xmlPrinterClasses,
	portArg, "html" },
      { "xhtml", "gnu.xml.XMLPrinter",
	"make", xmlPrinterClasses,
	portArg, "xhtml" },
      { "cgi", "gnu.kawa.xml.HttpPrinter",
	"make", httpPrinterClasses,
	portArg },
      { "ignore", "gnu.lists.VoidConsumer",
	"getInstance", noClasses },
      { null }
    };

  public static String defaultFormatName;
  public static Object[] defaultFormatInfo;
  public static java.lang.reflect.Method defaultFormatMethod;

  /** Specify the default output format.
   * @param name The name of the format, as an entry in the formats table.
   */
  public static void setDefaultFormat(String name)
  {
    name = name.intern();
    defaultFormatName = name;
    for (int i = 0;  ;  i++)
      {
	Object[] info = formats[i];
	Object iname = info[0];
	if (iname == null)
	  {
	    System.err.println ("kawa: unknown output format '"+name+"'");
	    System.exit (-1);
	  }
	else if (iname == name)
	  {
	    defaultFormatInfo = info;
	    try
	      {
		Class formatClass = Class.forName((String) info[1]);
		defaultFormatMethod
		  = formatClass.getMethod((String) info[2], (Class[]) info[3]);
		
	      }
	    catch (Throwable ex)
	      {
		System.err.println("kawa:  caught "+ex+" while looking for format '"+name+"'");
		System.exit (-1);
	      }
	    break;
	  }
      }
    if (! defaultFormatInfo[1].equals("gnu.lists.VoidConsumer"))
      ModuleBody.setMainPrintValues(true);
  }

  /** Return a Consumer that formats using the appropriate format.
   * The format is chosen depending on specified defaults.
   * @param out The output where formatted output is sent to.
   */
  public static Consumer getOutputConsumer(OutPort out)
  {
    Object[] info = defaultFormatInfo;
    if (out == null)
      return VoidConsumer.getInstance();
    else if (info == null)
      return Language.getDefaultLanguage().getOutputConsumer(out);
    try
      {
	Object args[] = new Object[info.length - 4];
	System.arraycopy(info, 4, args, 0, args.length);
	for (int i = args.length;  --i >= 0; )
	  if (args[i] == portArg)
	    args[i] = out;
	Object format = defaultFormatMethod.invoke(null, args);
	if (format instanceof AbstractFormat)
	  {
	    out.objectFormat = (AbstractFormat) format;
	    return out;
	  }
	else
	  return (Consumer) format;
      }
    catch (Throwable ex)
      {
	throw new RuntimeException("cannot get output-format '"
				   + defaultFormatName + "' - caught " + ex);
      }
  }

  public static boolean dontPrompt;

  public static boolean run (Language language, Environment env)
  {
    InPort inp = InPort.inDefault ();
    SourceMessages messages = new SourceMessages();
    OutPort perr;
    if (! dontPrompt && inp instanceof TtyInPort)
      {
	Procedure prompter = language.getPrompter();
	if (prompter != null)
	  ((TtyInPort)inp).setPrompter(prompter);
        perr = OutPort.errDefault();
      }
    else
      perr = null;

    Throwable ex = run(language, env, inp, OutPort.outDefault(),
                       perr, messages);
    if (ex == null)
      return true;
    printError(ex, messages, OutPort.errDefault());
    return false;
  }

  public static Throwable run (Language language,  Environment env,
                               InPort inp, OutPort pout, OutPort perr,
                               SourceMessages messages)
  {
    Consumer out;
    AbstractFormat saveFormat = null;
    if (pout != null)
      saveFormat = pout.objectFormat;
    out = getOutputConsumer(pout);
    try
      {
	return run(language, env, inp, out, perr, null, messages);
      }
    finally
      {
	if (pout != null)
	  pout.objectFormat = saveFormat;
      }
  }

  public static boolean run (Language language,  Environment env,
                             InPort inp, Consumer out, OutPort perr,
                             java.net.URL url)
  {
    SourceMessages messages = new SourceMessages();
    Throwable ex = run(language, env, inp, out, perr, url, messages);
    if (ex != null)
      printError(ex, messages, perr);
    return ex == null;
  }

  public static Throwable run (Language language,  Environment env,
                               InPort inp, Consumer out, OutPort perr,
                               java.net.URL url, SourceMessages messages)
  {
    Language saveLanguage = Language.getDefaultLanguage();
    Lexer lexer = language.getLexer(inp, messages);
    // Wrong for the case of '-f' '-':
    //boolean interactive = inp instanceof TtyInPort;
    boolean interactive = perr != null;
    lexer.setInteractive(interactive);
    CallContext ctx = CallContext.getInstance();
    Consumer saveConsumer = null;
    if (out != null)
      {
	saveConsumer = ctx.consumer;
	ctx.consumer = out;
      }
    if (language != saveLanguage)
      Language.setDefaultLanguage(language);
    try
      {
        Thread thread = Thread.currentThread();
        ClassLoader parentLoader = thread.getContextClassLoader();
        // Create a "session" ClassLoader.  Use this for remembering classes
        // created in one command (Compilation) through further command,
        // while still allowing the classes to be replaced and collected.
        if (!(parentLoader instanceof ArrayClassLoader))
          thread.setContextClassLoader(new ArrayClassLoader(parentLoader));
      }
    catch (SecurityException ex)
      {
        // Nothing - we'll just lose some minor functionality.
      }
    try
      {
	for (;;)
	  {
	    int opts = Language.PARSE_IMMEDIATE|Language.PARSE_ONE_LINE;
	    try
	      {
		Compilation comp = language.parse(lexer, opts, null);
                boolean sawError;
                if (interactive)
                  sawError = messages.checkErrors(perr, 20);
                else if (messages.seenErrors())
                  throw new SyntaxException(messages);
                else
                  sawError = false;
		if (comp == null) // ??? end-of-file
		  break;
		if (sawError)
		  continue;
		comp.getModule().setName("atInteractiveLevel$"
					 + (++ModuleExp.interactiveCounter));

		// Skip whitespace, in case (read-char) or similar is called:
		int ch;
		for (;;)
		  {
		    ch = inp.read();
		    if (ch < 0 || ch == '\r' || ch == '\n')
		      break;
		    if (ch != ' ' && ch != '\t')
		      {
			inp.unread();
			break;
		      }
		  }

		if (! ModuleExp.evalModule(env, ctx, comp, url, perr))
		  continue;
                if (out instanceof Writer)
                  ((Writer) out).flush();
		if (ch < 0)
		  break;
	      }
	    catch (Throwable e)
	      {
		if (! interactive)
		  return e;
                printError(e, messages, perr);
	      }
	  }
      }
    finally
      {
	if (out != null)
	  ctx.consumer = saveConsumer;
        if (language != saveLanguage)
          Language.setDefaultLanguage(saveLanguage);
      }
    return null;
  }

  public static void printError (Throwable ex, SourceMessages messages,
                                 OutPort perr)
  {
    if (ex instanceof WrongArguments)
      {
        WrongArguments e = (WrongArguments) ex;
        messages.printAll(perr, 20);
        if (e.usage != null)
          perr.println("usage: "+e.usage);
        e.printStackTrace(perr);
      }
    else if (ex instanceof ClassCastException)
      {
        messages.printAll(perr, 20);
        perr.println("Invalid parameter, was: "+ ex.getMessage());
        ex.printStackTrace(perr);
      }
    /*
    else if (ex instanceof java.io.IOException)
      {
        messages.printAll(perr, 20);
        String msg = new SourceError(inp, 'e', "").toString();
        msg = msg.substring(0, msg.length() - 2);
        perr.println(msg + " (or later): caught IOException");
        ex.printStackTrace(perr);
      }
    */
    else
      {
        SyntaxException se;
        if (ex instanceof SyntaxException
            && (se = (SyntaxException) ex).getMessages() == messages)
          {
            se.printAll(perr, 20);
            se.clear();
          }
        else
          {
            messages.printAll(perr, 20);
            ex.printStackTrace(perr);
          }
      }
  }

  public static boolean runFile (String fname, int skipLines)
  {
    Environment env = Environment.getCurrent();
    try
      {
	if (fname.equals ("-"))
          {
            InPort in = InPort.inDefault();
            while (--skipLines >= 0)
              in.skipRestOfLine();
            kawa.standard.load.loadSource(in, env, null);
          }
	else
	  kawa.standard.load.apply(fname, env, false, skipLines);
        return true;
      }
    catch (gnu.text.SyntaxException e)
      {
	e.printAll(OutPort.errDefault(), 20);
        return false;
      }
    catch (FileNotFoundException e)
      {
	System.err.println("Cannot open file "+fname);
        return false;
      }
    catch (Throwable e)
      {
	e.printStackTrace(System.err);
        return false;
      }
  }
}
