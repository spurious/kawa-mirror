package kawa;

import kawa.lang.*;
import gnu.kawa.lispexpr.LispReader;
import kawa.standard.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.text.*;
import gnu.lists.*;

/** Utility functions (static methods) for kawa.repl.
 * Should probably be merged with kawa.repl.  FIXME. */

public class Shell
{
  private static Class[] noClasses = { };
  private static  Class[] boolClasses = { Boolean.TYPE };
  private static  Class[] xmlPrinterClasses
    = {gnu.lists.Consumer.class,  java.lang.Object.class };
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
      return Interpreter.getInterpreter().getOutputConsumer(out);
    try
      {
	Object args[] = new Object[info.length - 4];
	System.arraycopy(info, 4, args, 0, args.length);
	for (int i = args.length;  --i >= 0; )
	  if (args[i] == portArg)
	    args[i] = out;
	Object format = defaultFormatMethod.invoke(null, args);
	if (format instanceof FormatToConsumer)
	  {
	    out.objectFormat = (FormatToConsumer) format;
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

  public static void run (Interpreter interp)
  {
    run(interp, interp.getEnvironment());
  }

  public static void run (Interpreter interp, Environment env)
  {
    InPort inp = InPort.inDefault ();
    if (inp instanceof TtyInPort)
      {
	Procedure prompter = interp.getPrompter();
	if (prompter != null)
	  ((TtyInPort)inp).setPrompter(prompter);
      }

    run(interp, env, inp, OutPort.outDefault(), OutPort.errDefault());
  }

  public static void run (Interpreter interp,  Environment env,
			  InPort inp, OutPort pout, OutPort perr)
  {
    Consumer out;
    FormatToConsumer saveFormat = null;
    if (pout != null)
      saveFormat = pout.objectFormat;
    out = getOutputConsumer(pout);
    try
      {
	run(interp, env, inp, out, perr);
      }
    finally
      {
	if (pout != null)
	  pout.objectFormat = saveFormat;
      }
  }

  public static void run (Interpreter interp,  Environment env,
			  InPort inp, Consumer out, OutPort perr)
  {
    SourceMessages messages = new SourceMessages();
    Lexer lexer = interp.getLexer(inp, messages);
    // Wrong for the case of '-f' '-':
    boolean interactive = inp instanceof TtyInPort;
    lexer.setInteractive(interactive);
    CallContext ctx = CallContext.getInstance();
    Consumer saveConsumer = null;
    if (out != null)
      {
	saveConsumer = ctx.consumer;
	ctx.consumer = out;
      }
    try
      {
	for (;;)
	  {
	    try
	      {
		Compilation comp = interp.parse(env, lexer);
		if (comp == null) // end-of-file
		  break;
		comp.getModule().setName("atInteractiveLevel");  // FIXME
		if (lexer.checkErrors(perr, 20))
		  continue;

		// Skip whitespace, in case somebody calls (read-char) or similar.
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


		ModuleExp.evalModule(env, ctx, comp);
		if (messages.checkErrors(perr, 20))
		  continue;
		ctx.runUntilDone();
		if (ch < 0)
		  break;
	      }
	    catch (WrongArguments e)
	      {
		if (e.usage != null)
		  perr.println("usage: "+e.usage);
		e.printStackTrace(perr);
	      }
	    catch (java.lang.ClassCastException e)
	      {
		perr.println("Invalid parameter, was: "+ e.getMessage());
		e.printStackTrace(perr);
	      }
	    catch (gnu.text.SyntaxException e)
	      {
		e.printAll(perr, 20);
		e.clear();
	      }
	    catch (java.io.IOException e)
	      {
		messages.printAll(perr, 20);
		String msg = new SourceError(inp, 'e', "").toString();
		msg = msg.substring(0, msg.length() - 2);
		perr.println(msg + " (or later): caught IOException");
		e.printStackTrace(perr);
		if (! interactive)
		  return;
	      }
	    catch (Throwable e)
	      {
		e.printStackTrace(perr);
		if (! interactive)
		  return;
	      }
	  }
      }
    finally
      {
	if (out != null)
	  ctx.consumer = saveConsumer;
      }
  }

  public static void runString (String str, Interpreter interp, Environment env)
  {
    run(interp, env, new CharArrayInPort(str),
	ModuleBody.getMainPrintValues() ? OutPort.outDefault() : null,
	OutPort.errDefault());
  }

  public static void runFile (String fname)
  {
    Environment env = Environment.getCurrent();
    try
      {
	if (fname.equals ("-"))
	  kawa.standard.load.loadSource(InPort.inDefault(), env);
	else
	  kawa.standard.load.apply(fname, env, false);
      }
    catch (gnu.text.SyntaxException e)
      {
	e.printAll(OutPort.errDefault(), 20);
      }
    catch (FileNotFoundException e)
      {
	System.err.println("Cannot open file "+fname);
	System.exit(1);
      }
    catch (Throwable e)
      {
	e.printStackTrace(System.err);
	System.exit(1);
      }
  }
  
}
