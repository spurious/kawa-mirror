package kawa;
import kawa.lang.CompileFile;
import java.io.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;
import java.util.Vector;

/** Start a "Read-Eval-Print-Loop" for the Kawa Scheme evaluator. */

public class repl extends Procedure0or1
{
  public static String compilationDirectory = null;
  public static String compilationTopname = null;
  public static String compilationPrefix = null;

  Interpreter interp;

  public repl(Interpreter interp)
  {
    this.interp = interp;
  }

  public Object apply0 ()
  {
    Shell.run(interp, Environment.getCurrent());
    return Values.empty;
  }

  public Object apply1(Object env)
  {
    Shell.run(interp, (Environment) env);
    return Values.empty;
  }

  static void bad_option (String str)
  {
    System.err.println ("kawa: bad option '" + str + "'");
    printOptions(System.err);
    System.exit (-1);
  }

  public static void printOption(PrintStream out, String option, String doc)
  {
    out.print(" ");
    out.print(option);

    int len = option.length() + 1;
    for (int i = 0; i < 30 - len; ++i)
      out.print(" ");
    out.print(" ");
    out.println(doc);
  }
  
  public static void printOptions(PrintStream out)
  {
    out.println("Usage: [java kawa.repl | kawa] [options ...]");
    out.println();
    out.println(" Generic options:");
    printOption(out, "--help", "Show help about options");
    printOption(out, "--author", "Show author information");
    printOption(out, "--version", "Show version information");
    out.println();
    out.println(" Options");
    printOption(out, "-e <expr>", "Evaluate expression <expr>");
    printOption(out, "-c <expr>", "Same as -e, but make sure ~/.kawarc.scm is run first");
    printOption(out, "-f <filename>", "File to interpret");
    printOption(out, "-s| --", "Start reading commands interactively from console");
    printOption(out, "-w", "Launch the interpreter in a GUI window");
    printOption(out, "--server <port>", "Start a server accepting telnet connections on <port>");
    printOption(out, "--debug-dump-zip", "Compiled interactive expressions to a zip archive");
    printOption(out, "--debug-print-expr", "Print generated internal expressions");
    printOption(out, "--debug-print-final-expr", "Print expression after any optimizations");
    printOption(out,"--[no-]full-tailcalls", "(Don't) use full tail-calls");
    printOption(out, "-C <filename> ...", "Compile named files to Java class files");
    printOption(out, "--output-format <format>", "Use <format> when printing top-level output");
    printOption(out,"--<language>", "Select source language, one of:");
    String[][] languages = Interpreter.getLanguages();
    for (int i = 0; i < languages.length; i++)
      {
	out.print("   ");
	String[] lang = languages[i];
	// skip last entry, which is class name
	int nwords = lang.length - 1;
	for (int j = 0; j < nwords; j++) 
	  out.print(lang[j] + " ");
	if (i == 0)
	  out.print("[default]");
	out.println();
      }
    out.println(" Compilation options, must be specified before -C");
    printOption(out, "-d <dirname>", "Directory to place .class files in");
    printOption(out, "-P <prefix>", "Prefix to prepand to class names");
    printOption(out, "-T <topname>", "name to give to top-level class");
    
    printOption(out, "--main", "Generate an application, with a main method");
    printOption(out, "--applet", "Generate an applet");
    printOption(out, "--servlet", "Generate a servlet");
    printOption(out, "--module-static", "Top-leval definitions are by default static");

    Vector keys = Compilation.options.keys();
    for (int i = 0; i < keys.size(); ++i)
      {
        String name = (String) keys.get(i);
        printOption(out, "--" + name, Compilation.options.getDoc(name));
      }
        
    out.println();
    out.println("For more information go to:  http://www.gnu.org/software/kawa/");
  }

  /** Number of times exitDecrement calls before we exit. */
  private static int exitCounter;
  /** See exitDecrement. */
  public static synchronized void exitIncrement()
  {
    if (exitCounter == 0)
      exitCounter++;
    exitCounter++;
  }

  /** Work around an AWT bug, where AWT threads are non-daemon.
   * Thus if you start up AWT, the JVM will wait for the AWT to finish,
   * even if there are no other non-daemon threads.
   * So call exitIncrement() each time a Freme is created,
   * and call exitDecrement() a Frame is closed. */
  public static synchronized void exitDecrement()
  {
    int counter = exitCounter;
    if (counter > 0)
      {
	counter--;
	if (counter == 0)
	  {
	    System.exit(0);
	  }
	else
	  exitCounter = counter;
      }
  }

  public static String[] commandLineArgArray;
  public static FVector commandLineArguments;

  public static String homeDirectory;

  static void checkInitFile ()
  {
    /* Set homeDirectory;  if first time called, run ~/.kawarc.scm. */
    if (homeDirectory == null)
      {
	File initFile = null;
	homeDirectory = System.getProperty ("user.home");
	Object scmHomeDirectory;
	if (homeDirectory != null)
	  {
	    scmHomeDirectory = new FString (homeDirectory);
	    String file_separator = System.getProperty("file.separator");
	    String kawarc_name =
	      "/".equals(file_separator) ? ".kawarc.scm"
	      : "kawarc.scm";
	    initFile = new File(homeDirectory, kawarc_name);
	  }
	else
	  scmHomeDirectory = Boolean.FALSE;
	Environment.getCurrent().put("home-directory", scmHomeDirectory);
	if (initFile != null && initFile.exists())
	  Shell.runFile(initFile.getPath());
      }
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

  public static Interpreter getInterpreterFromFilenameExtension(String name)
  {
    if (Interpreter.defaultInterpreter == null)
      {
	Interpreter interp
	  = Interpreter.getInstanceFromFilenameExtension(name);
	if (interp != null)
	  {
	    Interpreter.defaultInterpreter = interp;
	    Environment.setCurrent(interp.getEnvironment());
	    return interp;
	  }
      }
    return getInterpreter();
  }

  public static Interpreter getInterpreter()
  {
    Interpreter interpreter = Interpreter.defaultInterpreter;
    if (interpreter == null)
      {
	interpreter = Interpreter.getInstance(null);
	Interpreter.defaultInterpreter = interpreter;
	Environment.setCurrent(interpreter.getEnvironment());
      }
    return interpreter;
  }

  static boolean shutdownRegistered
    = gnu.text.WriterManager.instance.registerShutdownHook();

  public static int processArgs(String[] args, int iArg, int maxArg)
  {
    boolean something_done = false;
    for ( ;  iArg < maxArg;  iArg++)
      {
	String arg = args[iArg];
	if (arg.equals ("-c") || arg.equals ("-e"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    getInterpreter();
	    setArgs (args, iArg+1);
	    if (arg.equals ("-c"))
	      checkInitFile();
	    Interpreter interp = Interpreter.defaultInterpreter;
	    Shell.runString(args[iArg], interp, interp.getEnvironment());
	    something_done = true;
	  }
	else if (arg.equals ("-f"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    String filename = args[iArg];
	    getInterpreterFromFilenameExtension(filename);
	    setArgs (args, iArg+1);
	    checkInitFile();
	    Shell.runFile (filename);
	    something_done = true;
	  }
	else if (arg.equals("\\"))
	  {
	    // Scsh-like "meta-arg".  See Kawa manual (SOON-FIXME).
	    if (++iArg == maxArg)
	      bad_option (arg);
	    String filename = args[iArg];
	    InPort freader;
	    try
	      {
		InputStream fstream = new BufferedInputStream(new FileInputStream(filename));
		int ch = fstream.read();
		if (ch == '#')
		  {
		    StringBuffer sbuf = new StringBuffer(100);
		    Vector xargs = new Vector(10);
		    int state = 0;
		    while (ch != '\n' && ch != '\r' && ch >= 0)
		      ch = fstream.read();
		    for (;;)
		      {
			ch = fstream.read();
			if (ch < 0)
			  {
			    System.err.println("unexpected end-of-file processing argument line for: '" + filename + '\'');
			    System.exit(-1);
			  }
			if (state == 0)
			  {
			    if (ch == '\\' || ch == '\'' || ch == '\"')
			      {
				state = ch;
				continue;
			      }
			    else if (ch == '\n' || ch == '\r')
			      break;
			    else if (ch == ' ' || ch == '\t')
			      {
				if (sbuf.length() > 0)
				  {
				    xargs.addElement(sbuf.toString());
				    sbuf.setLength(0);
				  }
				continue;
			      }
			  }
			else if (state == '\\')
			  state = 0;
			else if (ch == state)
			  {
			    state = 0;
			    continue;
			  }
			sbuf.append((char) ch);
		      }
		    if (sbuf.length() > 0)
		      xargs.addElement(sbuf.toString());
		    int nxargs = xargs.size();
		    if (nxargs > 0)
		      {
			String[] sargs = new String[nxargs];
			xargs.copyInto(sargs);
			int ixarg = processArgs(sargs, 0, nxargs);
			if (ixarg >= 0 && ixarg < nxargs)
			  { // FIXME
			    System.err.println(""+(nxargs-ixarg)+" unused meta args");
			  }
		      }
		  }
		getInterpreterFromFilenameExtension(filename);
		freader = InPort.openFile(fstream, filename);
		// FIXME adjust line number
		setArgs(args, iArg+1);
		checkInitFile();
		kawa.standard.load.loadSource(freader, Environment.user());
		return -1;
	      }
	    catch (gnu.text.SyntaxException ex)
	      {
		ex.printAll(OutPort.errDefault(), 20);
	      }
	    catch (java.io.FileNotFoundException ex)
	      {
		System.err.println("Cannot open file "+filename);
		System.exit(1);
	      }
	    catch (Throwable ex)
	      {
		ex.printStackTrace(System.err);
		System.exit(1);
	      }
	    return -1;
	  }
	else if (arg.equals ("-s") || arg.equals ("--"))
	  {
	    iArg++;
	    getInterpreter();
	    setArgs (args, iArg);
	    checkInitFile();
	    Shell.run(Interpreter.defaultInterpreter, Environment.getCurrent());
	    return -1;
	  }
	else if (arg.equals ("-w"))
	  {
	    iArg++;
	    getInterpreter();
	    setArgs (args, iArg);
	    checkInitFile();
	    // Do this instead of just new GuiConsole in case we have
	    // configured --without-awt.
	    try
	      {
		Class.forName("kawa.GuiConsole").newInstance();
	      }
	    catch (Exception ex)
	      {
		System.err.println("failed to create Kawa window: "+ex);
		System.exit (-1);
	      }
	    something_done = true;
	  }
	else if (arg.equals ("-d"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    compilationDirectory = args[iArg];
	  }
	else if (arg.equals ("-P"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    compilationPrefix = args[iArg];
	  }
	else if (arg.equals ("-T"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    compilationTopname = args[iArg];
	  }
	else if (arg.equals ("-C"))
	  {
	    ++iArg;
	    if (iArg == maxArg)
	      bad_option (arg);
	    for ( ; iArg < maxArg;  iArg++)
	      {
		arg = args[iArg];
		getInterpreterFromFilenameExtension(arg);
		try
		  {
		    System.err.println("(compiling "+arg+")");
		    SourceMessages messages = new SourceMessages();

		    CompileFile.compile_to_files(arg,
						 compilationDirectory,
						 compilationPrefix,
						 compilationTopname,
						 messages);
		    boolean sawErrors = messages.seenErrors();
		    messages.checkErrors(System.err, 50);
		    if (sawErrors)
		      System.exit(-1);
		  }
		catch (Throwable ex)
		  {
		    System.err.println("Internal error while compiling "+arg);
		    ex.printStackTrace(System.err);
		    System.exit(-1);
		  }
	      }
	    return -1;
	  }
	else if (arg.equals("--output-format")
		 || arg.equals("--format"))
	  {
	    if (++iArg == maxArg)
	      bad_option (arg);
	    Shell.setDefaultFormat(args[iArg]);
	  }
	else if (arg.equals("--connect"))
	  {
	    ++iArg;
	    if (iArg == maxArg)
	      bad_option (arg);
	    int port;
	    if (args[iArg].equals("-"))
	      port = 0;
	    else
	      {
		try
		  {
		    port = Integer.parseInt(args[iArg]);
		  }
		catch (NumberFormatException ex)
		  {
		    bad_option ("--connect port#");
		    port = -1; // never seen.
		  }
	      }
	    try
	      {
		java.net.Socket socket = new java.net.Socket("localhost",port);
		Telnet conn = new Telnet(socket, true);
		java.io.InputStream sin = conn.getInputStream();
		java.io.OutputStream sout = conn.getOutputStream();
		java.io.PrintStream pout = new PrintStream (sout, true);
		System.setIn(sin);
		System.setOut(pout);
		System.setErr(pout);
	      }
	    catch (java.io.IOException ex)
	      {
		ex.printStackTrace(System.err);
		throw new Error(ex.toString());
	      }
	  }
	else if (arg.equals("--server"))
	  {
	    getInterpreter();
	    ++iArg;
	    if (iArg == maxArg)
	      bad_option (arg);
	    int port;
	    if (args[iArg].equals("-"))
	      port = 0;
	    else
	      {
		try
		  {
		    port = Integer.parseInt(args[iArg]);
		  }
		catch (NumberFormatException ex)
		  {
		    bad_option ("--server port#");
		    port = -1; // never seen.
		  }
	      }
	    try
	      {
		java.net.ServerSocket ssocket
		  = new java.net.ServerSocket(port);
		port = ssocket.getLocalPort();
		System.err.println("Listening on port "+port);
		for (;;)
		  {
		    System.err.print("waiting ... ");  System.err.flush();
		    java.net.Socket client = ssocket.accept();
		    System.err.println("got connection from "
				       +client.getInetAddress()
				       +" port:"+client.getPort());
		    TelnetRepl.serve(Interpreter.defaultInterpreter, client);
		  }
	      }
	    catch (java.io.IOException ex)
	      {
		throw new Error(ex.toString());
	      }
	  }
	else if (arg.equals("--main"))
	  {
	    Compilation.generateMainDefault = true;
	  }
	else if (arg.equals("--applet"))
	  {
	    Compilation.generateAppletDefault = true;
	  }
	else if (arg.equals("--servlet"))
	  {
	    Compilation.generateServletDefault = true;
	  }
	else if (arg.equals("--debug-dump-zip"))
	  {
	    gnu.expr.ModuleExp.dumpZipPrefix = "kawa-zip-dump-";
	  }
	else if (arg.equals("--debug-print-expr"))
	  {
	    gnu.expr.ModuleExp.debugPrintExpr = true;
	  }
	else if (arg.equals("--debug-print-final-expr"))
	  {
	    Compilation.debugPrintFinalExpr = true;
	  }
	else if (arg.equals("--module-static"))
	  {
	    gnu.expr.Compilation.moduleStatic = 1;
	  }
	else if (arg.equals("--fewer-classes"))
	  {
	    gnu.expr.Compilation.fewerClasses = true;
	  }
	else if (arg.equals("--no-inline")
		 || arg.equals("--inline=none"))
	  {
	    gnu.expr.Compilation.inlineOk = false;
	  }
	else if (arg.equals("--inline"))
	  {
	    gnu.expr.Compilation.inlineOk = true;
	  }
	else if (arg.equals("--cps"))
	  {
	    Compilation.fewerClasses = true;
	    Compilation.defaultCallConvention
	      = Compilation.CALL_WITH_CONTINUATIONS;
	  }
	else if (arg.equals("--full-tailcalls"))
	  {
	    Compilation.defaultCallConvention
	      = Compilation.CALL_WITH_TAILCALLS;
	  }
	else if (arg.equals("--no-full-tailcalls"))
	  {
	    Compilation.defaultCallConvention
	      = Compilation.CALL_WITH_RETURN;
	  }
	else if (arg.equals("--help"))
	  {
	    printOptions(System.out);
	    System.exit(0);
	  }
	else if (arg.equals("--author"))
	  {
	    System.out.println("Per Bothner <per@bothner.com>");
	    System.exit(0);
	  }
	else if (arg.equals("--version"))
	  {
	    System.out.print("Kawa ");
	    System.out.print(Version.getVersion());
	    System.out.println();
	    System.out.println("Copyright (C) 2004 Per Bothner");
	    something_done = true;
	  }
	else if (arg.length () > 0 && arg.charAt(0) == '-')
	  { // Check if arg is a known language name.
	    Interpreter previous = Interpreter.defaultInterpreter;
	    String name = arg;
	    if (name.length() > 2 && name.charAt(0) == '-')
	      name = name.substring(name.charAt(1) == '-' ? 2 :1);
	    Interpreter interpreter = Interpreter.getInstance(name);
	    if (interpreter != null)
	      {
		Interpreter.defaultInterpreter = interpreter;
		if (previous == null)
		  Environment.setCurrent(interpreter.getEnvironment());
	      }
	    else
	      {
		// See if arg is a valid Compilation option, and if so set it.
		int eq = name.indexOf("=");
		String opt_value;
		if (eq < 0)
		  opt_value = null;
		else
		  {
		    opt_value = name.substring(eq+1);
		    name = name.substring(0, eq);
		  }

		// Convert "--no-xxx" to "--xxx=no":
		boolean startsWithNo
		  = name.startsWith("no-") && name.length() > 3;
		if (opt_value == null && startsWithNo)
		  {
		    opt_value = "no";
		    name = name.substring(3);
		  }

		String msg = Compilation.options.set(name, opt_value);
		if (msg != null)
		  {
		    // It wasn't a valid Complation option.
		    if (startsWithNo && msg == gnu.text.Options.UNKNOWN)
		      msg = "both '--no-' prefix and '="+
			opt_value+"' specified";
		    if (msg == gnu.text.Options.UNKNOWN)
		      {
			bad_option(arg);
		      }
		    else
		      {
			System.err.println ("kawa: bad option '"
					    + arg + "': " + msg);
			System.exit (-1);
		      }
		  }
	      }
	  }
	else
	  return iArg;
      }
    return something_done ? -1 : iArg;
  }

  public static void main(String args[])
  {
    try
      {
	int iArg = processArgs(args, 0, args.length);
	if (iArg < 0)
	  return;
	if (iArg < args.length)
	  {
	    String filename = args[iArg];
	    getInterpreterFromFilenameExtension(filename);
	    setArgs (args, iArg+1);
	    checkInitFile();
	    Shell.runFile (filename);
	  }
	else
	  {
	    getInterpreter();
	    setArgs (args, iArg);
	    checkInitFile();
	    Shell.run(Interpreter.defaultInterpreter);
	  }
      }
    finally
      {
	if (! shutdownRegistered)
	  {
	    // Redundant if registerShutdownHook succeeded (e.g on JDK 1.3).
	    gnu.mapping.OutPort.runCleanups();
	  }
	exitDecrement();
      }
   }
}
