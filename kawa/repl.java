package kawa;
import kawa.lang.*;
import kawa.standard.*;
import java.io.*;

/** Start a "Read-Eval-Print-Loop" for the Kawa Scheme evaluator. */

class repl
{
  public static String compilationDirectory = null;
  public static String compilationTopname = null;
  public static String compilationPrefix = null;

  static void bad_option (String str)
  {
    System.err.println ("bad option " + str);
    System.exit (-1);
  }

  public static Vector commandLineArguments;

  public static String homeDirectory;

  static void checkInitFile ()
  {
    /* Set homeDirectory;  if first time called, run ~/.kawarc.scm. */
    if (homeDirectory == null)
      {
	homeDirectory = System.getProperty ("user.home");
	Object scmHomeDirectory;
	if (homeDirectory != null)
	  {
	    scmHomeDirectory = new FString (homeDirectory);
	    String file_separator = System.getProperty("file.separator");
	    String kawarc_name =
	      "/".equals(file_separator) ? ".kawarc.scm"
	      : "kawarc.scm";
	    File initFile = new File(homeDirectory, kawarc_name);
	    if (initFile.exists())
	      Shell.runFile(initFile.getPath());
	  }
	else
	  scmHomeDirectory = Scheme.falseObject;
	Environment.define_global(Symbol.make("home-directory"),
				  scmHomeDirectory);
      }
  }

  public static void setArgs (String[] args, int arg_start)
  {
    Object[] array = new Object[args.length - arg_start];
    for (int i = arg_start;  i < args.length;  i++)
      array[i - arg_start] = new FString (args[i]);
    commandLineArguments = new Vector (array);  // FIXME scsh has list
    // FIXME scsh also has command-line proc
    Environment.define_global (Symbol.make ("command-line-arguments"),
			       commandLineArguments);
  }

  public static void main(String args[])
  {
    Environment env = Scheme.makeEnvironment ();

    int iArg = 0;
    boolean something_done = false;
    for ( ;  iArg < args.length;  iArg++)
      {
	String arg = args[iArg];
	if (arg.equals ("-c") || arg.equals ("-e"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    setArgs (args, iArg+1);
	    if (arg.equals ("-c"))
	      checkInitFile();
	    Shell.runString (args[iArg], env);
	    something_done = true;
	  }
	else if (arg.equals ("-f"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    setArgs (args, iArg+1);
	    checkInitFile();
	    Shell.runFile (args[iArg]);
	    something_done = true;
	  }
	else if (arg.equals ("-s") || arg.equals ("--"))
	  {
	    iArg++;
	    setArgs (args, iArg);
	    checkInitFile();
	    Shell.run(env);
	    return;
	  }
	else if (args[iArg].equals ("-d"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    compilationDirectory = args[iArg];
	  }
	else if (args[iArg].equals ("-P"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    compilationPrefix = args[iArg];
	  }
	else if (args[iArg].equals ("-T"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    compilationTopname = args[iArg];
	  }
	else if (args[iArg].equals ("-C"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    try
	      {
		if (CompileFile.compile_to_files(args[iArg],
						 compilationDirectory,
						 compilationPrefix,
						 compilationTopname))
		  System.exit(-1);
	      }
	    catch (GenericError ex)
	      {
		System.err.println(ex.getMessage ());
		System.exit(-1);
	      }
	    something_done = true;
	  }
	else if (arg.length () > 0 && arg.charAt(0) == '-')
	  bad_option (arg);
	else
	  break;
      }
    if (something_done)
      return;

    if (iArg < args.length)
      {
	setArgs (args, iArg+1);
	checkInitFile();
	Shell.runFile (args[iArg]);
      }
    else
      {
	setArgs (args, iArg);
	checkInitFile();
	Shell.run(env);
      }
   }
}
