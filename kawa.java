import kawa.lang.*;
import kawa.standard.*;
import kawa.Shell;
import java.io.*;

class kawa
{
  static void bad_option (String str)
  {
    System.err.println ("bad option " + str);
    System.exit (-1);
  }

  public static Vector commandLineArguments;

  public static void setArgs (String[] args, int arg_start)
  {
    Object[] array = new Object[args.length - arg_start];
    for (int i = arg_start;  i < args.length;  i++)
      array[i - arg_start] = new StringBuffer (args[i]);
    commandLineArguments = new Vector (array);
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
	    Shell.runString (args[iArg], env, false);
	    something_done = true;
	  }
	else if (arg.equals ("-f"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      bad_option (arg);
	    setArgs (args, iArg+1);
	    Shell.runFile (args[iArg]);
	    something_done = true;
	  }
	else if (arg.equals ("-s") || arg.equals ("--"))
	  {
	    iArg++;
	    setArgs (args, iArg);
	    Shell.run (InPort.inDefault (), env, true, true);
	    return;
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
	Shell.runFile (args[iArg]);
      }
    else
      {
	setArgs (args, iArg);
	Shell.run (InPort.inDefault (), env, true, true);
      }
   }
}
