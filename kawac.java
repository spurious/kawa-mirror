import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

class kawac
{

  static void usage ()
  {
    System.err.println
      ("usage:  kawac infile [-d outdirectory] [prefix [topname]]");
    System.exit (-1);
  }

  public static void main(java.lang.String args[])
  {
    if (args.length == 0)
      usage ();
    String infile = args[0];
    String directory = null;
    String prefix = null;
    String topname = null;

    for (int iArg = 1;  iArg < args.length;  iArg++)
      {
	if (args[iArg].equals ("-d"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      usage ();
	    directory = args[iArg];
	  }
	else if (prefix == null)
	  {
	    prefix = args[iArg];
	  }
	else if (topname == null)
	  {
	    topname = args[iArg];
	  }
	else
	  usage ();
      }

    kawa.standard.StandardInterpreter interpreter =
      new kawa.standard.StandardInterpreter(InPort.inDefault (),
					    OutPort.outDefault (),
					    OutPort.errDefault ()
         );
    
    try
      {
	CompileFile.compile_to_files (infile, directory, prefix, topname);
	if (interpreter.errors > 0)
	  System.exit (-1);
      }
    catch (GenericError ex)
      {
	System.err.println(ex.message);
      }
   }
}
