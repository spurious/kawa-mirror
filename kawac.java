import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

class kawac
{

  static void usage ()
  {
    System.err.println
      ("usage: [java] kawac infile [-d outdirectory] [prefix [topname]]");
    System.exit (-1);
  }

  public static void main(java.lang.String args[])
  {
    String infile = null;
    String directory = null;
    String prefix = null;
    String topname = null;

    for (int iArg = 0;  iArg < args.length;  iArg++)
      {
	if (args[iArg].equals ("-d"))
	  {
	    iArg++;
	    if (iArg == args.length)
	      usage ();
	    directory = args[iArg];
	  }
	else if (infile == null)
	  infile = args[iArg];
	else if (prefix == null)
	  prefix = args[iArg];
	else if (topname == null)
	  topname = args[iArg];
	else
	  usage ();
      }

    if (infile == null)
      usage ();

    kawa.standard.Scheme interpreter =
      new kawa.standard.Scheme(InPort.inDefault (),
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
	System.err.println(ex.getMessage ());
      }
   }
}
