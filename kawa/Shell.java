package kawa;

import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

public class Shell
{
  public static void run (InPort inp, Interpreter interp,
			  boolean pflag, boolean dflag)
  {
    java.io.PrintStream pout = interp.out;
    java.io.PrintStream perr = interp.err;
    kawa.lang.Interpreter interpreter = interp;
    boolean prompt = pflag;
    boolean display = dflag;

    Environment env = Environment.user ();
    for (;;)
      {
	try
	  {
            if (prompt)
	      {
		pout.print("kawa>");
		pout.flush();
	      }

	    Object sexp = inp.readSchemeObject ();
	    if (sexp == Sequence.eofValue)
	      {
		if (prompt)
		  pout.println ();
		return;
	      }

	    interpreter.errors = 0;

	    LambdaExp lexp = new LambdaExp (ModuleBody.formals,
					    new Pair (sexp, List.Empty),
					    interpreter);
	    lexp.setName (Symbol.make ("atInteractiveLevel"));  // FIXME
	    String filename = inp.getName ();
	    if (filename == null)
	      filename = "<unknown>";
	    lexp.setFile (filename);

	    /* DEBUGGING:
	    perr.print ("[Re-written expression: ");
	    exp.print (perr);
	    perr.print ("\nbefore eval<"+exp.getClass().getName()+">");
	    perr.println();
	    perr.flush();
	    */

	    if (interpreter.errors == 0)
	      {
		Object result = lexp.eval_module (env);
		if (result == null)
		  pout.println ("[null returned]\n");
		else if (display && result != Interpreter.voidObject)
		  {
		    SFormat.print (result, pout);
		    pout.println();
		    pout.flush();
		  }
	      }
	  }
	catch (kawa.lang.WrongArguments e)
	  {
	    perr.println("Wrong arguments to procedure "+e.procname
			 +",expected "+e.number+".");
	    perr.println("usage: "+e.usage);
	    e.printStackTrace(perr);
	  }
	catch (kawa.lang.WrongType e)
	  {
	    perr.println("Argument "+e.number+" to "+e.procname
			 +" must be of type "+e.typeExpected);
	    e.printStackTrace(perr);
	  }
	catch (java.lang.ClassCastException e)
	  {
	    perr.println("Invalid parameter, should be: "+ e.getMessage());
	    e.printStackTrace(perr);
	  }
	catch (kawa.lang.ReadError e)
	  {
	    perr.println (e);
	  }
	catch (Exception e)
	  {
	    e.printStackTrace(perr);
	  }
      }
  }

  public static void runString (String str, Interpreter interp, boolean dflag)
  {
    InPort str_port = call_with_input_string.open_input_string (str);
    run (str_port, interp, false, dflag);
  }

  public static void runFile (String fname)
  {
    try
      {
	InPort iport;
	if (fname.equals ("-"))
	  iport = InPort.inDefault ();
	else
	  iport = new InPort (new FileInputStream(fname), fname);
	kawa.standard.load.loadSource (iport, Environment.user ());
      }
    catch (FileNotFoundException e)
      {
	System.err.println("Cannot open file "+fname);
	System.exit(1);
      }
    catch (Exception e)
      {
	e.printStackTrace(System.err);
	System.exit(1);
      }
  }
  
}
