package kawa;

import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

public class Shell
{
  public static void run (Environment env)
  {
    InPort inp = InPort.inDefault ();
    if (inp instanceof TtyInPort)
      {
	Object prompter = env.get (Symbol.make ("default-prompter"));
	if (prompter != null && prompter instanceof Procedure)
	  ((TtyInPort)inp).setPrompter((Procedure) prompter);
      }

    run(env, inp, OutPort.outDefault());
  }

  public static void run (Environment env, InPort inp, OutPort pout)
  {
    Translator tr = new Translator (env);
    OutPort perr = OutPort.errDefault();
    for (;;)
      {
	try
	  {
	    Object sexp = inp.readSchemeObject ();
	    if (sexp == Sequence.eofValue)
	      return;

	    tr.errors = 0;

	    String filename = inp.getName ();
	    if (filename == null)
	      filename = "<unknown>";
	    ModuleExp mod = new ModuleExp (new Pair (sexp, List.Empty),
					   tr, filename);
	    mod.setName (Symbol.make ("atInteractiveLevel"));  // FIXME

	    /* DEBUGGING:
	    perr.print ("[Re-written expression: ");
	    mod.print (perr);
	    perr.print ("\nbefore eval<"+mod.getClass().getName()+">");
	    perr.println();
	    perr.flush();
	    */

	    if (tr.errors == 0)
	      {
		Object result = mod.eval_module (env);
		if (pout != null && result != Scheme.voidObject)
		  {
		    if (result instanceof Values)
		      {
			Object[] results = ((Values) result).values();
			for (int i = 0;  i < results.length;  i++)
			  {
			    SFormat.print (results[i], pout);
			    pout.println();
			  }
		      }
		    else
		      {
			SFormat.print (result, pout);
			pout.println();
		      }
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

  public static void runString (String str, Environment env)
  {
    InPort str_port = call_with_input_string.open_input_string (str);
    run (env, str_port, null);
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
