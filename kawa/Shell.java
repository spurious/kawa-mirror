package kawa;

import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

public class Shell extends Procedure0
{
  Interpreter interp;
  InPort in;
  OutPort out, err;

  public Shell (Interpreter interp, InPort in, OutPort out, OutPort err)
  {
    this.interp = interp;
    this.in = in;
    this.out = out;
    this.err = err;
  }

  public Object apply0 ()
  {
    InPort saveIn = InPort.inDefault();
    OutPort saveOut = OutPort.outDefault();
    OutPort saveErr = OutPort.errDefault();
    Environment saveEnv = Environment.getCurrent();
    try
      {
	OutPort.setOutDefault(out);
	OutPort.setErrDefault(err);
	InPort.setInDefault(in);
	Environment.setCurrent(interp.getEnvironment());

	if (in instanceof TtyInPort)
	  {
	    Object prompter = interp.lookup("default-prompter");
	    if (prompter != null && prompter instanceof Procedure)
	      ((TtyInPort)in).setPrompter((Procedure) prompter);
	  }
	run(interp, in, out, err);
	return Scheme.voidObject;
      }
    finally
      {
	OutPort.setOutDefault(saveOut);
	OutPort.setErrDefault(saveErr);
	InPort.setInDefault(saveIn);
	Environment.setCurrent(saveEnv);
      }
  }

  public static void run (Interpreter interp)
  {
    InPort inp = InPort.inDefault ();
    if (inp instanceof TtyInPort)
      {
	Object prompter = interp.lookup("default-prompter");
	if (prompter != null && prompter instanceof Procedure)
	  ((TtyInPort)inp).setPrompter((Procedure) prompter);
      }

    run(interp, inp, OutPort.outDefault(), OutPort.errDefault());
  }

  public static void run (Interpreter interp,
			  InPort inp, OutPort pout, OutPort perr)
  {
    Environment env = interp.getEnvironment();
    Translator tr = new Translator (env);
    for (;;)
      {
	try
	  {
	    Object sexp = interp.read(inp);
	    if (sexp == Sequence.eofValue)
	      return;
	    tr.errors = 0;

	    String filename = inp.getName ();
	    if (filename == null)
	      filename = "<unknown>";
	    ModuleExp mod = new ModuleExp (new Pair (sexp, List.Empty),
					   tr, filename);
	    mod.setName("atInteractiveLevel");  // FIXME

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
		if (pout != null)
		  interp.print(result, pout);
	      }
	  }
	catch (kawa.lang.WrongArguments e)
	  {
	    if (e.usage != null)
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

  public static void runString (String str, Interpreter interp)
  {
    run (interp, new CharArrayInPort(str), null, OutPort.errDefault());
  }

  public static void runFile (String fname)
  {
    Environment env = Environment.user();
    try
      {
	if (fname.equals ("-"))
	  kawa.standard.load.loadSource(InPort.inDefault(), env);
	else
	  {
	    InPort fstream = InPort.openFile(fname);
	    kawa.standard.load.loadSource(fstream, env);
	    fstream.close();
	  }
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
