package kawa;

import kawa.lang.*;
import kawa.standard.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.text.SourceMessages;
import gnu.text.Lexer;

/** Utility functions (static methods) for kawa.repl.
 * Should probably be merged with kawa.repl.  FIXME. */

public class Shell
{
  public static void run (Interpreter interp)
  {
    run(interp, interp.getEnvironment());
  }

  public static void run (Interpreter interp, Environment env)
  {
    InPort inp = InPort.inDefault ();
    if (inp instanceof TtyInPort)
      {
	Object prompter = env.get("default-prompter");
	if (prompter != null && prompter instanceof Procedure)
	  ((TtyInPort)inp).setPrompter((Procedure) prompter);
      }

    run(interp, env, inp, OutPort.outDefault(), OutPort.errDefault());
  }

  public static void run (Interpreter interp,  Environment env,
			  InPort inp, OutPort pout, OutPort perr)
  {
    SourceMessages messages = new SourceMessages();
    Translator tr = new Translator(env, messages);
    Lexer lexer = interp.getLexer(inp, messages);
    for (;;)
      {
	try
	  {
	    lexer.clearErrors();
	    PairWithPosition body = new PairWithPosition(inp,
							 null, List.Empty);
	    Object sexp = ((gnu.text.LispReader) lexer).readObject(); // FIXME
	    if (sexp == Sequence.eofValue)
	      return;
	    body.car = sexp;
	    /* If the errors were minor, we could perhaps try to
	       do Translation (to check for more errors)  .  ??? */
	    ModuleExp mod = Scheme.makeModuleExp(body, tr);
	    mod.setName("atInteractiveLevel");  // FIXME
	    if (lexer.checkErrors(perr, 20))
	      continue;

	    /* DEBUGGING:
	    perr.print ("[Re-written expression: ");
	    mod.print (perr);
	    perr.print ("\nbefore eval<"+mod.getClass().getName()+">");
	    perr.println();
	    perr.flush();
	    */

	    Object result = mod.evalModule (env);
	    if (pout != null)
	      interp.print(result, pout);
	  }
	catch (WrongArguments e)
	  {
	    if (e.usage != null)
	      perr.println("usage: "+e.usage);
	    e.printStackTrace(perr);
	  }
	catch (WrongType e)
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
	catch (gnu.text.SyntaxException e)
	  {
	    e.printAll(perr, 20);
	  }
	catch (Exception e)
	  {
	    e.printStackTrace(perr);
	  }
      }
  }

  public static void runString (String str, Interpreter interp, Environment env)
  {
    run(interp, env, new CharArrayInPort(str), null, OutPort.errDefault());
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
