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
    SourceMessages messages = new SourceMessages();
    Lexer lexer = interp.getLexer(inp, messages);
    CallContext ctx = new CallContext();
    FormatToConsumer saveFormat = null;
    if (pout == null)
      ctx.consumer = new VoidConsumer();
    else
      {
	saveFormat = pout.objectFormat;
	ctx.consumer = interp.getOutputConsumer(pout);
      }
    try
      {
	for (;;)
	  {
	    try
	      {
		ModuleExp mod = interp.parse(env, lexer);
		if (mod == null) // end-of-file
		  break;
		mod.setName("atInteractiveLevel");  // FIXME
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


		mod.evalModule(env, ctx);
		ctx.run();
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
	    catch (Exception e)
	      {
		e.printStackTrace(perr);
	      }
	  }
      }
    finally
      {
	if (pout != null)
	  pout.objectFormat = saveFormat;
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
    Environment env = Environment.user();
    try
      {
	if (fname.equals ("-"))
	  kawa.standard.load.loadSource(InPort.inDefault(), env);
	else
	  kawa.standard.load.apply(fname,env);
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
    catch (Exception e)
      {
	e.printStackTrace(System.err);
	System.exit(1);
      }
  }
  
}
