package kawa;

import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

public class Shell
{
  protected java.io.PrintStream pout;
  protected java.io.PrintStream perr;
  kawa.lang.Interpreter interpreter;
  boolean prompt;
  boolean display;

  public Shell (Interpreter interp, boolean pflag, boolean dflag)
  {
    pout = interp.out;
    perr = interp.err;
    interpreter = interp;
    prompt = pflag;
    display = dflag;
  }

  public int run()
  {
    Environment env = new Environment (interpreter);
    for (;;)
      {
	try
	  {
            if (prompt)
	      {
		pout.print("kawa>");
		pout.flush();
	      }

	    Object sexp = interpreter.read();
	    if (sexp==Interpreter.eofObject)
	      {
		if (prompt)
		  pout.println ();
		return 0;
	      }

	    interpreter.errors = 0;
	    Expression exp = interpreter.rewrite (sexp);

	    /* DEBUGGING:
	    perr.print ("[Re-written expression: ");
	    exp.print (perr);
	    perr.print ("\nbefore eval<"+exp.getClass().getName()+">");
	    perr.println();
	    perr.flush();
	    */

	    if (interpreter.errors == 0)
	      {
		Object result = exp.eval (env);
		if (result == null)
		  pout.println ("[null returned]\n");
		else if (display && result != Interpreter.voidObject)
		  {
		    kawa.lang.print.print (result, pout);
		    pout.println();
		    pout.flush();
		  }
	      }
	  }
	catch (kawa.lang.WrongArguments e)
	  {
	    perr.println();
	    perr.println("Wrong arguments to procedure "+e.procname
			 +",expected "+e.number+".");
	    perr.println("usage: "+e.usage);
	  }
	catch (kawa.lang.WrongType e)
	  {
	    perr.println();
	    perr.println("Argument "+e.number+" to "+e.procname
			 +" must be of type "+e.typeExpected);
	  }
	catch (kawa.lang.GenericError e)
	  {
	    perr.println();
	    perr.println(e.message);
	  }
	catch (java.lang.ClassCastException e)
	  {
	    perr.println();
	    perr.println("Invalid parameter, should be: "+ e.getMessage());
	    e.printStackTrace(perr);
	  }
	catch (kawa.lang.SyntaxError e)
	  {
	    perr.println();
	    perr.println(e);
	  }
	catch (kawa.lang.UnboundSymbol e)
	  {
	    perr.println();
	    perr.println("Unbound symbol "+e.symbol+" in execution.");
	  }
	catch (java.io.IOException e)
	  {
	    perr.println();
	    perr.println("A fatal IO exception occurred on a read.");
	  }
      }
  }
}
