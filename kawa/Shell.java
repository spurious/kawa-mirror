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
    Object obj = null;
    boolean noFatalExceptions = true;
    Environment env = new Environment (interpreter);
    do
      {
	try
	  {
            if (prompt)
	      {
		pout.print("kawa>");
		pout.flush();
	      }
            try
	      {
		obj = interpreter.read();
		if (obj!=Interpreter.eofObject)
		  {
		    interpreter.errors = 0;
		    Expression exp = interpreter.rewrite (obj);
		    /* DEBUGGING:
		      pout.print ("[Re-written expression: ");
		      exp.print (pout);
		      pout.print ("\nbefore eval<"+exp.getClass().getName()+">");
		      pout.println();
		      pout.flush();
		    */
		    if (interpreter.errors == 0)
		      {
			obj = exp.eval (env);
			if (obj == null)
			  pout.println ("[null returned]\n");
			else
			  {
			    if (obj instanceof kawa.lang.Exit)
			      obj = null;
			    else if (display && obj != Interpreter.voidObject)
			      {
				kawa.lang.print.print (obj, pout);
				pout.println();
				pout.flush();
			      }
			  }
		      }
		  }
		else if (prompt)
		  pout.println ();
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
            /* try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            }  */
         }
	catch (java.io.IOException e)
	  {
            perr.println();
            perr.println("A fatal IO exception occurred on a read.");
            noFatalExceptions = false;
	  } 
      }
    while (obj != Interpreter.eofObject && noFatalExceptions);
    return 0;
  }
}
