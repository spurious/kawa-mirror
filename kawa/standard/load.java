package kawa.standard;
import kawa.lang.*;
import java.io.*;

public class load extends Procedure1 {
  public load ()
  {
    super("load");
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof StringBuffer))
      throw new WrongType (this.name, 1, "file name");
    String name = arg1.toString ();
    FileInputStream fstream;
    try
      {
	fstream = new FileInputStream (name);
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new GenericError ("load: file not found: " + name);
      }
    InPort port = new InPort (fstream);
    Interpreter interpreter = Interpreter.current ();
    Environment env = new Environment (interpreter);
    Object last = Interpreter.voidObject;
    int i = 0;
    for (;; i++)
      {
	Object obj;
	try
	  {
	    obj = port.readSchemeObject ();
	  }
	catch (SyntaxError e)
	  {
	    throw new GenericError ("syntax error in load: " + e.toString ());
	  }
	catch (java.io.IOException e)
	  {
	    throw new GenericError ("I/O exception in load: " + e.toString ());
	  }
	if (obj == Interpreter.eofObject)
	  break;
	int save_errors = interpreter.errors;
	Expression exp;
	try
	  {
	    interpreter.errors = 0;
	    exp = interpreter.rewrite (obj);
	    if (interpreter.errors > 0)
	      throw new GenericError ("syntax error during load");
	  }
	finally
	  {
	    interpreter.errors = save_errors;
	  }
	last = exp.eval (env);
      }
    return last;
  }
}
