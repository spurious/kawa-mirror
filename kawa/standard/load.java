package kawa.standard;
import kawa.lang.*;
import java.io.*;
import codegen.ZipArchive;
import codegen.ZipLoader;

public class load extends Procedure1 {
  public load ()
  {
    super("load");
  }

  public final static Object loadClassFile (String name)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	Class clas = Class.forName (name);
	Object inst = clas.newInstance ();
	if (! (inst instanceof Procedure0))
	  throw new GenericError ("load - class is not an Procedure0");
	Procedure0 proc = (Procedure0) inst;
	return proc.apply0 ();
      }
    catch (ClassNotFoundException ex)
      {
	throw new GenericError ("class not found in load");
      }
    catch (InstantiationException ex)
      {
	throw new GenericError ("class not instantiable: in load");
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError ("class illegal access: in load");
      }
  }

  public final static Object loadCompiled (String name)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	File zfile = new File (name);
	if (!zfile.exists ())
	  throw new GenericError ("load: "+name+" - not found");
	if (!zfile.canRead ())
	  throw new GenericError ("load: "+name+" - not readable");
	ZipArchive zar = new ZipArchive (name, "r");
	ZipLoader loader = new ZipLoader (zar);
	Class clas = loader.loadClass ("Top", true);
	Object inst = clas.newInstance ();
	Procedure0 proc = (Procedure0) inst;
	return proc.apply0 ();
      }
    catch (java.io.IOException ex)
      {
	throw new GenericError ("load: "+name+" - "+ex.toString ());
      }
    catch (ClassNotFoundException ex)
      {
	throw new GenericError ("class not found in load");
      }
    catch (InstantiationException ex)
      {
	throw new GenericError ("class not instantiable: in load");
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError ("class illegal access: in load");
      }
  }

  public final static Object loadSource (String name)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    FileInputStream fstream;
    try
      {
	fstream = new FileInputStream (name);
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new GenericError ("load: file not found: " + name);
      }
    return loadSource (new InPort (fstream, name));
  }

  public final static Object loadSource (InPort port)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Interpreter interpreter = Interpreter.current ();
    Environment env = new Environment (interpreter);
    Object last = Interpreter.voidObject;
    for (;;)
      {
	Object obj;
	try
	  {
	    obj = port.readSchemeObject ();
	    if (obj == Interpreter.eofObject)
	      {
		port.close ();
		break;
	      }
	  }
	catch (ReadError e)
	  {
	    // The '\n' is because a ReadError includes a line number,
	    // and it is better if that starts the line.
	    throw new GenericError ("read error in load:\n" + e.toString ());
	  }
	catch (SyntaxError e)
	  {
	    throw new GenericError ("syntax error in load: " + e.toString ());
	  }
	catch (java.io.IOException e)
	  {
	    throw new GenericError ("I/O exception in load: " + e.toString ());
	  }
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

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof StringBuffer))
      throw new WrongType (this.name(), 1, "file name");
    String name = arg1.toString ();
    if (name.endsWith (".zip"))
      return loadCompiled (name);
    if (name.endsWith (".scm"))
      return loadSource (name);
    if (name.endsWith (".class"))
      {
	name = name.substring (0, name.length () - 6);
	return loadClassFile (name.replace ('/', '.'));
      }
    File file = new File (name);
    if (file.exists ())
      {
	try
	  {
	    FileInputStream fstream = new FileInputStream (name);
	    InPort port = new InPort (fstream);
	    int char0 = port.readChar ();
	    if (char0 == -1)
	      return Interpreter.eofObject;
	    if (char0 == 'P')
	      {
		int char1 = port.readChar ();
		if (char1 == 'K')
		  {
		    int char2 = port.readChar ();
		    if (char2 == '\003')
		      {
			int char3 = port.readChar ();
			if (char3 == '\004')
			  {
			    port.close ();
			    return loadCompiled (name);
			  }
			port.unreadChar ();  // unread char3
		      }
		    port.unreadChar ();  // unread char2
		  }
		port.unreadChar ();  // unread char 1
	      }
	    port.unreadChar ();  // unread char 0
	    return loadSource (port);
	  }
	catch (java.io.FileNotFoundException e)
	  {
	    throw new GenericError ("load: file not readable: " + name);
	  }
	catch (java.io.IOException e)
          {
            throw new GenericError ("I/O exception in load: "+e.toString ());
          }
      }
    else
      {
	String xname = name + ".zip";
	file = new File (xname);
	if (file.exists ())
	  return loadCompiled (xname);

	xname = name + ".class";
	file = new File (xname);
	if (file.exists ())
	  return loadClassFile (xname);

	xname = name + ".scm";
	file = new File (xname);
	if (file.exists ())
	  return loadSource (xname);
      }
    throw new GenericError ("load:  " + name + " - not found");
  }
}
