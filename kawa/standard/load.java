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

  public final static Object loadClassFile (String name, Environment env)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	Class clas = Class.forName (name);
	Object inst = clas.newInstance ();
	if (! (inst instanceof Procedure0))
	  throw new GenericError ("load - class is not an Procedure0");
	return ((ModuleBody)inst).run (env);
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

  public final static Object loadCompiled (String name, Environment env)
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
	Class clas = loader.loadClass (LambdaExp.fileFunctionName, true);
	return ((ModuleBody) clas.newInstance ()).run (env);
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

  public final static Object loadSource (String name, Environment env)
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
    return loadSource (new InPort (fstream, name), env);
  }

  public final static Object loadSource (InPort port, Environment env)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Translator tr = new Translator (env);
    ModuleExp mexp = CompileFile.read (port, tr);
    mexp.setName (Symbol.make (LambdaExp.fileFunctionName));
    if (tr.errors > 0)
      throw new GenericError ("syntax errors during load");
    return ((ModuleBody) mexp.eval (env)).run (env);
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply2 (arg1, Environment.current ());
  }

  public final Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Environment env = (Environment) arg2;
    String name = arg1.toString ();
    if (name.endsWith (".zip"))
      return loadCompiled (name, env);
    if (name.endsWith (".scm"))
      return loadSource (name, env);
    if (name.endsWith (".class"))
      {
	name = name.substring (0, name.length () - 6);
	return loadClassFile (name.replace ('/', '.'), env);
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
	      return Sequence.eofValue;
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
			    return loadCompiled (name, env);
			  }
			port.unreadChar ();  // unread char3
		      }
		    port.unreadChar ();  // unread char2
		  }
		port.unreadChar ();  // unread char 1
	      }
	    port.unreadChar ();  // unread char 0
	    return loadSource (port, env);
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
	  return loadCompiled (xname, env);

	xname = name + ".class";
	file = new File (xname);
	if (file.exists ())
	  return loadClassFile (xname, env);

	xname = name + ".scm";
	file = new File (xname);
	if (file.exists ())
	  return loadSource (xname, env);
      }
    throw new GenericError ("load:  " + name + " - not found");
  }
}
