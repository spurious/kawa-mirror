package kawa.standard;
import kawa.lang.*;
import java.io.*;
import gnu.bytecode.ZipLoader;
import gnu.mapping.*;
import gnu.expr.*;

public class load extends Procedure1 {
  /** Load using the name of a compile .class file. */
  /* This should probably be re-written to use a ClassLoader, unless '.'
   * is in the CLASSPATH, since it a bit ugly that load of a source file
   * or .zip file reads a file (using a relative or absolute file name),
   * while load of a compiled .class uses the classpath. */
  public final static Object loadClassFile (String name, Environment env)
  {
    try
      {
	Class clas = Class.forName (name);
	Object inst = clas.newInstance ();
	if (! (inst instanceof Procedure0))
	  throw new RuntimeException ("load - class is not an Procedure0");
	return ((ModuleBody)inst).run (env);
      }
    catch (ClassNotFoundException ex)
      {
	throw new RuntimeException ("class not found in load");
      }
    catch (InstantiationException ex)
      {
	throw new RuntimeException ("class not instantiable: in load");
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException ("class illegal access: in load");
      }
  }

  public final static Object loadCompiled (String name, Environment env)
  {
    try
      {
	File zfile = new File (name);
	if (!zfile.exists ())
	  throw new RuntimeException ("load: "+name+" - not found");
	if (!zfile.canRead ())
	  throw new RuntimeException ("load: "+name+" - not readable");
	ZipLoader loader = new ZipLoader (name);
	Class clas = loader.loadClass (LambdaExp.fileFunctionName, true);
	return ((ModuleBody) clas.newInstance ()).run (env);
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException ("load: "+name+" - "+ex.toString ());
      }
    catch (ClassNotFoundException ex)
      {
	throw new RuntimeException ("class not found in load");
      }
    catch (InstantiationException ex)
      {
	throw new RuntimeException ("class not instantiable: in load");
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException ("class illegal access: in load");
      }
  }

  public final static Object loadSource (String name, Environment env)
  {
    try
      {
	InPort fstream = InPort.openFile(name);
	Object result = loadSource (fstream, env);
	fstream.close();
	return result;
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new RuntimeException ("load: file not found: " + name);
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException("failed to close \""+name+"\" after loading");
      }
  }

  public final static Object loadSource (InPort port, Environment env)
  {
    // Reading the entire file and evaluting it as a unit is more
    // consistent with compiled code, and more efficient.
    // Unfortunately, it is difficult to get macros to work properly.
    // So instead, we read and evaluate each line individually.
    if (true)
      {
	kawa.Shell.run(new Scheme(env), port, null, OutPort.errDefault());
	return Scheme.voidObject;
      }
    else
      {
	Translator tr = new Translator (env);
	ModuleExp mexp = CompileFile.read (port, tr);
	mexp.setName (Symbol.make (LambdaExp.fileFunctionName));
	if (tr.errors > 0)
	  throw new RuntimeException ("syntax errors during load");
	return ((ModuleBody) mexp.eval (env)).run (env);
      }
  }

  public final Object apply1 (Object arg1)
  {
    return apply2 (arg1, Environment.current ());
  }

  public final Object apply2 (Object arg1, Object arg2)
  {
    Environment env = (Environment) arg2;
    String name = arg1.toString ();
    if (name.endsWith (".zip") || name.endsWith(".jar"))
      return loadCompiled (name, env);
    if (name.endsWith (".scm"))
      return loadSource (name, env);
    char file_separator = System.getProperty ("file.separator").charAt(0);
    if (name.endsWith (".class"))
      {
	name = name.substring (0, name.length () - 6);
	name = name.replace ('/', '.');
	if (file_separator != '/')
	  name = name.replace (file_separator, '.');
	return loadClassFile (name, env);
      }
    File file = new File (name);
    if (file.exists ())
      {
	try
	  {
	    InputStream fs = new BufferedInputStream (new FileInputStream (name));
	    fs.mark(5);
	    int char0 = fs.read ();
	    if (char0 == -1)
	      return Sequence.eofValue;
	    if (char0 == 'P')
	      {
		int char1 = fs.read ();
		if (char1 == 'K')
		  {
		    int char2 = fs.read ();
		    if (char2 == '\003')
		      {
			int char3 = fs.read ();
			if (char3 == '\004')
			  {
			    fs.close ();
			    return loadCompiled (name, env);
			  }
		      }
		  }
	      }
	    fs.reset();
	    try
	      {
		InPort src = InPort.openFile(fs, name);
		Object result = loadSource (src, env);
		src.close();
		return result;
	      }
	    catch (java.io.IOException ex)
	      {
		throw new RuntimeException("failed to close \""+name
				       +"\" after loading");
	      }
	  }
	catch (java.io.FileNotFoundException e)
	  {
	    throw new RuntimeException ("load: file not readable: " + name);
	  }
	catch (java.io.IOException e)
          {
            throw new RuntimeException ("I/O exception in load: "+e.toString ());
          }
      }
    else
      {
	String fname = name.replace ('.', file_separator);
	String xname = fname + ".zip";
	file = new File (xname);
	if (file.exists ())
	  return loadCompiled (xname, env);
	xname = fname + ".jar";
	file = new File (xname);
	if (file.exists ())
	  return loadCompiled (xname, env);

	xname = fname + ".class";
	file = new File (xname);
	if (file.exists ())
	  return loadClassFile (name, env);

	xname = fname + ".scm";
	file = new File (xname);
	if (file.exists ())
	  return loadSource (xname, env);
      }
    throw new RuntimeException ("load:  " + name + " - not found");
  }
}
