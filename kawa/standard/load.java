package kawa.standard;
import kawa.lang.*;
import java.io.*;
import gnu.bytecode.ZipLoader;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.lists.*;

public class load extends Procedure1 {
  /** Load using the name of a compile .class file. */
  /* This should probably be re-written to use a ClassLoader, unless '.'
   * is in the CLASSPATH, since it a bit ugly that load of a source file
   * or .zip file reads a file (using a relative or absolute file name),
   * while load of a compiled .class uses the classpath. */
  public final static void loadClassFile (String name, Environment env)
  {
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
	Class clas = Class.forName (name);
	Object inst = clas.newInstance ();
	gnu.kawa.reflect.ClassMemberConstraint.defineAll(inst, env);
	if (inst instanceof Runnable)
	  ((Runnable)inst).run();
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
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public final static void loadCompiled (String name, Environment env)
    throws Throwable
  {
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
	File zfile = new File (name);
	if (!zfile.exists ())
	  throw new RuntimeException ("load: "+name+" - not found");
	if (!zfile.canRead ())
	  throw new RuntimeException ("load: "+name+" - not readable");
	ZipLoader loader = new ZipLoader (name);
	loader.loadAllClasses();
	Class clas = loader.loadClass (LambdaExp.fileFunctionName, true);
	Object proc = clas.newInstance ();
	if (proc instanceof ModuleBody)
	  {
	    gnu.kawa.reflect.ClassMemberConstraint.defineAll(proc, env);
	    ((ModuleBody) proc).run();
	  }
	else
	  ((CallFrame) proc).apply0();
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
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public final static void loadSource (String name, Environment env)
    throws SyntaxException
  {
    try
      {
	InPort fstream = InPort.openFile(name);
	loadSource (fstream, env);
	fstream.close();
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new RuntimeException ("load: file not found: " + name);
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException("failed to close \""+name+"\" after loading");
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  public final static void loadSource (InPort port, Environment env)
    throws SyntaxException, Throwable
  {
    boolean print = ModuleBody.getMainPrintValues();
    Interpreter interp = Interpreter.getInterpreter();
    Consumer out = (print ? kawa.Shell.getOutputConsumer(OutPort.outDefault())
		    : new VoidConsumer());
    out.beginDocument();
    // Reading the entire file and evaluting it as a unit is more
    // consistent with compiled code, and more efficient.
    // Unfortunately, it is difficult to get macros to work properly.
    // So instead, we read and evaluate each line individually.
    if (true)
      {
	kawa.Shell.run(interp, env, port, out, OutPort.errDefault());
      }
    else
      {
	SourceMessages messages = new SourceMessages();
	ModuleExp mexp = CompileFile.read(port, messages);
	mexp.setName (Symbol.make (LambdaExp.fileFunctionName));
	if (messages.seenErrors())
	  throw new SyntaxException(messages);
	CallContext ctx = CallContext.getInstance();
	Consumer save = ctx.consumer;
	try
	  {
	    ctx.consumer = out;
	    ctx.values = Values.noArgs;
	    mexp.evalModule(env, ctx);
	  }
	finally
	  {
	    ctx.consumer = save;
	  }
      }
    out.endDocument();
  }

  public final Object apply1 (Object arg1)
    throws Throwable
  {
    return apply2 (arg1, Environment.current ());
  }

  public final Object apply2 (Object arg1, Object arg2)
    throws Throwable
  {
    String name = arg1.toString();
    try
      {
	Environment env = (Environment) arg2;
	apply (name, env);
	return Scheme.voidObject;
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new RuntimeException ("load: file not readable: " + name);
      }
    catch (SyntaxException ex)
      {
	throw new RuntimeException("load: errors while compiling '"+
				   name+"':\n"+ex.getMessages().toString(20));
      }
  }

  public static final void apply (String name, Environment env)
    throws Throwable
  {
    if (name.endsWith (".zip") || name.endsWith(".jar"))
      {
	loadCompiled (name, env);
	return;
      }
    if (name.endsWith (".scm"))
      {
	loadSource (name, env);
	return;
      }
    char file_separator = System.getProperty ("file.separator").charAt(0);

    if (name.endsWith (".class"))
      {
	name = name.substring (0, name.length () - 6);
	name = name.replace ('/', '.');
	if (file_separator != '/')
	  name = name.replace (file_separator, '.');
	loadClassFile (name, env);
	return;
      }
    File file = new File (name);
    if (file.exists ())
      {
	InputStream fs = new BufferedInputStream (new FileInputStream (name));
	fs.mark(5);
	int char0 = fs.read ();
	if (char0 == -1)
	  return; // Sequence.eofValue;
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
			loadCompiled (name, env);
			return;
		      }
		  }
	      }
	  }
	fs.reset();
	InPort src = InPort.openFile(fs, name);
	loadSource (src, env);
	src.close();
	return;
      }
    else
      {
	String fname = name.replace ('.', file_separator);
	String xname = fname + ".zip";
	file = new File (xname);
	if (file.exists ())
	  {
	    loadCompiled (xname, env);
	    return;
	  }
	xname = fname + ".jar";
	file = new File (xname);
	if (file.exists ())
	  {
	    loadCompiled (xname, env);
	    return;
	  }

	xname = fname + ".class";
	file = new File (xname);
	if (file.exists ())
	  {
	    loadClassFile (name, env);
	    return;
	  }

	xname = fname + ".scm";
	file = new File (xname);
	if (file.exists ())
	  {
	    loadSource (xname, env);
	    return;
	  }
      }
    throw new java.io.FileNotFoundException(name);
  }
}
