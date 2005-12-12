package kawa.standard;
import kawa.lang.*;
import java.io.*;
import gnu.bytecode.ZipLoader;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.lists.*;
import gnu.kawa.functions.BaseUri;
import java.net.URL;

public class load extends Procedure1 {
  boolean relative;

  public load (String name, boolean relative)
  {
    super(name);
    this.relative = relative;
  }

  public static final load load = new load("load", false);
  public static final load loadRelative = new load("load-relative", true);

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
	gnu.kawa.reflect.ClassMemberLocation.defineAll(inst, env);
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
	gnu.kawa.reflect.ClassMemberLocation.defineAll(proc, env);
	((ModuleBody) proc).run();
      }
    catch (java.io.IOException ex)
      {
	throw new WrappedException ("load: "+name+" - "+ex.toString (), ex);
      }
    catch (ClassNotFoundException ex)
      {
	throw new WrappedException ("class not found in load", ex);
      }
    catch (InstantiationException ex)
      {
	throw new WrappedException ("class not instantiable: in load", ex);
      }
    catch (IllegalAccessException ex)
      {
	throw new WrappedException ("class illegal access: in load", ex);
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public final static void loadSource (InPort port, Environment env, URL url)
    throws SyntaxException, Throwable
  {
    boolean print = ModuleBody.getMainPrintValues();
    Language language = Language.getDefaultLanguage();
    Consumer out = (print ? kawa.Shell.getOutputConsumer(OutPort.outDefault())
		    : new VoidConsumer());
    // Reading the entire file and evaluting it as a unit is more
    // consistent with compiled code, and more efficient.
    // Unfortunately, it is difficult to get macros to work properly.
    // So instead, we read and evaluate each line individually.
    if (true)
      {
	kawa.Shell.run(language, env, port, out, OutPort.errDefault(), url);
      }
    else
      {
	SourceMessages messages = new SourceMessages();
	Compilation comp
          = language.parse(port, messages, Language.PARSE_IMMEDIATE);
	ModuleExp mexp = comp.getModule();
	mexp.setName(Symbols.make(LambdaExp.fileFunctionName));
	CallContext ctx = CallContext.getInstance();
	Consumer save = ctx.consumer;
	try
	  {
	    ctx.consumer = out;
	    ctx.values = Values.noArgs;
	    ModuleExp.evalModule(env, ctx, comp, url);
	    if (messages.seenErrors())
	      throw new SyntaxException(messages);
	  }
	finally
	  {
	    ctx.consumer = save;
	  }
      }
  }

  public final Object apply1 (Object arg1)
    throws Throwable
  {
    return apply2 (arg1, Environment.getCurrent ());
  }

  public final Object apply2 (Object arg1, Object arg2)
    throws Throwable
  {
    String name = arg1.toString();
    try
      {
	Environment env = (Environment) arg2;
	apply (name, env, relative, 0);
	return Values.empty;
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

  public static final void apply (String name, Environment env,
				  boolean relative, int skipLines)
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    String savedBaseUri = ctx.getBaseUri();
    try
      {
        boolean isUri = InPort.uriSchemeSpecified(name);
        // Resolve a relative URI.  However, if the base uri matches the
        // default base uri (i.e. the current directory) just leave it as
        // a filename, rather than coercing it to a URI.
        String resolved = name;
        if (! isUri)
          {
            String base = savedBaseUri;
            if (! InPort.uriSchemeSpecified(base))
              base = BaseUri.resolve(base, ctx.getBaseUriDefault());
            resolved = BaseUri.resolve(name, base);
            if (relative && ! savedBaseUri.equals(ctx.getBaseUriDefault()))
              name = resolved;
          }
	ctx.setBaseUri(name);
	if (name.endsWith (".zip") || name.endsWith(".jar"))
	  {
	    loadCompiled (name, env);
	    return;
	  }
        URL url = new URL(resolved);
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
        InputStream fs;
        if (isUri)
          fs = url.openConnection().getInputStream();
        else
          fs = new FileInputStream(name);
        fs = new BufferedInputStream(fs);
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
        while (--skipLines >= 0)
          src.skipRestOfLine();
        loadSource (src, env, url);
        src.close();
        return;
      }
    finally
      {
	ctx.setBaseUri(savedBaseUri);
      }
  }
}
