package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.text.*;
import java.io.*;
import gnu.kawa.reflect.StaticFieldLocation;
import java.net.URL;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends LambdaExp
		       implements Externalizable
{
  public static final int EXPORT_SPECIFIED = LambdaExp.NEXT_AVAIL_FLAG;
  public static final int STATIC_SPECIFIED = EXPORT_SPECIFIED << 1;
  public static final int NONSTATIC_SPECIFIED = STATIC_SPECIFIED << 1;
  public static final int SUPERTYPE_SPECIFIED = NONSTATIC_SPECIFIED << 1;
  public static final int STATIC_RUN_SPECIFIED = SUPERTYPE_SPECIFIED << 1;
  public static final int LAZY_DECLARATIONS = STATIC_RUN_SPECIFIED << 1;
  public static final int IMMEDIATE = LAZY_DECLARATIONS << 1;

  public ModuleExp ()
  {
  }

  /** Used to control which .zip file dumps are generated. */
  public static String dumpZipPrefix;

  static int lastZipCounter;

  /** Numeric identifier for this interactive "command".
   * Incremented by Shell.run, and used to set the module name,
   * and maybe the name of the --debug-dump-zip output file.
   * We increment and use this counter purely to ease debugging.
   * (Since each module gets its own ClassLoader, they don't
   * need to be named differently, and it doesn't matter
   * if there is a race condition on the counter.) */
  public static int interactiveCounter;

  public static Class evalToClass (Compilation comp, URL url)
    throws SyntaxException
  {
    ModuleExp mexp = comp.getModule();
    SourceMessages messages = comp.getMessages();
    try
      {

        comp.minfo.loadByStages(Compilation.COMPILED);

	if (messages.seenErrors())
	  return null;

	ArrayClassLoader loader = comp.loader;
        if (url == null)
          url = Path.currentPath().toURL();
        loader.setResourceContext(url);

	java.util.zip.ZipOutputStream zout = null;
	if (dumpZipPrefix != null)
	  {
	    StringBuffer zipname = new StringBuffer(dumpZipPrefix);
            
            lastZipCounter++;
	    if (interactiveCounter > lastZipCounter)
	      lastZipCounter = interactiveCounter;
            zipname.append(lastZipCounter);
	    zipname.append(".zip");
	    java.io.FileOutputStream zfout
	      = new java.io.FileOutputStream(zipname.toString());
	    zout = new java.util.zip.ZipOutputStream(zfout);
	  }

	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    String className = clas.getName ();
	    byte[] classBytes = clas.writeToArray ();
	    loader.addClass(className, classBytes);

	    if (zout != null)
	      {
		String clname = className.replace ('.', '/') + ".class";
		java.util.zip.ZipEntry zent
		  = new java.util.zip.ZipEntry (clname);
		zent.setSize(classBytes.length);
		java.util.zip.CRC32 crc = new java.util.zip.CRC32();
		crc.update(classBytes);
		zent.setCrc(crc.getValue());
		zent.setMethod(java.util.zip.ZipEntry.STORED);
		zout.putNextEntry(zent);
		zout.write(classBytes);
	      }
	  }
	if (zout != null)
	  {
	    zout.close ();
	  }

	/* DEBUGGING:
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  ClassTypeWriter.print(comp.classes[iClass], System.out, 0);
	*/

        Class clas = null;
        // Use the "session" ClassLoader, for remembering classes
        // created in one command (Compilation) through further command,
        // while still allowing the classes to be replaced and collected.
        ArrayClassLoader context = loader;
        while (context.getParent() instanceof ArrayClassLoader)
          context = (ArrayClassLoader) context.getParent();
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType ctype = comp.classes[iClass];
            Class cclass = loader.loadClass(ctype.getName());
            ctype.setReflectClass(cclass);
            ctype.setExisting(true);
            if (iClass == 0)
              clas = cclass;
            // Add all classes except the main module class to the "session"
            // ClassLoader.  Don't add the main module class, as it's
            // anonymous.  We might go further and skip other anonymous
            // classes (theough defining which classes are anonymous is tricky).
            else if (context != loader)
              context.addClass(cclass);
          }

        ModuleInfo minfo = comp.minfo;
        minfo.setModuleClass(clas);
        comp.cleanupAfterCompilation();
        int ndeps = minfo.numDependencies;

        for (int idep = 0;  idep < ndeps;  idep++)
          {
            ModuleInfo dep = minfo.dependencies[idep];
            Class dclass = dep.getModuleClassRaw();
            if (dclass == null)
              dclass = evalToClass(dep.comp, null);
            comp.loader.addClass(dclass);
          }

        return clas;
      }
    catch (java.io.IOException ex)
      {
	throw new WrappedException("I/O error in lambda eval", ex);
      }
    catch (ClassNotFoundException ex)
      {
	throw new WrappedException("class not found in lambda eval", ex);
      }
    catch (Throwable ex)
      {
	comp.getMessages()
          .error('f', "internal compile error - caught "+ex, ex);
        throw new SyntaxException(messages);
      }
  }

  /** Flag to force compilation, even when not required. */
  public static boolean alwaysCompile = true;

  public final static boolean evalModule (Environment env, CallContext ctx,
                                       Compilation comp, URL url,
                                       OutPort msg)
    throws Throwable
  {
    ModuleExp mexp = comp.getModule();
    Language language = comp.getLanguage();
    Object inst = evalModule1(env, ctx, comp, url, msg);
    if (inst == null)
      return false;
    evalModule2(env, ctx, language, mexp, inst, msg);
    return true;
  }

  public final static Object evalModule1 (Environment env, CallContext ctx,
                                       Compilation comp, URL url,
                                       OutPort msg)
    throws Throwable
  {
    comp.getLanguage().resolve(comp);
    ModuleExp mexp = comp.getModule();
    Environment orig_env = Environment.getCurrent();
    Compilation orig_comp = Compilation.getCurrent();
    SourceMessages messages = comp.getMessages();
    ClassLoader savedLoader = null;
    Thread thread = null; // Non-null if we need to restore context ClassLoader.
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
        if (comp != orig_comp)
          Compilation.setCurrent(comp);

        if (alwaysCompile)
          comp.mustCompile = true;

        if (comp.mustCompile)
          comp.addMainClass(mexp);

        comp.walkModule(mexp);
        comp.setState(Compilation.WALKED);

        if (msg != null ? messages.checkErrors(msg, 20) : messages.seenErrors())
          return null;

	if (! comp.mustCompile)
	  {
	    if (Compilation.debugPrintFinalExpr && msg != null)
	      {
		msg.println ("[Evaluating final module \""+mexp.getName()+"\":");
		mexp.print(msg);
		msg.println(']');
		msg.flush();
	      }
            return Boolean.TRUE;
          }
        else
          {
		Class clas = evalToClass(comp, url);
		if (clas == null)
		  return null;
                try
                  {
                    thread = Thread.currentThread();
                    savedLoader = thread.getContextClassLoader();
                    thread.setContextClassLoader(clas.getClassLoader());
                  }
                catch (Throwable ex)
                  {
                    thread = null;
                  }

                Object inst;
                try
                  {
                    inst = clas.getDeclaredField("$instance").get(null);
                  }
                catch (NoSuchFieldException ex)
                  {
                    inst = clas.newInstance();
                  }
                /* #ifdef use:java.lang.Throwable.getCause */
                catch (ExceptionInInitializerError ex)
                  {
                    throw ex.getCause();
                  }
                /* #endif */

                mexp.body = null;
                mexp.thisVariable = null;
                if (msg != null ? messages.checkErrors(msg, 20)
                    : messages.seenErrors())
                  return null;
                return inst;
          }
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
	if (comp != orig_comp)
	  Compilation.setCurrent(orig_comp);
        if (thread != null)
          thread.setContextClassLoader(savedLoader);
      }
  }

  public final static void evalModule2 (Environment env, CallContext ctx,
                                        Language language, ModuleExp mexp,
                                        Object inst, OutPort msg)
    throws Throwable
  {
    Environment orig_env = Environment.getCurrent();
    ClassLoader savedLoader = null;
    Thread thread = null; // Non-null if we need to restore context ClassLoader.
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);

	if (inst == Boolean.TRUE)
	  { // optimization - don't generate unneeded Class.
	    mexp.body.apply(ctx);
	  }
	else
	  {
            //	    try
	      {
                if (inst instanceof ModuleBody)
                  ((ModuleBody) inst).run(ctx);

		// Import declarations defined in module into the Environment.
		for (Declaration decl = mexp.firstDecl();
		     decl != null;  decl = decl.nextDecl())
		  {
		    Object dname = decl.getSymbol();
		    if (decl.isPrivate() || dname == null)
		      continue;
		    Field fld = decl.field;
		    Symbol sym = dname instanceof Symbol ? (Symbol) dname
		      : Symbol.make("", dname.toString().intern());
		    Object property = language.getEnvPropertyFor(decl);
                    Expression dvalue = decl.getValue();
                    // It would be cleaner to not bind these values in
                    // the environment, and just require lexical lookup
                    // However, various parts of the code makes use of
                    // the environment.
		    if ((decl.field.getModifiers() & Access.FINAL) != 0
                        && ! (dvalue instanceof ReferenceExp
                              && ((ReferenceExp) dvalue).getBinding().needsContext()))
		      {
			Object value;
			if (dvalue instanceof QuoteExp
			    && dvalue != QuoteExp.undefined_exp)
			  value = ((QuoteExp) dvalue).getValue();
			else
			  {
			    value = decl.field.getReflectField().get(null);
                            if (! decl.isIndirectBinding())
                              decl.setValue(QuoteExp.getInstance(value));
                            else if (! decl.isAlias() || ! (dvalue instanceof ReferenceExp))
                              decl.setValue(null);
                          }
			if (decl.isIndirectBinding())
                          env.addLocation(sym, property, (Location) value);
                        else
                          env.define(sym, property, value);
		      }
		    else
		      {
                        StaticFieldLocation loc
                          = new StaticFieldLocation(fld.getDeclaringClass(),
                                                    fld.getName());
			loc.setDeclaration(decl);
			env.addLocation(sym, property, loc);
			decl.setValue(null);
		      }
		  }
	      }
            /*
	    catch (IllegalAccessException ex)
	      {
		throw new RuntimeException("class illegal access: in lambda eval");
	      }
            */
	  }
	ctx.runUntilDone();
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
        if (thread != null)
          thread.setContextClassLoader(savedLoader);
      }
  }

  ClassType superType;
  ClassType[] interfaces;

  ModuleInfo info;

  public String getNamespaceUri () { return info.uri; }

  public final ClassType getSuperType() { return superType; }
  public final void setSuperType(ClassType s) { superType = s; }
  public final ClassType[] getInterfaces() { return interfaces; }
  public final void setInterfaces(ClassType[] s) { interfaces = s; }

  public final boolean isStatic ()
  {
    // In immediate mode there is no point in a non-static module:
    // a static module is simpler and more efficient.
    return (getFlag(STATIC_SPECIFIED)
	    || ((gnu.expr.Compilation.moduleStatic > 0
                 || getFlag(IMMEDIATE))
		&& ! getFlag(SUPERTYPE_SPECIFIED)
		&& ! getFlag(NONSTATIC_SPECIFIED)));
  }

  /** True if module body (i.e. run) is called by class initializer. */
  public boolean staticInitRun ()
  {
    return (isStatic()
            && (getFlag(STATIC_RUN_SPECIFIED)
                || Compilation.moduleStatic == 2));
  }

  public void allocChildClasses (Compilation comp)
  {
    declareClosureEnv();
    if (! comp.usingCPStyle())
      return;
    allocFrame(comp);
  }

  void allocFields (Compilation comp)
  {
    // We want the create the loc$XXX Symbol fields for unknowns first,
    // because it is possible some later Declaration's initializer may depend
    // on it.  Normally this is not an issue, as initializer are usually
    // run as part of the "body" of the module, which is executed later.
    // However, constant initializers are an exception - they are
    // executed at init time.
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if ((decl.isSimple() && ! decl.isPublic()) || decl.field != null)
	  continue;
	if (decl.getFlag(Declaration.IS_UNKNOWN)
            // We might have an unrefered unknown if the reference gets
            // optimized away. For example references to <CLASSNAME>.
            && decl.getFlag(Declaration.CAN_READ|Declaration.CAN_CALL))
	  decl.makeField(comp, null);
      }
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.field != null)
	  continue;
	Expression value = decl.getValue();
	if (((decl.isSimple() && ! decl.isPublic()))
            // Kludge - needed for macros - see Savannah bug #13601.
            && ! decl.isNamespaceDecl()
            && ! (decl.getFlag(Declaration.IS_CONSTANT)
                  && decl.getFlag(Declaration.CAN_READ|Declaration.CAN_CALL))
	    && ! (value instanceof ClassExp))
	  continue;
	if (decl.getFlag(Declaration.IS_UNKNOWN))
	  continue;
	if (value instanceof LambdaExp
            && ! (value instanceof ModuleExp // from a module-name command.
                  || value instanceof ClassExp))
	  {
	    ((LambdaExp) value).allocFieldFor(comp);
	  }
	else
	  {
            if (! (decl.getFlag(Declaration.IS_CONSTANT) || decl.isAlias())
		|| value == QuoteExp.undefined_exp)
	      value = null;
	    decl.makeField(comp, value);
	  }
      }
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkModuleExp(this);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Module/", ")", 2);
    Object sym = getSymbol();
    if (sym != null)
      {
	out.print(sym);
	out.print('/');
      }
    out.print(id);
    out.print('/');
    out.writeSpaceFill();
    out.startLogicalBlock("(", false, ")");
    Declaration decl = firstDecl();
    if (decl != null)
      {
	out.print("Declarations:");
	for (; decl != null;  decl = decl.nextDecl())
	  {
	    out.writeSpaceFill();
	    decl.printInfo(out);
	  }
      }
    out.endLogicalBlock(")");
    out.writeSpaceLinear();
    if (body == null)
      out.print("<null body>");
    else
      body.print (out);
    out.endLogicalBlock(")");
  }

  public Declaration firstDecl ()
  {
    synchronized (this)
      {
	if (getFlag(LAZY_DECLARATIONS))
          info.setupModuleExp();
      }
    return decls;
  }

  /** Return the class this module.
   * If not set yet, sets it now, based on the source file name.
   */
  public ClassType classFor (Compilation comp)
  {
    if (type != null && type != Compilation.typeProcedure)
      return (ClassType) type;
    String fileName = getFileName();
    String mname = getName();
    String className = null;
    Path path = null;
    if (mname != null)
      fileName = mname;
    else if (fileName == null)
      {
        fileName = getName();
        if (fileName == null)
          fileName = "$unnamed_input_file$";
      }
    else if (filename.equals("-") || filename.equals("/dev/stdin"))
      {
        fileName = getName();
        if (fileName == null)
          fileName = "$stdin$";
      }
    else
      {
        path = Path.valueOf(fileName);
        fileName = path.getLast();
        int dotIndex = fileName.lastIndexOf('.');
        if (dotIndex > 0)
          fileName = fileName.substring (0, dotIndex);
      }
    Path parentPath;
    String parent;
    if (getName() == null)
      setName(fileName);
    fileName = Compilation.mangleNameIfNeeded(fileName);
    if (comp.classPrefix.length() == 0
        && path != null
        && ! path.isAbsolute()
        && (parentPath = path.getParent()) != null
        && (parent = parentPath.toString()).length() > 0 // Probably redundant.
        && parent.indexOf("..") < 0)
      {
        parent = parent.replaceAll(System.getProperty("file.separator"), "/");
        if (parent.startsWith("./"))
          parent = parent.substring(2);
        className = parent.equals(".") ? fileName
          : Compilation.mangleURI(parent) + "." + fileName;
      }
    else
      className = comp.classPrefix + fileName;
    ClassType clas = new ClassType(className);
    setType(clas);
    if (comp.mainLambda == this)
      {
        if (comp.mainClass == null)
          comp.mainClass = clas;
        else if (! className.equals(comp.mainClass.getName()))
          comp.error('e', "inconsistent main class name: "+className
                     +" - old name: "+comp.mainClass.getName());
      }
    return clas;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    String name = null;
    if (type != null && type != Compilation.typeProcedure
	&& ! type.isExisting())
      // The class is (presumably) one we're currently generating.
      // At run-time it may be loaded by a non-system ClassLoader.
      // Thus compiling the class literal needs to use loadClassRef.
      out.writeObject(type);
    else
      {
	if (name == null)
	  name = getName();
	if (name == null)
	  name = getFileName();
	out.writeObject(name);
      }
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    Object name = in.readObject();
    if (name instanceof ClassType)
      {
	type = (ClassType) name;
	setName(type.getName());
      }
    else
      setName((String) name);
    flags |= LAZY_DECLARATIONS;
  }
}
