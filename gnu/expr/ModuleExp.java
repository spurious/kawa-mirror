package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.text.*;
import java.io.*;
import gnu.kawa.reflect.StaticFieldLocation;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends LambdaExp
		       implements Externalizable
{
  public static boolean debugPrintExpr = false;

  public static final int EXPORT_SPECIFIED = LambdaExp.NEXT_AVAIL_FLAG;
  public static final int STATIC_SPECIFIED = EXPORT_SPECIFIED << 1;
  public static final int NONSTATIC_SPECIFIED = STATIC_SPECIFIED << 1;
  public static final int SUPERTYPE_SPECIFIED = NONSTATIC_SPECIFIED << 1;
  public static final int LAZY_DECLARATIONS = SUPERTYPE_SPECIFIED << 11;

  public String getJavaName ()
  {
    String name = getName();
    return name == null ? "lambda" : Compilation.mangleName(name);
  }

  public ModuleExp ()
  {
  }

  public static ModuleExp make (String name)
  {
    ModuleExp mexp = new ModuleExp();
    mexp.setName(name);
    mexp.flags |= LAZY_DECLARATIONS;
    return mexp;
  }

  public static ModuleExp make (ClassType type)
  {
    ModuleExp mexp = new ModuleExp();
    mexp.type = type;
    mexp.setName(type.getName());
    mexp.flags |= LAZY_DECLARATIONS;
    return mexp;
  }

  /** Used to control which .zip file dumps are generated. */
  public static String dumpZipPrefix;

  /** Numeric identifier for this interactive "command".
   * Incremented by Shell.run, and used to set the module name,
   * and maybe the name of the --debug-dump-zip output file.
   * We increment and use this counter purely to ease debugging.
   * (Since each module gets its own ClassLoader, they don't
   * need to be named differently, and it doesn't matter
   * if there is a race condition on the counter.) */
  public static int interactiveCounter;

  ///** A cache if this has already been evaluated. */
  //Procedure thisValue;

  public static Class evalToClass (Compilation comp)
  {
    ModuleExp mexp = comp.getModule();
    SourceMessages messages = comp.getMessages();
    try
      {
	String class_name = mexp.getJavaName ();
	comp.immediate = true;
	ArrayClassLoader loader = new ArrayClassLoader ();
	comp.loader = loader;

	comp.compile(mexp, class_name, null);
	// FIXME - doesn't emit warnings.
	if (messages.seenErrors())
	  return null;

	byte[][] classes = new byte[comp.numClasses][];

	java.util.zip.ZipOutputStream zout = null;
	if (dumpZipPrefix != null)
	  {
	    StringBuffer zipname = new StringBuffer(dumpZipPrefix);
	    if (interactiveCounter >= 0)
	      // Incremented by Shell.run.
	      zipname.append(interactiveCounter);
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

        CallContext ctx = CallContext.getInstance();
        ctx.value1 = comp;

	Class clas = loader.loadClass (class_name, true);
	comp.mainClass.setReflectClass(clas);
        return clas;
      }
    catch (java.io.IOException ex)
      {
	ex.printStackTrace(OutPort.errDefault());
	throw new RuntimeException ("I/O error in lambda eval: "+ex);
      }
    catch (ClassNotFoundException ex)
      {
	throw new RuntimeException("class not found in lambda eval");
      }
  }

  /** Flag to force compilation, even when not required. */
  public static boolean alwaysCompile = false;

  public final static void evalModule (Environment env, CallContext ctx, Compilation comp) throws Throwable
  {
    ModuleExp mexp = comp.getModule();
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);

	if (! alwaysCompile && ! comp.mustCompile)
	  { // optimization - don't generate unneeded Class.
	    if (debugPrintExpr)
	      {
		OutPort dout = OutPort.outDefault();
		dout.println ("[Evaluating module \""+mexp.getName()+"\":");
		mexp.print(dout);
		dout.println(']');
		dout.flush();
	      }
	    mexp.body.apply(ctx);
	  }
	else
	  {
	    try
	      {
		Class clas = evalToClass(comp);
		if (clas == null)
		  return;
                Object inst;
                try
                  {
                    inst = clas.getDeclaredField("$instance").get(null);
                  }
                catch (NoSuchFieldException ex)
                  {
                    inst = clas.newInstance();
                  }

		// Import declarations defined in module into the Environment.
		for (Declaration decl = mexp.firstDecl();
		     decl != null;  decl = decl.nextDecl())
		  {
		    Object dname = decl.getName();
		    if (decl.isPrivate() || dname == null)
		      continue;
		    Field fld = decl.field;
		    Symbol sym = dname instanceof Symbol ? (Symbol) dname
		      : Symbol.make("", dname.toString().intern());
		    Object property = comp.getLanguage()
		      .getEnvPropertyFor(decl);
		    // Would it be better to check if fld is FINAL?
                    // If it is, gets its value; otherwise create
                    // a FieldLocation to access it?  FIXME.
		    if (decl.getFlag(Declaration.PROCEDURE|Declaration.IS_CONSTANT|Declaration.INDIRECT_BINDING))
		      {
			Expression dvalue = decl.getValue();
			Object value;
			if (dvalue instanceof QuoteExp
			    && dvalue != QuoteExp.undefined_exp)
			  value = ((QuoteExp) dvalue).getValue();
			else
                          value = decl.field.getReflectField().get(null);
			if (decl.isIndirectBinding())
			  env.addLocation(sym, property, (Location) value);
			else
			  env.define(sym, property, value);
			// if IS_CONSTANT ... make Location constant.  FIXME.
		      }
		    else
		      {
                        Location loc
                          = new StaticFieldLocation(fld.getDeclaringClass(),
                                                    fld.getName());
			// Perhaps set loc.decl = decl?
			env.addLocation(sym, property, loc);
		      }
		  }
                if (inst instanceof ModuleBody)
                  ((ModuleBody) inst).run(ctx);
	      }
	    catch (IllegalAccessException ex)
	      {
		throw new RuntimeException("class illegal access: in lambda eval");
	      }
	  }
	ctx.runUntilDone();
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  /** Call-back from compiled code to initialize literals in immediate mode.
   * In non-immediate mode (i.e. generating class files) the compiler emits
   * code to "re-construct" literal values.  However, in immediate mode
   * that would be wasteful, plus we would get values that are similar (equals)
   * to but not necessarily identical (eq) to the compile-time literal.
   * So we need to pass the literal values to the compiled code, by using
   * reflectiion to initialize various static fields.  This method does that.
   * It is called from start of the the generated static initializer, which
   * helps makes things more consistent between immediate and non-immediate
   * mode.
   */
  public static void setupLiterals ()
  {
    CallContext ctx = CallContext.getInstance();
    Compilation comp = (Compilation) ctx.value1;
    try
      {
        Class clas = comp.loader.loadClass(comp.mainClass.getName(), true);

	/* Pass literal values to the compiled code. */
	for (Literal init = comp.litTable.literalsChain;  init != null;
	     init = init.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("init["+init.index+"]=");
	    out.print(init.value);
	    out.println();
	    */
            clas.getDeclaredField(init.field.getName())
              .set(null, init.value);
	  }
      }
    catch (Throwable ex)
      {
        throw new WrappedException("internal error", ex);
      }
  }

  ClassType superType;
  ClassType[] interfaces;

  public final ClassType getSuperType() { return superType; }
  public final void setSuperType(ClassType s) { superType = s; }
  public final ClassType[] getInterfaces() { return interfaces; }
  public final void setInterfaces(ClassType[] s) { interfaces = s; }

  public final boolean isStatic ()
  {
    return (getFlag(STATIC_SPECIFIED)
	    || (gnu.expr.Compilation.moduleStatic > 0
		&& ! getFlag(SUPERTYPE_SPECIFIED)
		&& ! getFlag(NONSTATIC_SPECIFIED)));
  }

  /** True if module body (i.e. run) is called by class initializer. */
  public boolean staticInitRun ()
  {
    return Compilation.moduleStatic == 2 && isStatic();
  }

  void allocFields (Compilation comp)
  {
    // We want the create the id$XXX Symbol fields for unknowns first,
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
	if (decl.getFlag(Declaration.IS_UNKNOWN))
	  decl.makeField(comp, null);
      }
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.field != null)
	  continue;
	Expression value = decl.getValue();
	if (((decl.isSimple() && ! decl.isPublic()) || decl.ignorable())
	    && ! (value instanceof ClassExp))
	  continue;
	if (decl.getFlag(Declaration.IS_UNKNOWN))
	  continue;
	if (value instanceof LambdaExp && ! (value instanceof ClassExp))
	  {
	    ((LambdaExp) value).allocFieldFor(comp);
	  }
	else
	  {
            if ((! decl.getFlag(Declaration.IS_CONSTANT) && ! decl.isAlias())
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
	  {
	    if (type == null)
	      type = ClassType.make(getName());
            ModuleInfo info = ModuleInfo.find(type.getName());
	    kawa.standard.require.makeModule(this, type, info.instance);
	    flags &= ~LAZY_DECLARATIONS;
	  }
      }
    return decls;
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
	  name = getFile();
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
