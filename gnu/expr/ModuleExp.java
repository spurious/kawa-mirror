package gnu.expr;
import java.io.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.text.*;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends LambdaExp
{
  /** True if the body is too complex to evaluate,and we must compile it.
   * This is because it contains a construct we know how to compile, but not
   * evaluate, and it it outside a lambda (which we always compile).
   * This can be a let scope, or primitive procedure. */
  public boolean mustCompile;

  public static boolean debugPrintExpr = false;

  public static final int EXPORT_SPECIFIED = LambdaExp.NEXT_AVAIL_FLAG;
  public static final int STATIC_SPECIFIED = EXPORT_SPECIFIED << 1;
  public static final int NONSTATIC_SPECIFIED = STATIC_SPECIFIED << 1;
  public static final int SUPERTYPE_SPECIFIED = NONSTATIC_SPECIFIED << 1;

  public String getJavaName ()
  {
    return name == null ? "lambda" : Compilation.mangleName (name);
  }

  public ModuleExp ()
  {
  }

  /** Used to control which .zip file dumps are generated. */
  public static String dumpZipPrefix;
  public static int dumpZipCounter;

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
	if (messages.seenErrors())
	  return null;

	byte[][] classes = new byte[comp.numClasses][];

	java.util.zip.ZipOutputStream zout = null;
	if (dumpZipPrefix != null)
	  {
	    StringBuffer zipname = new StringBuffer(dumpZipPrefix);
	    if (dumpZipCounter >= 0)
	      zipname.append(++dumpZipCounter);
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

	Class clas = loader.loadClass (class_name, true);
	/* Pass literal values to the compiled code. */
	for (Literal init = comp.litTable.literalsChain;  init != null;
	     init = init.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("init["+init.index+"]=");
	    SFormat.print(init.value, out);
	    out.println();
	    */
	    try
	      {
		clas.getDeclaredField(init.field.getName())
		  .set(null, init.value);
	      }
	    catch (java.lang.NoSuchFieldException ex)
	      {
		throw new Error("internal error - "+ex);
	      }
	  }
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
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("class illegal access: in lambda eval");
      }
  }

  public final static void evalModule (Environment env, CallContext ctx, Compilation comp) throws Throwable
  {
    ModuleExp mexp = comp.getModule();
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);

	if (debugPrintExpr)
	  {
	    OutPort dout = OutPort.outDefault();
	    dout.println ("[Evaluating module \""+mexp.getName()+"\" mustCompile="+mexp.mustCompile+':');
	    mexp.print(dout);
	    dout.println(']');
	    dout.flush();
	  }

	if (! mexp.mustCompile) // optimization - don't generate unneeded Class.
	  mexp.body.eval (env, ctx);
	else
	  {
	    ModuleBody mod;
	    try
	      {
		Class clas = evalToClass(comp);
		if (clas == null)
		  return;
		Object inst = clas.newInstance ();
		
		Procedure proc = (Procedure) inst;
		if (proc.getName() == null)
		  proc.setName (mexp.name);
		//thisValue = proc;
		mod = (ModuleBody) inst;
	      }
	    catch (InstantiationException ex)
	      {
		throw new RuntimeException("class not instantiable: in lambda eval");
	      }
	    catch (IllegalAccessException ex)
	      {
		throw new RuntimeException("class illegal access: in lambda eval");
	      }
	    gnu.kawa.reflect.ClassMemberConstraint.defineAll(mod, env);
	    ctx.proc = mod;
	  }
	ctx.runUntilDone();
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
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

  void allocFields (Compilation comp)
  {
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if ((decl.isSimple() && ! decl.isPublic()) || decl.field != null)
	  continue;
	if (decl.getFlag(Declaration.IS_SYNTAX)
	    && ((kawa.lang.Macro) decl.getConstantValue()).expander instanceof LambdaExp
	    && ! decl.isPrivate())
	  continue;  // Handled in SetExp.
	Expression value = decl.getValue();
	if (value instanceof LambdaExp && ! (value instanceof ClassExp))
	  {
	    ((LambdaExp) value).allocFieldFor(comp);
	  }
	else
	  {
	    if (! (value instanceof QuoteExp)
		|| ! decl.getFlag(Declaration.IS_CONSTANT) || comp.immediate)
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
    if (name != null)
      {
	out.print(name);
	out.print('/');
      }
    out.print(id);
    out.print('/');
    out.writeSpaceFill();
    out.startLogicalBlock("(", false, ")");
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	out.print(decl);
	out.writeSpaceFill();
      }
    out.endLogicalBlock(")");
    out.writeSpaceLinear();
    if (body == null)
      out.print("<null body>");
    else
      body.print (out);
    out.endLogicalBlock(")");
  }
}
