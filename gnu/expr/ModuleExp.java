package gnu.expr;
import java.io.*;
import java.util.zip.*;
// import java.util.jar.*; // Java2
import gnu.mapping.*;
import gnu.bytecode.*;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends ClassExp
{
  /** True if the body is too complex to evaluate,and we must compile it.
   * This is because it contains a construct we know how to compile, but not
   * evaluate, and it it outside a lambda (which we always compile).
   * This can be a let scope, or primitive procedure. */
  public boolean mustCompile;

  public static final int EXPORT_SPECIFIED = LambdaExp.NEXT_AVAIL_FLAG;
  public static final int STATIC_SPECIFIED = EXPORT_SPECIFIED << 1;
  public static final int NONSTATIC_SPECIFIED = STATIC_SPECIFIED << 1;
  public static final int SUPERTYPE_SPECIFIED = NONSTATIC_SPECIFIED << 1;

  public ModuleExp ()
  {
  }

  public Object eval (Environment env)
  {
    //if (thisValue != null)
    /// return thisValue;
    try
      {
        Class clas = evalToClass();
	Object inst = clas.newInstance ();

	Procedure proc = (Procedure) inst;
	if (proc.getName() == null)
	  proc.setName (this.name);
        //thisValue = proc;
	return inst;
      }
    catch (InstantiationException ex)
      {
	throw new RuntimeException("class not instantiable: in lambda eval");
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("class illegal access: in lambda eval");
      }
  }

  /** Used to control which .zip file dumps are generated. */
  public static String dumpZipPrefix;
  public static int dumpZipCounter;

  ///** A cache if this has already been evaluated. */
  //Procedure thisValue;

  public Class evalToClass ()
  {
    try
      {
	String class_name = getJavaName ();

	Compilation comp = new Compilation (this, class_name, null, true);

	byte[][] classes = new byte[comp.numClasses][];
	String[] classNames = new String[comp.numClasses];
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    classNames[iClass] = clas.getName ();
	    classes[iClass] = clas.writeToArray ();
	  }
	if (dumpZipPrefix != null)
	  {
	    StringBuffer zipname = new StringBuffer(dumpZipPrefix);
	    if (dumpZipCounter >= 0)
	      zipname.append(++dumpZipCounter);
	    zipname.append(".zip");
	    java.io.FileOutputStream zfout
	      = new java.io.FileOutputStream(zipname.toString());
	    java.util.zip.ZipOutputStream zout
	      = new java.util.zip.ZipOutputStream(zfout);
	    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	      {
		String clname
		  = classNames[iClass].replace ('.', '/') + ".class";
		java.util.zip.ZipEntry zent
		  = new java.util.zip.ZipEntry (clname);
		zent.setSize(classes[iClass].length);
		java.util.zip.CRC32 crc = new java.util.zip.CRC32();
		crc.update(classes[iClass]);
		zent.setCrc(crc.getValue());
		zent.setMethod(java.util.zip.ZipEntry.STORED);
		zout.putNextEntry(zent);
		zout.write(classes[iClass]);
	      }
	    zout.close ();
	  }

	/* DEBUGGING:
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  ClassTypeWriter.print(comp.classes[iClass], System.out, 0);
	*/

	ArrayClassLoader loader = new ArrayClassLoader (classNames, classes);
	Class clas = loader.loadClass (class_name, true);
	/* Pass literal values to the compiled code. */
	for (Literal init = comp.literalsChain;  init != null;
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

  public final Object evalModule (Environment env)
  {
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
	if (! mustCompile) // optimization - don't generate unneeded Class.
	  return body.eval (env);
	ModuleBody mod = (ModuleBody) eval (env);
	gnu.kawa.reflect.ClassMemberConstraint.defineAll(mod, env);
	return mod.run();
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
	if (value instanceof LambdaExp)
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

  public void compileToFiles (String topname, String directory, String prefix)
    throws java.io.IOException
  {
    if (directory == null || directory.length() == 0)
      directory = "";
    else if (directory.charAt(directory.length() - 1) != '/')
      directory = directory + '/';
    String name = getName();
    if (name != null)
      {
	topname = name;
	if (prefix == null)
	  {
	    int index = name.lastIndexOf('.');
	    if (index >= 0)
	      prefix = name.substring(0, index+1);
	  }
      }
    Compilation comp = new Compilation(this, topname, prefix, false);
    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
      {
	ClassType clas = comp.classes[iClass];
	String out_name
	  = (directory + clas.getName().replace('.', File.separatorChar)
	     + ".class");
	String parent = new File(out_name).getParent();
	if (parent != null)
	  new File(parent).mkdirs();
	clas.writeToFile(out_name);
      }
  }

  public void compileToArchive (String fname)
    throws java.io.IOException
  {
    boolean makeJar = false;
    if (fname.endsWith(".zip"))
      makeJar = false;
    else if (fname.endsWith(".jar"))
      makeJar = true;
    else
      {
	fname = fname + ".zip";
	makeJar = false;
      }
    Compilation comp = new Compilation(this, LambdaExp.fileFunctionName,
				       null, false);
    File zar_file = new File (fname);
    if (zar_file.exists ())
      zar_file.delete ();
    ZipOutputStream zout;
    /* Java2:
    if (makeJar)
      zout = new JarOutputStream (new FileOutputStream (zar_file));
    else
    */
      {
	zout = new ZipOutputStream (new FileOutputStream (zar_file));
	zout.setMethod(zout.STORED); // no compression
      }

    byte[][] classes = new byte[comp.numClasses][];
    CRC32 zcrc = new CRC32();
    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
      {
	ClassType clas = comp.classes[iClass];
	classes[iClass] = clas.writeToArray ();
	ZipEntry zent = new ZipEntry(clas.getName ().replace ('.', '/')
				     + ".class");

	zent.setSize(classes[iClass].length);
	zcrc.reset();
	zcrc.update(classes[iClass], 0, classes[iClass].length);
	zent.setCrc(zcrc.getValue());

	zout.putNextEntry (zent);
	zout.write (classes[iClass]);
      }
    zout.close ();
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkModuleExp(this);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%module/");
    if (name != null)
      {
	ps.print(name);
	ps.print('/');
      }
    ps.print(id);
    ps.println("/ (");
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	ps.print("  ");
	ps.println(decl);
      }
    ps.print(") ");
    if (body == null)
      ps.print("<null body>");
    else
      body.print (ps);
    ps.print(")");
  }
}
