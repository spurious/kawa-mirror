package gnu.expr;
import java.io.*;
import gnu.mapping.*;
import gnu.bytecode.*;

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

  public ModuleExp ()
  {
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
	return ((ModuleBody) eval (env)).run (env);
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public void compileToFiles (String topname, String directory, String prefix)
    throws java.io.IOException
  {
    if (directory == null || directory.length() == 0)
      directory = "";
    else if (directory.charAt(directory.length() - 1) != '/')
      directory = directory + '/';
    Compilation comp = new Compilation(this, topname, prefix, false);
    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
      {
	ClassType clas = comp.classes[iClass];
	String out_name
	  = directory + clas.getName().replace('.', '/') + ".class";
	clas.writeToFile(out_name);
      }
  }

  public void compileToArchive (String fname)
    throws java.io.IOException
  {
    if (! fname.endsWith(".zip") && ! fname.endsWith(".jar"))
      fname = fname + ".zip";
    Compilation comp = new Compilation(this, LambdaExp.fileFunctionName,
				       null, false);
    File zar_file = new File (fname);
    if (zar_file.exists ())
      zar_file.delete ();
    ZipArchive zar = new ZipArchive (zar_file, "rw");

    byte[][] classes = new byte[comp.numClasses][];
    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
      {
	ClassType clas = comp.classes[iClass];
	classes[iClass] = clas.writeToArray ();

	zar.append (clas.getName ().replace ('.', '/') + ".class",
		    classes[iClass]);
      }
    zar.close ();
  }

}
