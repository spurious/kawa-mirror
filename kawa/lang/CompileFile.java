package kawa.lang;
import codegen.*;
import java.io.*;

/** Procedure to read and compile and entire file.
 * Creates a .zip archive containing the resulting classes.
 * @author	Per Bothner
 */

public class CompileFile extends Procedure2
{
  public CompileFile ()
  {
    super ("compile-file");
  }

  public static final LambdaExp read (String name, Interpreter interpreter)
       throws GenericError
  {
    FileInputStream fstream;
    try
      {
	fstream = new FileInputStream (name);
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new GenericError ("compile-file: file not found: " + name);
      }

    InPort port = new InPort (fstream, name);
    Environment env = new Environment (interpreter);

    List body = List.Empty;
    Pair last = null;

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
	    throw new GenericError ("read error in compile-file:\n" + e.toString ());
	  }
	catch (SyntaxError e)
	  {
	    throw new GenericError ("syntax error in compile-file: " + e.toString ());
	  }
	catch (java.io.IOException e)
	  {
	    throw new GenericError ("I/O exception in compile-file: " + e.toString ());
	  }
	Pair cur = new Pair (obj, List.Empty);
	if (last == null)
	  body = cur;
	else
	  last.cdr = cur;
	last = cur;

      }

    try
      {
	LambdaExp lexp = new LambdaExp (List.Empty, body, interpreter);
	lexp.setModuleBody (true);
	return lexp;
      }
    catch (WrongArguments ex)
      {
	throw new IllegalArgumentException (ex.toString ());
      }
  }

  public final Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof StringBuffer))
      throw new WrongType (this.name (), 1, "file name");
    Interpreter interpreter = Interpreter.current ();
    LambdaExp lexp = read (arg1.toString (), interpreter);
    Compilation comp = new Compilation (lexp, "Top", false);

    try
      {
	String fname = arg2.toString ();
	if (! fname.endsWith (".zip"))
	  fname = fname + ".zip";
	File zar_file = new File (fname);
	if (zar_file.exists ())
	  zar_file.delete ();
	ZipArchive zar = new ZipArchive (zar_file, "rw");

	byte[][] classes = new byte[comp.numClasses][];
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    classes[iClass] = clas.emit_to_array ();

	    zar.append (clas.getClassName ().replace ('.', '/') + ".class",
			classes[iClass]);
	  }
	zar.close ();
      }
    catch (IOException ex)
      {
	throw new GenericError (ex.toString ());
      }

    return Interpreter.voidObject;
  }

  /** Compile a Scheme source file to one or more .class file.
   * @param inname name of the Scheme source file
   * @param directory where to place the .class files
   * @param topname name for the class of the .class for the top-level code.
   *  If null, topname is derived from prefix and inname.
   * @param prefix to prepend classnames for functions
   */
  public static void compile_to_files (String inname, String directory,
				String prefix, String topname)
       throws GenericError
  {
    if (topname == null)
      {
	File infile = new File (inname);
	String short_name = infile.getName ();
	if (short_name.endsWith (".scm"))
	  short_name
	    = short_name.substring (0, short_name.length () - 4);
	topname = short_name;
	if (prefix != null)
	  topname = prefix + short_name;
      }
    Interpreter interpreter = Interpreter.current ();
    int save_errors = interpreter.errors;
    LambdaExp lexp = read (inname, interpreter);
    if (interpreter.errors > save_errors)
      return;
    Compilation comp = new Compilation (lexp, topname, prefix);
    if (directory == null || directory.length () == 0)
      directory = "";
    else if (directory.charAt (directory.length () - 1) != '/')
      directory = directory + '/';

    try
      {
        for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
          {
            ClassType clas = comp.classes[iClass];
	    String out_name
	      = directory +
	      clas.getClassName ().replace ('.', '/') + ".class";
	    clas.emit_to_file (out_name);
          }
      }
    catch (IOException ex)
      {
        throw new GenericError (ex.toString ());
      }
  }
}
