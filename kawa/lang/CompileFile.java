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

  public final Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof StringBuffer))
      throw new WrongType (this.name, 1, "file name");
    String name = arg1.toString ();
    FileInputStream fstream;
    try
      {
	fstream = new FileInputStream (name);
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new GenericError ("load: file not found: " + name);
      }

    InPort port = new InPort (fstream);
    Interpreter interpreter = Interpreter.current ();
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
	catch (SyntaxError e)
	  {
	    throw new GenericError ("syntax error in load: " + e.toString ());
	  }
	catch (java.io.IOException e)
	  {
	    throw new GenericError ("I/O exception in load: " + e.toString ());
	  }
	Pair cur = new Pair (obj, List.Empty);
	if (last == null)
	  body = cur;
	else
	  last.cdr = cur;
	last = cur;

      }

    LambdaExp lexp = new LambdaExp (List.Empty, body, interpreter);
    Compilation comp = new Compilation (lexp, "lambda0", false);

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
	    classes[iClass] = comp.classes[iClass].emit_to_array ();

	    zar.append ("lambda" + iClass + ".class", classes[iClass]);
	  }
	zar.close ();
      }
    catch (IOException ex)
      {
	throw new GenericError (ex.toString ());
      }

    return Interpreter.voidObject;
  }
}
