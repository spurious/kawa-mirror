package kawa.lang;
import java.io.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;

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

  public static final ModuleExp read (String name, Translator tr)
    throws gnu.text.SyntaxException
  {
    try
      {
	InPort fstream = InPort.openFile(name);
	ModuleExp result = read(fstream, tr);
	fstream.close();
	return result;
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new GenericError ("compile-file: file not found: " + name);
      }
    catch (java.io.IOException e)
      {
	throw new GenericError(name.toString());
      }
  }

  public static final Object readBody (InPort port, SourceMessages messages)
    throws gnu.text.SyntaxException
  {
    Object body;
    try
      {
	ScmRead lexer = new ScmRead(port, messages);
	body = lexer.readListBody ();
	if (messages.seenErrors())
	  throw new gnu.text.SyntaxException(lexer.getMessages());
	if (port.peek() == ')')
	  lexer.fatal("An unexpected close paren was read.");
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("I/O exception reading file: " + e.toString ());
      }
    return body;
  }

  public static final ModuleExp read (InPort port, Translator tr)
    throws gnu.text.SyntaxException
  {
    return kawa.standard.Scheme.makeModuleExp(readBody(port, tr.getMessages()),
					      tr);
  }

  public final Object apply2 (Object arg1, Object arg2)
  {
    if (! (arg1 instanceof FString))
      throw new WrongType (this.name (), 1, "file name");
    Translator tr = new Translator ();
    ModuleExp lexp;
    try
      {
	lexp = read (arg1.toString (), tr);
      }
    catch (gnu.text.SyntaxException e)
      {
	// The '\n' is because a SyntaxException includes a line number,
	// and it is better if that starts the line.  FIXME OBSOLETE
	throw new GenericError ("read error reading file:\n" + e.toString ());
      }
    try
      {
	lexp.compileToArchive(arg2.toString());
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
   * @return true iff there were syntax errors
   */
  public static boolean compile_to_files (String inname, String directory,
					  String prefix, String topname)
    throws gnu.text.SyntaxException
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
    SourceMessages messages = new SourceMessages();
    Translator tr = new Translator (Environment.user(), messages);
    ModuleExp mexp = read (inname, tr);
    if (messages.checkErrors(OutPort.errDefault(), 50))
      throw new gnu.text.SyntaxException(messages);

    try
      {
	mexp.compileToFiles (topname, directory, prefix);
      }
    catch (IOException ex)
      {
        throw new GenericError (ex.toString ());
      }
    return false;
  }
}
