package kawa.lang;
import java.io.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.kawa.util.*;

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
  {
    Object body = null;
    try
      {
	gnu.text.LispReader lexer = (gnu.text.LispReader)
	  Interpreter.getInterpreter().getLexer(port, messages);
	body = lexer.readListBody ();
	if (port.peek() == ')')
	  lexer.fatal("An unexpected close paren was read.");
      }
    catch (gnu.text.SyntaxException ex)
      {
        // Got a fatal error.
        if (ex.getMessages() != messages)
          throw new GenericError ("confussing syntax error: "+ex);
        // otherwise ignore it - it's already been recorded in messages.
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("I/O exception reading file: " + e.toString ());
      }
    return body;
  }

  public static final ModuleExp read (InPort port, Translator tr)
  {
    ModuleExp mexp = new ModuleExp();
    mexp.setFile(port.getName());
    java.util.Vector forms = new java.util.Vector(20);
    SourceMessages messages = tr.getMessages();
    tr.push(mexp);
    try
      {
	gnu.text.LispReader lexer = (gnu.text.LispReader) Interpreter.getInterpreter().getLexer(port, messages);
        for (;;)
          {
	    Object sexp = lexer.readObject(); // FIXME
	    if (sexp == Sequence.eofValue)
	      break;
            if (! tr.scan_form (sexp, forms, mexp))
              break;
          }
	if (port.peek() == ')')
	  lexer.fatal("An unexpected close paren was read.");
      }
    catch (gnu.text.SyntaxException ex)
      {
        // Got a fatal error.
        if (ex.getMessages() != messages)
          throw new GenericError ("confussing syntax error: "+ex);
        // otherwise ignore it - it's already been recorded in messages.
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("I/O exception reading file: " + e.toString ());
      }
    tr.finishModule(mexp, forms);
    return mexp;
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
        SourceMessages messages = tr.getMessages();
        if (messages.seenErrors())
          throw new gnu.text.SyntaxException(messages);
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
  public static void compile_to_files (String inname, String directory,
                                       String prefix, String topname,
                                       SourceMessages messages)
  {
    if (topname == null)
      {
	File infile = new File (inname);
	String short_name = infile.getName ();
	if (short_name.endsWith (".scm"))
	  short_name
	    = short_name.substring (0, short_name.length () - 4);
	else if (short_name.endsWith (".el"))
	  short_name
	    = short_name.substring (0, short_name.length () - 3);
	topname = short_name;
	if (prefix != null)
	  topname = prefix + short_name;
      }
    Translator tr = new Translator (Environment.user(), messages);
    ModuleExp mexp = read (inname, tr);
    if (messages.seenErrors())
      return;

    try
      {
	mexp.compileToFiles (topname, directory, prefix);
      }
    catch (IOException ex)
      {
        throw new GenericError (ex.toString ());
      }
  }
}
