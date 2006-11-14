// Copyright (C) 2000 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;
import java.io.File;
import gnu.mapping.*;
import gnu.kawa.lispexpr.*;

public class define_autoload extends Syntax
{
  public static final define_autoload define_autoload
    = new define_autoload("define-autoload", false);
  public static final define_autoload define_autoloads_from_file
    = new define_autoload("define-autoloads-from-file", true);

  boolean fromFile;

  public define_autoload (String name, boolean fromFile)
  {
    super(name);
    this.fromFile = fromFile;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    if (fromFile)
      {
	for (;;)
	  {
	    if (! (p.car instanceof FString))
	      break;
	    if (! scanFile(p.car.toString(), defs, tr))
	      return false;
	    Object rest = p.cdr;
	    if (rest == LList.Empty)
	      return true;
	    if (! (rest instanceof Pair))
	      break;
	    p = (Pair) p.cdr;
	  }
	tr.syntaxError("invalid syntax for define-autoloads-from-file");
	return false;
      }
    Object names = p.car;
    Object filename = null;
    if (p.cdr instanceof Pair)
      {
	p = (Pair) p.cdr;
	filename = p.car;
	return process(names, filename, forms, defs, tr);
      }
    tr.syntaxError("invalid syntax for define-autoload");
    return false;
  }

  public boolean scanFile(String filespec, ScopeExp defs, Translator tr)
  {
    if (filespec.endsWith(".el"))
      ;
    File file = new File(filespec);
    if (! file.isAbsolute())
      file = new File(new File(tr.getFileName()).getParent(), filespec);
    String filename = file.getPath();
    int dot = filename.lastIndexOf('.');
    Language language;
    if (dot >= 0)
      {
	String extension = filename.substring(dot);
	language = Language.getInstance(extension);
	if (language == null)
	  {
	    tr.syntaxError("unknown extension for "+filename);
	    return true;
	  }
	gnu.text.Lexer lexer;

	/*
	// Since (module-name ...) is not handled in this pass,
	// we won't see it until its too late.  FIXME.
	ModuleExp module = tr.getModule();
	String prefix = module == null ? null : module.getName();
	System.err.println("module:"+module);
	if (prefix == null)
	  prefix = kawa.repl.compilationPrefix;
	else
	  {
	    int i = prefix.lastIndexOf('.');
	    if (i < 0)
	      prefix = null;
	    else
	      prefix = prefix.substring(0, i+1);
	  }
	*/
	String prefix = tr.classPrefix;
	int extlen = extension.length();
	int speclen = filespec.length();
	String cname = filespec.substring(0, speclen - extlen);
	while (cname.startsWith("../"))
	  {
	    int i = prefix.lastIndexOf('.', prefix.length() - 2);
	    if (i < 0)
	      {
		tr.syntaxError("cannot use relative filename \"" + filespec
			       + "\" with simple prefix \"" + prefix + "\"");
		return false;
	      }
	    prefix = prefix.substring(0, i+1);
	    cname = cname.substring(3);
	  }
	String classname = (prefix + cname).replace('/', '.');

	try
	  {
	    InPort port = InPort.openFile(filename);
	    lexer = language.getLexer(port, tr.getMessages());
	    findAutoloadComments((LispReader) lexer, classname, defs, tr);
	  }
	catch (Exception ex)
	  {
	    tr.syntaxError("error reading "+filename+": "+ex);
	    return true;
	  }
      }
    return true;
  }

  public static void findAutoloadComments (LispReader in, String filename,
					   ScopeExp defs, Translator tr)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    boolean lineStart = true;
    String magic = ";;;###autoload";
    int magicLength = magic.length();
  mainLoop:
    for (;;)
      {
	int ch = in.peek();
	if (ch < 0)
	  return;
	if (ch == '\n' || ch == '\r')
	  {
	    in.read();
	    lineStart = true;
	    continue;
	  }
	if (lineStart && ch == ';')
	  {
	    int i = 0;
	    for (;;)
	      {
		if (i == magicLength)
		  break;
		ch = in.read();
		if (ch < 0)
		  return;
		if (ch == '\n' || ch == '\r')
		  {
		    lineStart = true;
		    continue mainLoop;
		  }
		if (i < 0 || ch == magic.charAt(i++))
		  continue;
		i = -1;  // No match.
	      }
	    if (i > 0)
	      {
		Object form = in.readObject();
		if (form instanceof Pair)
		  {
		    Pair pair = (Pair) form;
		    Object value = null;
		    String name = null;
		    Object car = pair.car;
		    String command
		      = car instanceof String ? car.toString()
		      : car instanceof Symbol ? ((Symbol) car).getName()
		      : null;
		    if (command == "defun")
		      {
			name = ((Pair)pair.cdr).car.toString();
			value = new AutoloadProcedure(name, filename,
						      tr.getLanguage());
		      }
		    else
		      tr.error('w', "unsupported ;;;###autoload followed by: "
			       + pair.car);
		    if (value != null)
		      {
			Declaration decl = defs.getDefine(name, 'w', tr);
			Expression ex = new QuoteExp(value);
			decl.setFlag(Declaration.IS_CONSTANT);
			decl.noteValue(ex);
			decl.setProcedureDecl(true);
			decl.setType(Compilation.typeProcedure);
		      }
		  }
		lineStart = false;
		continue;
	      }
	  }
	lineStart = false;
	in.skip();
	if (ch == '#')
	  {
	    if (in.peek() == '|')
	      {
		in.skip();
		in.readNestedComment('#', '|');
		continue;
	      }
	  }
	if (Character.isWhitespace ((char)ch))
	  ;
	else
	  {
	    Object skipped = in.readObject(ch);
	    if (skipped == Sequence.eofValue)
	      return;
	  }
      }
  }

  public static boolean process(Object names, Object filename,
			       java.util.Vector forms,
			       ScopeExp defs, Translator tr)
  {
    if (names instanceof Pair)
      {
	Pair p = (Pair) names;
	return (process(p.car, filename, forms, defs, tr)
		&& process(p.cdr, filename, forms, defs, tr));
      }
    if (names == LList.Empty)
      return true;
    String fn;
    int len;

    /*
    if (names == "*" && filename instanceof String
	&& (len = (fn = (String) filename).length()) > 2
	&& fn.charAt(0) == '<' && fn.charAt(len-1) == '>')
      {
	fn = fn.substring(1, len-1);
	try
	  {
	    Class fclass = Class.forName(fn);
	    Object instance = require.find(ctype, env);
	    ...;
	  }
	catch (ClassNotFoundException ex)
	  {
	    tr.syntaxError("class <"+fn+"> not found");
	    return false;
	  }
      }
    */

    if (names instanceof String || names instanceof Symbol)
      {
	String name = names.toString();
	Declaration decl = defs.getDefine(name, 'w', tr);
	if (filename instanceof String
	    && (len = (fn = (String) filename).length()) > 2
	    && fn.charAt(0) == '<' && fn.charAt(len-1) == '>')
	  filename = fn.substring(1, len-1);
	Object value = new AutoloadProcedure(name, filename.toString(),
					     tr.getLanguage());
	Expression ex = new QuoteExp(value);
	decl.setFlag(Declaration.IS_CONSTANT);
	decl.noteValue(ex);
	return true;
      }
    return false;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
