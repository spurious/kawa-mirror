// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xslt;
import gnu.mapping.*;
import kawa.standard.Scheme;
import gnu.kawa.lispexpr.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.xquery.lang.*;
import gnu.kawa.xml.*;

/** New Kawa language XSLT (XML Stylesheet Language Tranformations). */

public class XSLT extends XQuery
{
  static XSLT instance;

  public String getName()
  {
    return "XSLT";
  }

  public XSLT ()
  {
    instance = this;
    ModuleBody.setMainPrintValues(true);
    Environment.setCurrent(getEnvironment());
  }

  public static XSLT getXsltInstance()
  {
    if (instance == null)
      new XSLT ();
    return instance;
  }

  Expression parseXPath (String string, SourceMessages messages)
  {
    try
      {
	Compilation tr = new Compilation(messages);
	XQParser lexer
	  = (XQParser) super.getLexer(new CharArrayInPort(string), messages);
	//lexer.nesting = 1;
	java.util.Vector exps = new java.util.Vector(20);
	for (;;)
	  {
	    Expression sexp = lexer.parse(tr);
	    if (sexp == null)
	      break;
	    exps.addElement(sexp);
	  }
	int nexps = exps.size();
	if (nexps == 0)
	  return QuoteExp.voidExp;
	else if (nexps == 1)
	  return (Expression) exps.elementAt(0);
	else
	  throw new InternalError("too many xpath expressions"); // FIXME
      }
    catch (Throwable ex)
      {
	ex.printStackTrace();
	throw new InternalError ("caught "+ex);
      }
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new XslTranslator(inp, messages, this);
    // return null;
  /*
    XslTranslator xtr = new XslTranslator(mexp, messages, this);
    XMLParser parser = new XMLParser(inp, new NamespaceResolver(xtr));
    return xtr;
    return null; // FIXME
  */
  }

  public Compilation parse(Environment env, Lexer lexer)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    lexer.clearErrors();
    Compilation.usingTailCalls = true;
    gnu.text.SourceMessages messages = lexer.getMessages();
    Compilation tr = new Compilation(messages);
    tr.immediate = true;
    ModuleExp mexp = new ModuleExp();
    mexp.setFile(lexer.getName());
    tr.push(mexp);
    tr.mustCompileHere();
    XslTranslator xtr = (XslTranslator) lexer;
    //XslTranslator xtr = new XslTranslator(mexp, messages, this);
    xtr.beginDocument(mexp);
    XMLParser parser
      = new XMLParser(lexer.getPort(), new NamespaceResolver(xtr), messages);
    parser.parse();
    // FIXME - need check for eof.
    xtr.endDocument();
    tr.pop(mexp);
    return tr;
  }

  public Compilation parseFile (InPort port, gnu.text.SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Compilation.usingTailCalls = true;
    Compilation tr = new Compilation(messages);
    tr.mustCompileHere();
    ModuleExp mexp = new ModuleExp();
    mexp.setFile(port.getName());
    tr.push(mexp);
    XslTranslator xtr = new XslTranslator(mexp, messages, this);
    xtr.beginDocument();
    XMLParser parser
      = new XMLParser(port, new NamespaceResolver(xtr), messages);
    parser.parse();
    xtr.endDocument();
    tr.pop(mexp);
    return tr;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    XSLT interp = new XSLT();
    Interpreter.defaultInterpreter = interp;
    Environment.setCurrent(interp.getEnvironment());
  }

  public static void defineCallTemplate(QName name,
					double priority,
					Procedure template)
  {
  }

  public static QName nullMode = QName.make(null, "");

  public static void defineApplyTemplate(String pattern,
					 double priority,
					 QName mode,
					 Procedure template)
  {
    if (mode == null)
      mode = nullMode;
    TemplateTable table = TemplateTable.getTemplateTable(mode);
    table.enter(pattern, priority, template);
  }

  public static void defineTemplate(QName name, String pattern,
				    double priority, QName mode,
				    Procedure template)
  {
    if (name != null)
      defineCallTemplate(name, priority, template);
    if (pattern != null)
      defineApplyTemplate(pattern, priority, mode, template);
  }

  public static void process(String url, CallContext ctx)
    throws Throwable
  {
    TreeList doc = Document.parse(url);
    Focus pos = Focus.getCurrent();
    pos.push(doc, 0, null);
    process(doc, pos, ctx);
  }

  public static void process(TreeList doc, Focus pos, CallContext ctx)
    throws Throwable
  {
    Consumer out = ctx.consumer;
    for (;;)
      {
	int ipos = pos.ipos;
	int kind = doc.getNextKind(ipos, null);
	switch (kind)
	  {
	  case Sequence.DOCUMENT_VALUE:
	    ipos = doc.gotoChildrenStart(ipos >> 1) << 1;
	    break;
	  case Sequence.GROUP_VALUE:
	    Object type = pos.getNextTypeObject();
	    Procedure proc = TemplateTable.nullModeTable.find(pos.getNextTypeName());
	    String name = pos.getNextTypeName();
	    if (proc != null)
	      proc.apply(ctx);
	    else
	      {
		out.beginGroup(name, type);
		// FIXME emit attributes
		pos.push(doc, doc.gotoChildrenStart(ipos >> 1) << 1, null);
		process(doc, pos, ctx);
		pos.pop();
		out.endGroup(name);
	      }
	    ipos = doc.nextDataIndex(ipos >>> 1) << 1;
	    pos.gotoNext();
	    break;
	  case Sequence.CHAR_VALUE:
	    int ichild = ipos >>> 1;
	    int next = doc.nextNodeIndex(ichild, -1 >>> 1);
	    if (ipos == next)
	      next = doc.nextDataIndex(ichild);
	    doc.consumeRange(ichild, next, out);
	    ipos = next << 1;
	    break;
	  case Sequence.TEXT_BYTE_VALUE:
	  case Sequence.OBJECT_VALUE:
	  default:
	    return;
	  }
	pos.ipos = ipos;
      }
  }

  public static void runStylesheet()
    throws Throwable
  {
    String[] args = kawa.repl.commandLineArgArray;
    String inputFileName = null;
    for (int i = 0;  i < args.length;  i++)
      {
	String arg = args[i];
	CallContext ctx = CallContext.getInstance();
	inputFileName = arg;
	process(arg, ctx);
      }
  }

  public static void applyTemplates(String select, QName mode)
    throws Throwable
  {
    if (mode == null)
      mode = nullMode;
    TemplateTable table = TemplateTable.getTemplateTable(mode);
    CallContext ctx = CallContext.getInstance();
    Focus pos = Focus.getCurrent();
    TreeList doc = (TreeList) pos.sequence;
    pos.push(doc, doc.gotoChildrenStart(pos.ipos >> 1) << 1, null);
    process(doc, pos, ctx);
    pos.pop();
  }
}
