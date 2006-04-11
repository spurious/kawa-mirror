// Copyright (c) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xslt;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.xquery.lang.*;
import gnu.kawa.xml.*;

/** New Kawa language XSLT (XML Stylesheet Language Tranformations). */

public class XSLT extends XQuery
{
  // This field need to be public so that the findLiteral method in
  // gnu.expr.LitTable can find it.
  public static XSLT instance;

  public String getName()
  {
    return "XSLT";
  }

  public XSLT ()
  {
    instance = this;
    ModuleBody.setMainPrintValues(true);
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
	Compilation tr = new Compilation(this, messages);
	XQParser parser
	  = (XQParser) super.getLexer(new CharArrayInPort(string), messages);
	//parser.nesting = 1;
	java.util.Vector exps = new java.util.Vector(20);
	for (;;)
	  {
	    Expression sexp = parser.parse(tr);
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
  }

  public Compilation parse(Lexer lexer, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    Compilation comp = new Compilation(this, lexer.getMessages());
    comp.immediate = (options & PARSE_IMMEDIATE) != 0;
    ((XslTranslator) lexer).parse(comp);
    return comp;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(new XSLT());
  }

  public static void defineCallTemplate(Symbol name,
					double priority,
					Procedure template)
  {
  }

  public static Symbol nullMode = Symbol.make(null, "");

  public static void defineApplyTemplate(String pattern,
					 double priority,
					 Symbol mode,
					 Procedure template)
  {
    if (mode == null)
      mode = nullMode;
    TemplateTable table = TemplateTable.getTemplateTable(mode);
    table.enter(pattern, priority, template);
  }

  public static void defineTemplate(Symbol name, String pattern,
				    double priority, Symbol mode,
				    Procedure template)
  {
    if (name != null)
      defineCallTemplate(name, priority, template);
    if (pattern != null)
      defineApplyTemplate(pattern, priority, mode, template);
  }

  public static void process(TreeList doc, Focus pos, CallContext ctx)
    throws Throwable
  {
    Consumer out = ctx.consumer;
    for (;;)
      {
	int ipos = pos.ipos;
	int kind = doc.getNextKind(ipos);
	switch (kind)
	  {
	  case Sequence.DOCUMENT_VALUE:
	    ipos = doc.firstChildPos(ipos);
	    break;
	  case Sequence.GROUP_VALUE:
	    Object type = pos.getNextTypeObject();
	    Procedure proc = TemplateTable.nullModeTable.find(pos.getNextTypeName());
	    String name = pos.getNextTypeName();
	    if (proc != null)
	      {
		proc.check0(ctx);
		ctx.runUntilDone();
	      }
	    else
	      {
		out.beginGroup(name, type);
		// FIXME emit attributes
		pos.push(doc, doc.firstChildPos(ipos));
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
	    doc.consumeIRange(ichild, next, out);
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
    CallContext ctx = CallContext.getInstance();
    String[] args = kawa.repl.commandLineArgArray;
    for (int i = 0;  i < args.length;  i++)
      {
	String arg = args[i];
	KDocument doc = Document.parse(arg);
	Focus pos = Focus.getCurrent();
	pos.push(doc.sequence, doc.ipos);
	process((TreeList) doc.sequence, pos, ctx);
      }
  }

  public static void applyTemplates(String select, Symbol mode)
    throws Throwable
  {
    if (mode == null)
      mode = nullMode;
    TemplateTable table = TemplateTable.getTemplateTable(mode);
    CallContext ctx = CallContext.getInstance();
    Focus pos = Focus.getCurrent();
    TreeList doc = (TreeList) pos.sequence;
    pos.push(doc, doc.firstChildPos(pos.ipos));
    process(doc, pos, ctx);
    pos.pop();
  }
}
