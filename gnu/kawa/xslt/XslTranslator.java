// Copyright (c) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xslt;
import gnu.lists.*;
import gnu.math.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.mapping.*;
import java.util.Stack;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.xquery.lang.*;
import gnu.kawa.functions.AppendValues;

/** Translate an XSLT stylesheet to a Kawa Expression tree. */

public class XslTranslator extends Lexer implements Consumer
{
  boolean inTemplate;
  Declaration consumerDecl;
  StringBuffer nesting = new StringBuffer(100);
  ModuleExp mexp;
  Compilation comp;

  /** We seen a beginAttribute but not the closing endAttribute. */
  boolean inAttribute;
  /** The 'attribute type' from the most recent beginAttribute. */
  Object attributeType;
  /** Buffer to acumulate the value of the current attribute. */
  StringBuffer attributeValue = new StringBuffer(100);

  XSLT interpreter;
  XMLParser parser;

  /** Non-null if we're inside an xsl:template. */
  LambdaExp templateLambda;

  XslTranslator(InPort inp, gnu.text.SourceMessages messages,
		XSLT interpreter)
  {
    super(inp, messages);
    this.interpreter = interpreter;
    parser = new XMLParser(inp, messages, this);
  }

  static final String XSL_TRANSFORM_URI
  = "http://www.w3.org/1999/XSL/Transform";

  public String popMatchingAttribute(String ns, String name, int start)
  {
    int size = comp.exprStack.size();
    for (int i = start; i < size;  i++)
      {
	Object el = comp.exprStack.elementAt(start);
	if (! (el instanceof ApplyExp))
	  return null;
	ApplyExp aexp = (ApplyExp) el;
	Expression function = aexp.getFunction();
	if (aexp.getFunction() != MakeAttribute.makeAttributeExp)
	  return null;
	Expression[] args = aexp.getArgs();
	if (args.length != 2)
	  return null;
	Expression arg0 = args[0];
	if (! (arg0 instanceof QuoteExp))
	  return null;
	Object tag = ((QuoteExp) arg0).getValue();
	if (! (tag instanceof Symbol))
	  return null;
	Symbol stag = (Symbol) tag;
	if (stag.getLocalPart() == name && stag.getNamespaceURI() == ns)
	  {
	    comp.exprStack.removeElementAt(i);
	    return (String) ((QuoteExp) args[1]).getValue();
	  }
      }
    return null;
  }

  Expression popTemplateBody(int start)
  {
    // should strip off attributes?
    int i = comp.exprStack.size() - start;
    Expression exp;
    Expression[] args = new Expression[i];
    while (--i >= 0)
      args[i] = (Expression) comp.exprStack.pop();
    return new ApplyExp(AppendValues.appendValues, args);
  }

  public static String isXslTag (Object type)
  {
    if (type instanceof QuoteExp)
      type = ((QuoteExp) type).getValue();
    if (! (type instanceof Symbol))
      return null;
    Symbol qname = (Symbol) type;
    if (qname.getNamespaceURI() != XSL_TRANSFORM_URI)
      return null;
    return qname.getLocalName();
  }

  void append(Expression expr)
  {
    // FIXME
  }

  public void beginGroup(Object type)
  { 
    String xslTag = isXslTag(type);
    if (xslTag == "template")
      {
	if (templateLambda != null)
	  error("nested xsl:template");
	templateLambda = new LambdaExp();
	//templateLambda.setFile(getName());
	//templateLambda.setLine(declLine, declColumn);
      }
    if (type instanceof XName)
      {
	// This gets rid of namespace "nodes".   That's not really right.
	// We do want to get rid of xmlns:xsl, though, at least.  FIXME.
	XName xn = (XName) type;
	type = Symbol.make(xn.getNamespaceURI(), xn.getLocalPart(),
                           xn.getPrefix());
      }
    nesting.append((char) comp.exprStack.size());
    push(type);
  }

  public void beginAttribute(Object attrType)
  {
    if (inAttribute)
      error('f', "internal error - attribute inside attribute");
    attributeType = attrType;
    attributeValue.setLength(0);
    nesting.append((char) comp.exprStack.size());
    inAttribute = true;
  }

  public void endAttribute()
  {
    Expression[] args = new Expression[2];
    args[0] = new QuoteExp(attributeType);
    args[1] = new QuoteExp(attributeValue.toString());
    push(new ApplyExp(MakeAttribute.makeAttributeExp, args));
    nesting.setLength(nesting.length()-1);
    inAttribute = false;
  }

  public void endGroup()
  {
    int nlen = nesting.length()-1;
    int start = nesting.charAt(nlen);
    nesting.setLength(nlen);
    Expression startTag = (Expression) comp.exprStack.elementAt(start);
    String xslTag = isXslTag(startTag);
    if (xslTag == "value-of")
      {
	String select = popMatchingAttribute("", "select", start + 1);
	if (select != null)
	  {
	    Expression exp = interpreter.parseXPath(select, getMessages());
	    exp = new ApplyExp(ClassType.make("gnu.kawa.xml.StringValue")
                               .getDeclaredMethod("stringValue", 1),
                               new Expression[] { exp });
	    comp.exprStack.pop();
	    push(exp);
	    return;
	  }
      }
    else if (xslTag == "apply-templates")
      {
	String select = popMatchingAttribute("", "select", start + 1);
	String mode = popMatchingAttribute("", "mode", start + 1);
	Expression[] args
	  = { new QuoteExp(select), resolveQNameExpression(mode) };
	comp.exprStack.pop();
	push(new ApplyExp(new QuoteExp(applyTemplatesProc), args));
      }
    else if (xslTag == "if")
      {
	String select = popMatchingAttribute("", "test", start + 1);
	Expression test = interpreter.parseXPath(select, getMessages());
	test = XQParser.booleanValue(test);
	Expression clause = popTemplateBody(start+1);
	comp.exprStack.pop();
	push(new IfExp(test, clause, QuoteExp.voidExp));
      }
    else if (xslTag == "stylesheet" || xslTag == "transform")
      {
	push(new ApplyExp(new QuoteExp(runStylesheetProc),
			  Expression.noExpressions));
	Expression body = popTemplateBody(start+1);
	push(body);
	mexp.body = body;
      }
    else if (xslTag == "template")
      {
	String match = popMatchingAttribute("", "match", start + 1);
	String name = popMatchingAttribute("", "name", start + 1);
	String priority = popMatchingAttribute("", "priority", start + 1);
	String mode = popMatchingAttribute("", "mode", start + 1);
	templateLambda.body = popTemplateBody(start+1);
	comp.exprStack.pop();
	Expression[] args = new Expression[5];
	double prio = 0.0; // FIXME
	args[0] = resolveQNameExpression(name);
	args[1] = new QuoteExp(match);
	args[2] = new QuoteExp(DFloNum.make(prio));
	args[3] = resolveQNameExpression(mode);
	args[4] = templateLambda;
	push(new ApplyExp(new QuoteExp(defineTemplateProc), args));
	templateLambda = null;
      }
    else
      {
	Expression[] args = new Expression[comp.exprStack.size() - start];
	for (int i = args.length;  --i >= 0; )
	  args[i] = (Expression) comp.exprStack.pop();
	// FIXME does not preserve namespace attributes.
	Expression exp = new ApplyExp(MakeElement.makeElement, args);
	push(exp);
	mexp.body = exp;
      }
  }

  public Consumer append (char v)
  {
    if (inAttribute)
      attributeValue.append(v);
    else
      push(String.valueOf(v));
    return this;
  }

  /* #ifdef use:java.lang.CharSequence */
  public Consumer append (CharSequence csq)
  {
    if (inAttribute)
      attributeValue.append(csq);
    else
      push(csq.toString());
    return this;
  }

  public Consumer append (CharSequence csq, int start, int end)
  {
    return append(csq.subSequence(start, end));
  }
  /* #else */
  // public Consumer append (String str)
  // {
  //   if (inAttribute)
  //     attributeValue.append(str);
  //   else
  //     push(str);
  //   return this;
  // }
  /* #endif */

  void push(Expression exp)
  {
    comp.exprStack.push(exp);
  }

  void push(Object value)
  {
    push(new QuoteExp(value));
  }

  public void writeBoolean(boolean v)
  {
    if (inAttribute)
      attributeValue.append(v);
    else
      push(v ? QuoteExp.trueExp : QuoteExp.falseExp);
  }

  public void writeFloat(float v)
  {
    if (inAttribute)
      attributeValue.append(v);
    else
      push(DFloNum.make(v));
  }

  public void writeDouble(double v)
  {
    if (inAttribute)
      attributeValue.append(v);
    else
      push(DFloNum.make(v));
  }

  public void writeInt(int v)
  {
    if (inAttribute)
      attributeValue.append(v);
    else
      push(IntNum.make(v));
  }

  public void writeLong(long v)
  { 
    if (inAttribute)
      attributeValue.append(v);
    else
      push(IntNum.make(v));
  }

  public void beginDocument()
  {
    
  }

  public void beginDocument(ModuleExp mexp)
  {
    this.mexp = mexp;
    beginDocument();
  }

  public void endDocument()
  {
  }

  public void writeObject(Object v)
  {
    if (inAttribute)
      attributeValue.append(v);
    else
      push(v);
  }

  public void write(char[] buf, int off, int len)
  {
    if (inAttribute)
      attributeValue.append(buf, off, len);
    else
      push(new String(buf, off, len));
  }

  public boolean ignoring()
  {
    return false;
  }

  public Expression getExpression()
  {
    return (Expression) comp.exprStack.pop();
  }

  public void error (char kind, String message)
  {
    getMessages().error(kind, message);
  }

  /*
  public void fatal(String message) throws SyntaxException
  {
    getMessages().error('f', message);
    throw new SyntaxException(getMessages());
  }
  */

  Expression resolveQNameExpression(String name)
  {
    if (name == null)
      return QuoteExp.nullExp;
    else
      return new QuoteExp(Symbol.make(null, name)); // FIXME
  }

  public void parse (Compilation comp)
  {
    this.comp = comp;
    if (comp.exprStack == null)
      comp.exprStack = new Stack();
    ModuleExp mexp = comp.pushNewModule(this);
    comp.mustCompileHere();
    beginDocument(mexp);
    parser.parse();
    endDocument();
    comp.pop(mexp);
  }

  static final ClassType typeXSLT
    = gnu.bytecode.ClassType.make("gnu.kawa.xslt.XSLT");
  static final ClassType typeTemplateTable
    = gnu.bytecode.ClassType.make("gnu.kawa.xslt.TemplateTable");
  static final Method defineTemplateMethod
    = typeXSLT.getDeclaredMethod("defineTemplate", 5);
  static final Method runStylesheetMethod
    = typeXSLT.getDeclaredMethod("runStylesheet", 0);
  static final PrimProcedure defineTemplateProc
    = new PrimProcedure(defineTemplateMethod);
  static final PrimProcedure runStylesheetProc
    = new PrimProcedure(runStylesheetMethod);
  static final Method applyTemplatesMethod
    = typeXSLT.getDeclaredMethod("applyTemplates", 2);
  static final PrimProcedure applyTemplatesProc
    = new PrimProcedure(applyTemplatesMethod);
}
