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
  Stack elements = new Stack();
  StringBuffer nesting = new StringBuffer(100);
  ModuleExp mexp;
  String attributeName;
  Object attributeType;
  AttributeConstructor attributeConstructor;
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
    int size = elements.size();
    for (int i = start; i < size;  i++)
      {
	Object el = elements.elementAt(start);
	if (! (el instanceof ApplyExp))
	  return null;
	ApplyExp aexp = (ApplyExp) el;
	Expression function = aexp.getFunction();
	if (! (function instanceof QuoteExp))
	  return null;
	Object fun = ((QuoteExp) function).getValue();
	if (! (fun instanceof AttributeConstructor))
	  return null;
	AttributeConstructor attrCon = (AttributeConstructor) fun;
	Symbol qname = attrCon.getQName();
	if (qname.getLocalName() == name &&
	    qname.getNamespaceURI() == ns)
	  {
	    elements.removeElementAt(i);
	    return (String) ((QuoteExp) aexp.getArgs()[0]).getValue();
	  }
      }
    return null;
  }

  Expression popTemplateBody(int start)
  {
    // should strip off attributes?
    int i = elements.size() - start;
    Expression exp;
    Expression[] args = new Expression[i];
    while (--i >= 0)
      args[i] = (Expression) elements.pop();
    return new ApplyExp(AppendValues.appendValues, args);
  }

  public static String isXslTag (Object type)
  {
    if (type instanceof QuoteExp)
      type = ((QuoteExp) type).getValue();
    if (type instanceof XName)
      type = ((XName) type).getQName();
    else if (type instanceof ElementConstructor)
      type = ((ElementConstructor) type).getQName();
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

  public void beginGroup(String typeName, Object type)
  { 
    String xslTag = isXslTag(type);
    if (xslTag == "template")
      {
	///if (templateLambda != null)
	// error();
	templateLambda = new LambdaExp();
	//templateLambda.setFile(getName());
	//templateLambda.setLine(declLine, declColumn);
      }
    nesting.append((char) elements.size());
    Symbol qname;
    if (type instanceof XName)
      qname = ((XName) type).getQName();
    else
      qname = (Symbol) type;
    push(ElementConstructor.make(typeName, qname));
    /*
    String xslcommand = ...;
    Expression[] args;
    if (inTemplate)
      {
	if (curXSLcommand == null)
	  {
	    args = new Expression[3];
	    args[0] = new ReferenceExp(consumerDecl);
	    args[1] = new QuoteExp(typeName);
	    args[2] = new QuoteExp(type);  // FIXME
	    append(new ApplyExp(beginGroupMethod, args));
	  }
	else
	  {
	    elements.add(typeName);
	    elements.add(type);
	    // error("unimplemented xsl command "+xslcommand);
	  }
      }
    else
      {
      }
    */
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    // if (attributeName != null) ERROR();
    attributeName = attrName;
    attributeType = attrType;
    Symbol qname;
    if (attrType instanceof XName)
      qname = ((XName) attrType).getQName();
    else
      qname = (Symbol) attrType;
    attributeConstructor = AttributeConstructor.make(attrName, qname);
    attributeValue.setLength(0);
    nesting.append((char) elements.size());
    /*
    if (inTemplate)
      {
	if (curXSLcommand == null)
	  {
	  }
	else
	  {
 	    elements.add(attrName);
	    elements.add(attrType);
	  }
      }
    */
  }

  public void endAttribute()
  {
    Expression[] args = { new QuoteExp(attributeValue.toString()) };
    push(new ApplyExp(attributeConstructor, args));
    nesting.setLength(nesting.length()-1);
    attributeConstructor = null;
    attributeType = null;
    attributeName = null;
  }

  public void endGroup(String typeName)
  {
    /*
    if (inTemplate)
      {
	if (curXSLcommand == null)
	  {
	    args = new Expression[2];
	    args[0] = new ReferenceExp(consumerDecl);
	    args[1] = new QuoteExp(typeName);
	    append(new ApplyExp(endGroupMethod, args));
	  }
	else
	  {
	  }
      }
    else
      {
      }
    */
    int nlen = nesting.length()-1;
    int start = nesting.charAt(nlen);
    nesting.setLength(nlen);
    Expression constructor = (Expression) elements.elementAt(start);
    String xslTag = isXslTag(constructor);
    if (xslTag == "value-of")
      {
	String select = popMatchingAttribute("", "select", start + 1);
	if (select != null)
	  {
	    Expression exp = interpreter.parseXPath(select, getMessages());
	    exp = XQParser.stringValue(exp);
	    elements.pop();
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
	elements.pop();
	push(new ApplyExp(new QuoteExp(applyTemplatesProc), args));
      }
    else if (xslTag == "if")
      {
	String select = popMatchingAttribute("", "test", start + 1);
	Expression test = interpreter.parseXPath(select, getMessages());
	test = XQParser.booleanValue(test);
	Expression clause = popTemplateBody(start+1);
	elements.pop();
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
	elements.pop();
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
	Expression[] args = new Expression[elements.size() - start - 1];
	for (int i = args.length;  --i >= 0; )
	  args[i] = (Expression) elements.pop();
	elements.pop();
	Expression exp = new ApplyExp(constructor, args);
	push(exp);
	mexp.body = exp;
      }
  }

  public void writeChar(int v)
  {
    if (attributeType != null)
      attributeValue.append(v);
    else
      push(String.valueOf((char) v));
  }

  void push(Expression exp)
  {
    elements.push(exp);
  }

  void push(Object value)
  {
    push(new QuoteExp(value));
  }

  public void writeBoolean(boolean v)
  {
    if (attributeType != null)
      attributeValue.append(v);
    else
      push(v ? QuoteExp.trueExp : QuoteExp.falseExp);
  }

  public void writeFloat(float v)
  {
    if (attributeType != null)
      attributeValue.append(v);
    else
      push(DFloNum.make(v));
  }

  public void writeDouble(double v)
  {
    if (attributeType != null)
      attributeValue.append(v);
    else
      push(DFloNum.make(v));
  }

  public void writeInt(int v)
  {
    if (attributeType != null)
      attributeValue.append(v);
    else
      push(IntNum.make(v));
  }

  public void writeLong(long v)
  { 
    if (attributeType != null)
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
    if (attributeType != null)
      attributeValue.append(v);
    else
      push(v);
  }

  public void writeChars(String str)
  {
    if (attributeType != null)
      attributeValue.append(str);
    else
      push(str);
  }

  public void write(char[] buf, int off, int len)
  {
    if (attributeType != null)
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
    return (Expression) elements.pop();
  }

  /*
  public void fatal(String message) throws SyntaxException
  {
    messages.error('f', message);
    throw new SyntaxException(messages);
  }
  */

  Expression resolveQNameExpression(String name)
  {
    if (name == null)
      return QuoteExp.nullExp;
    else
      return new QuoteExp(Symbol.make(null, name)); // FIXME
  }

  public void parse (ModuleExp mexp)
  {
    beginDocument(mexp);
    parser.parse();
    endDocument();
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
