// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.PrintStream;

public class XPathContext
	implements XPathConstants, PositionConsumer
{
  int currentPosition;

  int currentSize = -1;
	
  TreePosition position;

  //Node[] path;
  //  int numParents;

  VariableProvider variableProvider = null;

  /** Index in xpath of axis specifier that starts matchStep.
   * Used to evaluate position() and last(). */
  int stepPc;

  /** Index in xpath of most recent OP_PREDICATE.
   * Used to evaluate position() and last(). */
  int predPc;

  public XPathContext()
  {
    position = new TreePosition();
  }

  public XPathContext(TreePosition position)
  {
    this.position = position;
  }
	
  public XPathContext(Object root)
  {
    position = new TreePosition(root);
  }

  public int getCurrentPosition()
  {
    return currentPosition;
  }

  public int getCurrentSize()
  {
    return currentSize;
  }

  public String getCurrentTypeName()
  {
    // return getCurrentNode().getNodeName();
    throw new Error("not implemented");
  }

  public TreePosition getPosition()
  {
    return position;
  }

  public int getAncestorDepth()
  {
    return position.getDepth();
  }

  /** Array of counters used to keep track of position(). */
  int[] counters;

  public int getCounter(int counter)
  {
    if (counters == null)
      counters = new int[counter > 10 ? counter : 10];
    if (counter <= counters.length) {
      int newLength = counters.length;
      if (counter >= newLength)
	newLength = counter + 5;
      int[] newCounters = new int[newLength];
      System.arraycopy(counters, 0, newCounters, 0, counters.length);
      counters = newCounters;
    }
    return counters[counter];
  }

  public void setCounter(int counter, int value)
  {
    getCounter(counter);
    counters[counter] = value;
  }

  public void incrementCounter(int counter)
  {
    getCounter(counter);
    counters[counter]++;
  }

  int resultType;
  public double numResult;
  public boolean boolResult;
  public Object objResult;

  /** Specifies what to do if this.consume(TreePosition) is called. */
  public int consumeKind;

  /** Calling consume(Node) increments consumeCount.
   * Used to count the number of nodes in a result node-set. */
  public static final int CONSUME_COUNT = 1;

  /** Calling consume(Node) sets consumeKind to true and returns false.
   * Used when converting a node-set to boolean. */
  public static final int CONSUME_BOOL = 2;

  /** Calling consume(Node) sets consumeObject to string value to true
   * and returns false (so one first node is processed).
   * Used to implement string(nodeset). */
  public static final int CONSUME_STRING = 3;

  /** Calling consume(Node) appends string value to StringBuffer in consumeObject.
   * Used to implement testsuite. */
  public static final int CONSUME_STRING_APPEND = 4;

  /** Calling consume(Node) save name of first node in consumeObject.
   * Used to implement name() function. */
  public static final int CONSUME_NAME = 5;

  public int consumeCount;
  public Object consumeObject;

  /** Consume node.
   * If (resultType==BOOLEAN_RESULT), we only care *if* there are any nodes.
   */
  public boolean consume(TreePosition position)
  {
    switch (consumeKind)
      {
      case CONSUME_COUNT:
	consumeCount++;
	return true;
      case CONSUME_BOOL:
	consumeCount = 1;
	return false;
	/*
      case CONSUME_STRING_APPEND:
	if (consumeObject == null)
	  consumeObject = new StringBuffer(100);
	NodeUtil._valueOf((StringBuffer) consumeObject, node);
	return true;
      case CONSUME_STRING:
	if (consumeObject == null)
	  consumeObject = NodeUtil.valueOf(node);
	return false;
      case CONSUME_NAME:
	if (consumeObject == null)
	  consumeObject = node.getNodeName();
	return false;
	*/
      default:
	throw new Error("unknown consumeKind");
      }
  }

  public boolean writePosition(AbstractSequence seq, int ipos, Object xpos)
  {
    switch (consumeKind)
      {
      case CONSUME_COUNT:
	consumeCount++;
	return true;
      case CONSUME_BOOL:
	consumeCount = 1;
	return false;
      default:
	throw new Error("unknown consumeKind");
      }
  }

  public void setVariableProvider(VariableProvider newProvider)
  {
    variableProvider = newProvider;
  }

  public void getVariableReference(Object qname, int resultType)
  {
    if (variableProvider != null)
      {
	XPathVariable var = variableProvider.getVariable(qname);
	if (var != null) 
	  var.result(this, resultType);
	else
	  XPath.result(this, 
		       "<unknown variable " + qname.toString() + '>', 
		       resultType);
      }
    else
      {
	String value = System.getProperty(qname.toString());
	if (value == null)
	  value = "<unknown property " + qname + '>';
	XPath.result(this, value, resultType);
      }
  }

    public static void dump(TreePosition position, PrintStream out)
    {
		new XPathContext(position).dump("", out);
    }

  public void dump(PrintStream out)
  {
    dump("", out);
  }

  public void dump(String indent, PrintStream out)
  {
    String nodeName = getCurrentTypeName();
    if (nodeName.equals("#text") )
      {
	out.print(indent);
	out.println(position.getNext());
      }
    else
      {
	out.print(indent);
	out.print('<');
	out.print(nodeName);
	if (position.gotoAttributesStart())
	  {
	    int savePosition = currentPosition;
	    int saveSize = currentSize;
	    currentSize = position.fromEndIndex();
	    currentPosition = 0;
	    try
	      {
		while (position.hasMoreElements())
		  {
		    out.print(' ');
		    out.print(getCurrentTypeName());
		    out.print("=\"");
		    out.print(position.getNext());
		    out.print('\"');
		    position.gotoNext();
		    currentPosition++;
		  }
	      }
	    finally
	      {
		currentPosition = savePosition;
		currentSize = saveSize;
		position.gotoParent();
	      }
	  }
	out.print(">");

	int count;
	if (! position.gotoChildrenStart())
	  count = 0;
	else
	  {
	    int savePosition = currentPosition;
	    int saveSize = currentSize;
	    currentSize = count = position.fromEndIndex();
	    currentPosition = 0;
	    try
	      {
		while (position.hasMoreElements())
		  {
		    if (currentPosition == 0)
		      out.println();
		    dump(indent+" ", out);
		    position.gotoNext();
		    currentPosition++;
		  }
	      }
	    finally
	      {
		currentPosition = savePosition;
		currentSize = saveSize;
		position.gotoParent();
	      }
	  }
	if (count > 0) out.print(indent);
	out.print("</");
	out.print(nodeName);
	out.print('>');
	if (count > 0) out.print(indent);
      }
  }
}
