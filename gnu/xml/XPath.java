// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;

public class XPath implements XPathConstants
{
  String srcPath;
  short[] code;
  short codeLen;
  Object[] values;
  short valLen;

  /*
    public static String stringValue(Node node)
    {
    return NodeUtil.valueOf( node );
    }
  */

  public static String stringValue(double value)
  {
    long lvalue = (long) value;
    if ((double) lvalue == value)
      return Long.toString(lvalue);
    else // FIXME - should never use exponential notation!
      return Double.toString(value);
  }

  public static String stringValue(Object value)
  {
    if (value instanceof Number)
      return stringValue(((Number) value).doubleValue());
    /*
      else if (value instanceof NodeList)
      {
      NodeList nodes = (NodeList) value;
      int count = nodes.getLength();
      // FIXME? - Not sure if you should get the value-of here.
      return count == 0 ? "" : NodeUtil.valueOf(nodes);
	}
    */
    else
      return value.toString();
  }

  public static double numberValue(Object value)
  {
    if (value instanceof Number)
      return ((Number) value).doubleValue();
    else
      return Convert.parseDouble(stringValue(value));
  }

  public static boolean booleanValue(Object value)
  {
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();
    else if (value instanceof Number)
      return ((Number) value).doubleValue() != 0.0;
    /*
      else if (value instanceof NodeList)
      return ((NodeList) value).getLength() > 0;
    */
    else
      return value.toString().length() > 0;
  }

  void result(XPathContext context, Object value, int resultType)
  {
    if (value instanceof Boolean)
      result(context, ((Boolean) value).booleanValue(), resultType);
    else if (value instanceof Number)
      result(context, ((Number) value).doubleValue(), resultType);
      else if (value instanceof String)
	result(context, (String) value, resultType);
      /*
	else if (value instanceof NodeList)
	result(context, (NodeList) value, resultType);
      */
      else if (resultType == ANY_RESULT)
	{
	  context.objResult = value;
	  context.resultType = ANY_RESULT;
	}
      else
	throw new Error("unhandled result code"); // FIXME
    }

  void result(XPathContext context, double value, int resultType)
    {
      if (resultType == BOOLEAN_RESULT)
	{
	  context.boolResult = value != 0.0;
	  context.resultType = BOOLEAN_RESULT;
	}
      else if (resultType == PREDICATE_RESULT)
	{
	  context.boolResult = value == getContextPosition(context);
	  context.resultType = BOOLEAN_RESULT;
	}
      else if (resultType == STRING_RESULT)
	{
	  context.objResult = stringValue(value);
	  context.resultType = STRING_RESULT;
	}
      else if (resultType == NUMBER_RESULT || resultType == ANY_RESULT)
	{
	  context.numResult = value;
	  context.resultType = NUMBER_RESULT;
	}
      else
	throw new Error("unhandled result code"); // FIXME
    }

  static void result(XPathContext context, String value, int resultType)
    {
      if (resultType == ANY_RESULT || resultType == STRING_RESULT)
	{
	  context.objResult = value;
	  context.resultType = STRING_RESULT;
	}
      else if (resultType == BOOLEAN_RESULT || resultType == PREDICATE_RESULT)
	{
	  context.boolResult = value.length() > 0;
	  context.resultType = BOOLEAN_RESULT;
	}
      else if (resultType == NUMBER_RESULT)
	{
	  context.numResult = Convert.parseDouble(value);
	  context.resultType = NUMBER_RESULT;
	}
      /*
	else if (resultType == NODESET_RESULT)
	{
	SimpleNodeList list = new SimpleNodeList();
	list.appendChild( new TextNode(value));
	context.objResult = list;
	context.resultType = NODESET_RESULT;
	}
      */
      else
	throw new Error("unhandled result code"); // FIXME
    }

  static void result(XPathContext context,
		     TreePositionList value, int resultType)
  {
    if (resultType == ANY_RESULT || resultType == NODESET_RESULT)
      {
	context.objResult = value;
	context.resultType = NODESET_RESULT;
      }
    else if (resultType == BOOLEAN_RESULT
	     || resultType == PREDICATE_RESULT)
      {
	context.boolResult = ! value.isEmpty();
	context.resultType = BOOLEAN_RESULT;
      }
    else if (resultType == STRING_RESULT)
      {
	context.objResult = stringValue(value);
	context.resultType = STRING_RESULT;
      }
    else
      throw new Error("unhandled result code"); // FIXME
  }

  static void result(XPathContext context, boolean value, int resultType)
    {
      if (resultType == BOOLEAN_RESULT || resultType == ANY_RESULT
	  || resultType == PREDICATE_RESULT)
	{
	  context.boolResult = value;
	  context.resultType = BOOLEAN_RESULT;
	}
      else if (resultType == NUMBER_RESULT)
	{
	  context.numResult = value ? 1.0 : 0.0;
	  context.resultType = NUMBER_RESULT;
	}
      else if (resultType == STRING_RESULT)
	{
	  context.objResult = value ? "true" : "false";
	  context.resultType = STRING_RESULT;
	}
      else
	throw new Error("unknown result code");   // FIXME
    }

  public Object eval(XPathContext context)
    {
      return eval(context, codeLen - 1);
    }

  public Object eval(XPathContext context, int pc)
    {
      eval(context, pc, ANY_RESULT);
      switch (context.resultType) {
      case STRING_RESULT:
      case NODESET_RESULT:
      case ANY_RESULT:
	return context.objResult;
      case BOOLEAN_RESULT:
	return context.boolResult ? Boolean.TRUE : Boolean.FALSE;
      case NUMBER_RESULT:
	return new Double(context.numResult);
      default:
	throw new Error ("unknown result Type");
      }
    }

  public void error(String message)
    {
      throw new Error(message);
    }

  public void checkArgs(Object function, int argCount, int expected)
    {
      if (argCount != expected)
	error(function.toString() + ": wrong number of arguments ("
	      + argCount +", should be " + expected + ')');
    }

  public void checkArgs(Object function, int argCount, int min, int max)
    {
      if (argCount < min)
	error(function.toString() + ": too few arguments ("
	      + argCount +", should be at least " + min + ')');
      else if (argCount > max && max >= 0)
	error(function.toString() + ": too many arguments ("
	      + argCount +", should be at most " + max + ')');
    }

  public void call(XPathContext context,
		   Object function, Object[] args, int resultType)
  {
    error("unknown function: "+ function);
  }

  int getContextPosition(XPathContext context)
  {
    return context.getCurrentPosition();
  }

  int getContextSize(XPathContext context)
  {
    return (context.predPc > context.stepPc
	    ? (int) (getPositionAndLast(context) >> 32)
	    : context.getCurrentSize());
  }

  private long getPositionAndLast(XPathContext context)
  {
    /*
    Node curNode = context.getCurrentNode();
    try
      {
	context.popNode();
	Node parent = context.getCurrentNode();
	int pc = context.stepPc;
	short opcode = code[pc];
	int count = 0;
	int position = -1;
	int axis = ((opcode >= OP_AXIS_FIRST
		     && opcode < OP_AXIS_FIRST + COUNT_OP_AXIS)
		    ? opcode - OP_AXIS_FIRST
		    : AXIS_CHILD);
	switch (axis) {
	case AXIS_CHILD:
	  NodeList children = parent.getChildNodes();
	  int childCount = children.getLength();
	  for ( int i = 0; i < childCount; i++ ) {
	    try	{
	      Node child = children.item(i);
	      context.pushNode(child);

	      if (child == curNode)
		position = count;
	      if (matchStep(context, pc, context.predPc))
		count++;
	    }
	    finally {
	      context.popNode();
	    }
	  }
	}
	return ((long) count << 32) | (long) position;
    }
    finally {
      context.pushNode(curNode);
    }
    */
    throw new Error("not implemented");
  }

  /** Evaluate a FunctionCall.
   * @param pc index just after last argument expression
   */
  public void evalCall(XPathContext context, Object function,
		       int argCount, int pc, int resultType)
    {
      if (function instanceof XPathFunction)
	{
	  ((XPathFunction) function).apply(context, argCount,
					   this, pc, resultType);
	  return;
	}
      if (function == "count")
	{
	  checkArgs(function, argCount, 1);
	  pc -= code[pc];
	  int saveKind = context.consumeKind;
	  int saveCount = context.consumeCount;
	  try
	    {
	      context.consumeKind = XPathContext.CONSUME_COUNT;
	      context.consumeCount = 0;
	      evalToNodes(context, pc + code[pc], context);
	      context.resultType = NUMBER_RESULT;
	      context.numResult = context.consumeCount;
	    }
	  finally
	    {
	      context.consumeKind = saveKind;
	      context.consumeCount = saveCount;
	    }
	  return;
	}
      if (function == "not")
	{
	  checkArgs(function, argCount, 1);
	  pc -= code[pc];
	  pc += code[pc];
	  result(context, ! evalToBoolean(context, pc), resultType);
	  return;
	}
      if (function == "boolean")
	{
	  checkArgs(function, argCount, 1);
	  pc -= code[pc];
	  pc += code[pc];
	  result(context, evalToBoolean(context, pc), resultType);
	  return;
	}
      if (function == "number")
	{
	  checkArgs(function, argCount, 1);
	  pc -= code[pc];
	  pc += code[pc];
	  result(context, evalToNumber(context, pc), resultType);
	  return;
	}
      if (function == "position")
	{
	  checkArgs(function, argCount, 0);
	  result(context, getContextPosition(context), resultType);
	  return;
	}
      if (function == "last")
	{
	  checkArgs(function, argCount, 0);
	  result(context, getContextSize(context), resultType);
	  return;
	}
      if (function == "string")
	{
	  checkArgs(function, argCount, 1);
	  pc -= code[pc];
	  pc += code[pc];
	  int value;
	  result(context, evalToString(context, pc), resultType);
	  return;
	}
      if (function == "true") {
	checkArgs(function, argCount, 0);
	result(context, true, resultType);
	return;
      }
      if (function == "false") {
	checkArgs(function, argCount, 0);
	result(context, false, resultType);
	return;
      }
      if (function == "starts-with") {
	checkArgs(function, argCount, 2);
	pc -= code[pc];
	pc += code[pc];
			
	String source = evalToString( context, pc);

	int len = code[++pc];
	pc += len;
	String match = evalToString( context, pc);

	result(context, source.startsWith( match ), resultType);
	return;
      }
      if (function == "ends-with") {
	checkArgs(function, argCount, 2);
	pc -= code[pc];
	pc += code[pc];
			
	String source = evalToString( context, pc);

	int len = code[++pc];
	pc += len;
	String match = evalToString( context, pc);

	result(context, source.endsWith( match ), resultType);
	return;
      }
      if (function == "substring") {
	checkArgs(function, argCount, 2);
	pc -= code[pc];
	pc += code[pc];
			
	String source = evalToString( context, pc);

	int len = code[++pc];
	pc += len;
	int index = (int) evalToNumber( context, pc);
	String result = source.substring( index );
	result(context, result, resultType);
	return;
      }
      /*
	if (function == "name") {
	checkArgs(function, argCount, 0, 1);
	if (argCount == 0) {
	result(context,
	context.getCurrentNode().getNodeName(), resultType);
	}
	else {
	pc -= code[pc];
	pc += code[pc];
			
	int saveKind = context.consumeKind;
	Object saveObject = context.consumeObject;
	try {
	context.consumeKind = XPathContext.CONSUME_NAME;
	context.consumeObject = null;
	evalToNodes(context, pc, context);
	context.resultType = STRING_RESULT;
	result(context, context.consumeObject.toString(),
	resultType);
	}
	finally {
	context.consumeKind = saveKind;
	context.consumeObject = saveObject;
	}
	}
	return;
	}
      */
      Object[] args = new Object[argCount];
      pc -= code[pc];
      for (int i = 0;  i < argCount;  i++) {
	pc += code[pc];
	args[i] = eval(context, pc);
	pc++;
      }
      call(context, function, args, resultType);
    }

  public static boolean compare(Object val1, Object val2, int opcode)
    {
      /*
	if (val1 instanceof NodeList) {
	NodeList nodes = (NodeList) val1;
	int count = nodes.getLength();
	if (val2 instanceof Boolean) {
	val1 = count > 0 ? Boolean.TRUE : Boolean.FALSE;
	return compare(val1, val2, opcode);
	}
	for (int j = 0;  j < count;  j++) {
	Node node = nodes.item(j);
	if (compare(stringValue(node), val2, opcode))
	return true;
	}
	return false;
	}
	else if (val2 instanceof NodeList) {
	NodeList nodes = (NodeList) val2;
	int count = nodes.getLength();
	if (val1 instanceof Boolean) {
	val2 = count > 0 ? Boolean.TRUE : Boolean.FALSE;
	return compare(val1, val2, opcode);
	}
	for (int j = 0;  j < count;  j++) {
	Node node = nodes.item(j);
	if (compare(val1, stringValue(node), opcode))
	return true;
	}
	return false;
	}
	else
      */
      if (opcode <= OP_NEQ) { // OP_EQU or OP_NEQ
	if (val1 instanceof Boolean || val2 instanceof Boolean) {
	  boolean bool1 = booleanValue(val1);
	  boolean bool2 = booleanValue(val2);
	  return (bool1 == bool2) == (opcode == OP_EQU);
	}
	else if (val1 instanceof Number || val2 instanceof Number) {
	  double num1 = numberValue(val1);
	  double num2 = numberValue(val2);
	  return (num1 == num2) == (opcode == OP_EQU);
	}
	else {
	  String str1 = stringValue(val1);
	  String str2 = stringValue(val2);
	  return str1.equals(str2) == (opcode == OP_EQU);
	}
      }
      else {
	double num1 = numberValue(val1);
	double num2 = numberValue(val2);
	switch (opcode) {
	case OP_LSS:  return num1 <  num2;
	case OP_LEQ:  return num1 <= num2;
	case OP_GRT:  return num1 >  num2;
	case OP_GEQ:  return num1 >= num2;
	default:  return false;  // Should never happen.
	}
      }
    }

  public void eval(XPathContext context, int pc, int resultType)
    {
      short opcode = code[pc];
      double val1, val2;
      int op2len, op1;
      switch (opcode)
	{
	case OP_AND:
	case OP_OR:
	  op2len = code[--pc];
	  pc -= 2;
	  op1 = pc - op2len;
	  boolean bval = evalToBoolean(context, op1);
	  result(context,
		 (opcode == OP_OR) == bval ? bval : evalToBoolean(context, pc),
		 resultType);
	  break;
	case OP_EQU:
	case OP_NEQ:
	case OP_LSS:
	case OP_LEQ:
	case OP_GRT:
	case OP_GEQ:
	  op2len = code[--pc];
	  pc -= 2;
	  op1 = pc - op2len;
	  Object obj1 = eval(context, op1);
	  Object obj2 = eval(context, pc);
	  result(context, compare(obj1, obj2, opcode), resultType);
	  break;
	case OP_ADD:
	case OP_SUB:
	case OP_MUL:
	case OP_DIV:
	case OP_MOD:
	  op2len = code[--pc];
	  pc -= 2;
	  op1 = pc - op2len;
	  val1 = evalToNumber(context, op1);
	  val2 = evalToNumber(context, pc);
	  switch (opcode)
	    {
	    case OP_ADD:  val1 = val1 + val2;  break;
	    case OP_SUB:  val1 = val1 - val2;  break;
	    case OP_MUL:  val1 = val1 * val2;  break;
	    case OP_DIV:  val1 = val1 / val2;  break;
	    case OP_MOD:  val1 = val1 % val2;  break;  // FIXME check result
	    }
	  result(context, val1, resultType);
	  break;
	case OP_STRING_LITERAL:
	  result(context, (String) values[code[pc-1]], resultType);
	  break;
	case OP_SHORT:
	  result(context, (double) code[pc-1], resultType);
	  break;
	case OP_DOUBLE:
	  long bits = 0;
	  pc--;
	  for (int i = 4;  --i >= 0; )
	    bits = (bits << (16 * i)) | (long) (code[pc - i] & 0xffff);
	  result(context, Double.longBitsToDouble(bits), resultType);
	  break;
	case OP_VARIABLE_REFERENCE:
	  Object name = values[code[pc-1]];
	  context.getVariableReference(name, resultType);
	  break;
	case OP_FUNCTION_CALL:
	  Object func = values[code[--pc]];
	  int count = code[--pc];
	  --pc;
	  evalCall(context, func, count, pc, resultType);
	  break;
	case OP_LOCATIONPATH:
	case OP_EXPR_LOCATIONPATH:
	case OP_UNION:
	  if (resultType == BOOLEAN_RESULT
	      || resultType == PREDICATE_RESULT)
	    {
	      int saveKind = context.consumeKind;
	      int saveCount = context.consumeCount;
	      try
		{
		  context.consumeKind = XPathContext.CONSUME_BOOL;
		  context.consumeCount = 0;
		  if (opcode == OP_UNION)
		    {
		      int numOperands = code[pc-1];
		      for (int i = 0;  i < numOperands;  i++)
			{
			  int partPc = (i+1 == numOperands ? pc-2-numOperands
					: code[pc-numOperands+i] + pc - 1);
			  if (! evalToNodes(context, partPc, context))
			    break;
			}
		    }
		  else
		    evalToNodes(context, pc, context);
		  context.resultType = BOOLEAN_RESULT;
		  context.boolResult = context.consumeCount > 0;
		}
	      finally
		{
		  context.consumeKind = saveKind;
		  context.consumeCount = saveCount;
		}
	    }
	  else
	    {
	      TreePositionList list = new TreePositionList();
	      evalToNodes(context, pc, list);
	      result(context, list, resultType);
	      //list.finalize();
	      break;
	    }
	  break;
	default:
	  throw new Error ("unhandled opcode");
	}
    }

  public boolean evalToNodes(XPathContext context, PositionConsumer consumer)
    {
      context.stepPc = 0;
      context.predPc = 0;
      return evalToNodes(context, codeLen - 1, consumer);
    }

  /**
   * Return false iff the consumer is "satisfied" - i.e. will ignore
   * any more nodes. */
  public boolean evalToNodes(XPathContext context, int pc,
			     PositionConsumer consumer)
    {
      TreePositionList nodes;
      int nodeCount;
      short opcode = code[pc];
      int opcodepc = pc;
      int numSteps;
      switch (opcode)
	{
	case OP_LOCATIONPATH:
	  numSteps = code[--pc];
	  pc -= numSteps;
	  return evalPath(context, code[pc] + opcodepc, pc, consumer);
	  /*
	case OP_EXPR_LOCATIONPATH:
	  // FIXME - maybe implement this using XPathContext.consume.
	  numSteps = code[--pc];
	  pc -= numSteps;
	  int start = code[pc] + opcodepc;
	  eval(context, start - 1, NODESET_RESULT);
	  nodes = (TreePositionList) context.objResult;
	  nodeCount = nodes.size();
	  for (int i = 0; i < nodeCount; i++)
	    {
	      try
		{
		  Node node = nodes.get(i);
		  context.pushNode(node);
		  if (! evalPath(context, start, pc, consumer))
		    return false;
		}
	      finally
		{
		  context.popNode();
		}
	    }
	  return true;
	  */
	case OP_UNION:
	  int numOperands = code[pc-1];
	  int[] levels = new int[numOperands];
	  return evalUnionStep(context, levels, 0, pc, consumer);
	default:
	  /*
	  eval(context, pc, NODESET_RESULT);
	  nodes = (TreePositionList) context.objResult;

	  nodeCount = nodes.size();
	  for (int i = 0; i < nodeCount; i++)
	    if (! consumer.consume(nodes.get(i)))
	      return false;
	  return true;
	  */
	  throw new Error("unimplemented");
	}
    }

  /**
   * Return false iff the consumer is "satisfied" - i.e. will ignore
   * any more nodes. */
  public boolean evalPath(XPathContext context, int pc, int stop,
			  PositionConsumer consumer)
    {
      TreePosition position = context.position;
      int count, savePos;
      for (;;)
	{
	  if (pc == stop)
	    {
	      return consumer.consume(context.position);
	    }
	  short opcode = code[pc];
	  if (opcode >= OP_AXIS_FIRST
	      && opcode < OP_AXIS_FIRST + COUNT_OP_AXIS)
	    context.stepPc = pc;
	  context.predPc = pc;
	  int j;
	  pc++;
	  switch (opcode)
	    {
	    case OP_AXIS_FIRST+AXIS_CHILD:
	      if (position.gotoChildrenStart())
		{
		  try
		    {
		      while (position.hasNext())
			{
			  /*
			  int next = matchSingleStep(context, pc, stop);
			  if (next >= 0
			      && ! evalPath(context, next, stop, consumer))
			    return false;
			  */
			  if (! evalPath(context, pc, stop, consumer))
			    return false;
			  position.gotoNext();
			}
		    }
		  finally
		    {
		      position.gotoParent();
		    }
		}
	      return true;
	    case OP_AXIS_FIRST+AXIS_DESCENDANT_OR_SELF:
	      if (! evalPath(context, pc, stop, consumer))
		return false;
	      if (position.gotoChildrenStart())
		{
		  try
		    {
		      while (position.hasNext())
			{
			  // The trick here is that we re-evaluate the
			  // descendant-or-self operation (by using pc-1 in the
			  // recursive call) on each child.
			  if (! evalPath(context, pc - 1, stop, consumer))
			    return false;
			  position.gotoNext();
			}
		    }
		  finally
		    {
		      position.gotoParent();
		    }
		}
	      return true;
	    case OP_AXIS_FIRST+AXIS_DESCENDANT:
	      if (position.gotoChildrenStart())
		{
		  try
		    {
		      while (position.hasNext())
			{
			  // The trick here is that we re-evaluate the
			  // descendant-or-self operation (by using pc-1 in the
			  // recursive call) on each child.
			  if (! evalPath(context, pc, stop, consumer)
			      || ! evalPath(context, pc - 1, stop, consumer))
				return false;
			  position.gotoNext();
			}
		    }
		  finally
		    {
		      position.gotoParent();
		    }
		}
	      return true;
	      /*
				case OP_AXIS_FIRST+AXIS_ATTRIBUTE:
				NamedNodeMap attrs = node.getAttributes();
				int attrCount = attrs.getLength();
				for ( int i = 0; i < attrCount; i++ ) {
				try {
				Node child = attrs.item(i);
				context.pushNode(child);
				if (! evalPath(context, pc, stop, consumer))
				return false;
				}
				finally {
				context.popNode();
				}
				}
				return true;
	      */

	    case OP_SIMPLE_NAMETEST:
	      {
		Object name = values[code[pc]];
		if (! name.equals(position.getNextTypeName()))
		  return true;
		pc++;
	      }
	      continue;
	    case OP_AXIS_FIRST+AXIS_SELF:
	      if (code[pc] == OP_ROOT) {
		/* FIXME - need getDocumentRoot
		   try {
		   context.pushNode(context.getDocumentRoot());
		   if (! evalPath(context, pc + 1, stop, consumer))
		   return false;
		   }
		   finally {
		   context.popNode();
		   }
		*/
		return true;
	      }
				/* ... else fall though ... */
	    case OP_NODE:
	    case OP_ANY_NAMETEST:
	      continue;
				/*
				  case OP_ROOT:
				  if (node.getNodeType() != Node.DOCUMENT_NODE)
				  return true;
				  continue;
				  case OP_TEXT:
				  if (node.getNodeType() != Node.TEXT_NODE)
				  return true;
				  continue;
				  case OP_COMMENT:
				  if (node.getNodeType() != Node.COMMENT_NODE)
				  return true;
				  continue;
				  case OP_PROCESSING_INSTRUCTION:
				  if (node.getNodeType() != Node.PROCESSING_INSTRUCTION_NODE)
				  return true;
				  j = code[pc++];
				  if (j >= 0) {
				  Object name = values[j];
				  if (! node.getNodeName().equals(name))
				  return true;
				  }
				  continue;
				*/
	    case OP_PREDICATE:
	      int predPc = pc - 1;
	      int counter = code[pc++];
	      // We initialize context.predPc to context.stepPc.
	      // If context.predPc is less than the current pc, it means this
	      // is the first time for this predicate (in this step), and we
	      // need to first clear the counter.  We then increment the
	      // counter - and copy the result to context.currentPosition.
	      // The counter thus keeps track of the number of times we
	      // have evaluated this predicate - which is the same as
	      // the context position.
	      // Setting context.predPc to context.stepPc on a new step
	      // essentially forces all the counter in this step to zero,
	      // since they will be zerod the first time we evaluate the
	      // predicate.  (We do the same thing in matchSingleStep.)
	      if (context.predPc < predPc)
		context.setCounter(counter, 0);
	      context.predPc = predPc;
	      context.incrementCounter(counter);
	      j = code[pc++];
	      pc += j;
	      boolean doContinue;
	      int savePosition = context.currentPosition;
	      try
		{
		  context.currentPosition = context.counters[counter];
		  doContinue = evalToPredicate(context, pc - 1);
		}
	      finally
		{
		  context.currentPosition =  savePosition;
		}
	      if (doContinue)
		continue;
	      else
		return true;
	    default:
	      if (opcode < 0)
		; // end of Step - ignore FIXME
	      else
		error("unknown upcode "+opcode);
	    }
	}
    }

  public boolean evalUnionStep(XPathContext context, int levels[], int level,
			       int unionPc, PositionConsumer consumer)
  {
    TreePosition position = context.position;
    if (position.gotoChildrenStart())
      {
	try
	  {
	    while (position.hasNext())
	      {
		if (! evalUnionStepChild(context, levels, level,
				       unionPc, consumer))
		  return false;
		position.gotoNext();
	      }
	  }
	finally
	  {
	    position.gotoParent();
	  }
      }
    return true;
  }

  public boolean evalUnionStepChild(XPathContext context, int levels[],
				    int level, int unionPc,
				    PositionConsumer consumer)
    {
      // levels[i] is the last level-number that succeeded for part i.
      boolean doConsume = false;
      boolean doRecurse = false;
      int saveStepPc = context.stepPc;
      int savePredPc = context.predPc;
      int unionParts = code[unionPc-1];
      for (int i = 0;  i < unionParts;  i++) {
	int pathPc = i+1==unionParts ? unionPc - 2 - unionParts
	  : unionPc + code[unionPc-unionParts+i] - 1;
	int stepCount = code[pathPc-1];
	if (level >= stepCount)
	  continue;
	int stepStart = code[pathPc-1-stepCount+level]+pathPc;
	int pathEnd = pathPc - 1 - stepCount;
	int stepEnd = level+1==stepCount ? pathEnd
	  : pathPc + code[pathPc-stepCount+level];
	if (levels[i] == level) {
	  context.stepPc = stepStart;
	  context.predPc = stepStart;
	  stepStart = matchSingleStep(context, stepStart, stepEnd);
	  if (stepStart > 0) {
	    levels[i]++;
	    if (stepStart == pathEnd)
	      doConsume = true;
	    else
	      doRecurse = true;
	  }
	}
      }
      context.stepPc = saveStepPc;
      context.predPc = savePredPc;
      if (doConsume) {
	if (! consumer.consume(context.getPosition()))
	  return false;
      }

      if (doRecurse
	  && ! evalUnionStep(context, levels, level+1, unionPc, consumer))
	return false;
      for (int i = 0;  i < unionParts;  i++) {
	if (levels[i] > level)
	  levels[i] = level;
      }
      return true;
    }

  public final double evalToNumber(XPathContext context, int opcodepc)
  {
    eval(context, opcodepc, NUMBER_RESULT);
    return context.numResult;
  }

  public final boolean evalToBoolean(XPathContext context, int opcodepc)
  {
    eval(context, opcodepc, BOOLEAN_RESULT);
    return context.boolResult;
  }

  /**
   * Eval to a Boolean without specifying an opcode. For public use.
   */
  public final boolean evalToBoolean(XPathContext context )
  {
    evalToBoolean(context, codeLen - 1);
    return context.boolResult;
  }

  public final String evalToString( XPathContext context )
  {
    context.stepPc = 0;
    context.predPc = 0;
    return evalToString( context, codeLen - 1);
  }

  public final String evalToString(XPathContext context, int opcodepc)
  {
    int saveKind = context.consumeKind;
    Object saveObject = context.consumeObject;
    String result;
    try {
	context.consumeKind = XPathContext.CONSUME_STRING;
	context.consumeObject = null;
	evalToNodes(context, opcodepc, context);
	context.resultType = STRING_RESULT;
	result = context.consumeObject.toString();
      }
      finally {
	context.consumeKind = saveKind;
	context.consumeObject = saveObject;
      }
      return result;
    }

  /** Same as evalToBoolean, but a number is interpreted
	 * as position()==NUM. */
  public boolean evalToPredicate(XPathContext context, int opcodepc)
  {
    eval(context, opcodepc, PREDICATE_RESULT);
    return context.boolResult;
  }

  public final double evalToNumber(XPathContext context)
  {
    return evalToNumber(context, codeLen-1);
  }

  private boolean matchPath(XPathContext context, int stepNum, int opcodepc)
  {
    if (stepNum == 0)
      return true;
    int numSteps = code[opcodepc-1];
    int pc = opcodepc + code[opcodepc - numSteps + stepNum - 2];
    int stop = numSteps == stepNum ? opcodepc - numSteps - 1
      : opcodepc + code[opcodepc - numSteps + stepNum - 1];
    context.stepPc = pc;
    int axis = code[pc];
    if (axis != OP_AXIS_FIRST+AXIS_DESCENDANT_OR_SELF
	&& ! matchStep(context, pc, stop))
      return false;
    return context.position.gotoParent()
      && matchPath(context, stepNum - 1, opcodepc);
  }

  private boolean matchStep(XPathContext context, int pc, int stop)
  {
    while (pc < stop)
      {
	pc = matchSingleStep(context, pc, stop);
	if (pc < 0)
	  return false;
      }
    return true;
  }

  /** Match a single Step.
   * @return -1 if not a match; pc for next step if matched so far.
   */
  int matchSingleStep(XPathContext context, int pc, int stop)
  {
    int axis = AXIS_CHILD;
    for (;;)
      {
	if (pc == stop)
	  {
	    return pc;
	  }
	short opcode = code[pc];
	pc++;
	if (opcode >= OP_AXIS_FIRST
	    && opcode < OP_AXIS_FIRST + COUNT_OP_AXIS)
	  {
	    axis = opcode - OP_AXIS_FIRST;
	    /*
	    if (axis == AXIS_ATTRIBUTE
		&& node.getNodeType() != Node.ATTRIBUTE_NODE)
	      return pc - 1;
	    */
	    continue;
	  }
	  int j;
	  switch (opcode)
	    {
	    case OP_SIMPLE_NAMETEST:
	      {
		Object name = values[code[pc]];
		if (! name.equals(context.position.getNextTypeName()))
		  return -1;
		pc++;
	      }
	      continue;
	    case OP_NODE:
	    case OP_ANY_NAMETEST:
	      continue;
				/*
				  case OP_ROOT:
				  if (node.getNodeType() != Node.DOCUMENT_NODE)
				  return -1;
				  continue;
				  case OP_TEXT:
				  if (node.getNodeType() != Node.TEXT_NODE)
				  return -1;
				  continue;
				  case OP_COMMENT:
				  if (node.getNodeType() != Node.COMMENT_NODE)
				  return -1;
				  continue;
				  case OP_PROCESSING_INSTRUCTION:
				  if (node.getNodeType() != Node.PROCESSING_INSTRUCTION_NODE)
				  return -1;
				  j = code[pc++];
				  if (j >= 0) {
				  Object name = values[j];
				  if (! node.getNodeName().equals(name))
				  return -1;
				  }
				  continue;
				*/
	  case OP_PREDICATE:
				// See explanation of this code in evalPath.
	    int predPc = pc - 1;
	    int counter = code[pc++];
	    if (context.predPc < predPc)
	      context.setCounter(counter, 0);
	    context.predPc = predPc;
	    context.incrementCounter(counter);
	    j = code[pc++];
	    pc += j;
	    boolean doContinue;
	    int savePosition = context.currentPosition;
	    try {
	      context.currentPosition = context.counters[counter];
	      doContinue = evalToPredicate(context, pc - 1);
	    } finally {
	      context.currentPosition =  savePosition;
	    }
	    if (doContinue)
	      continue;
	    else
	      return -1;
	  }
	}
    }

  public boolean match(XPathContext context, int pc)
    {
      short opcode = code[pc];
      switch (opcode) {
      case OP_UNION:
	int numOperands = code[pc-1];
	for (int i = 0;  ; ) {
	  if (++i == numOperands)
	    return match(context, pc - 2 - numOperands);
	  else if (match(context, code[pc-numOperands+i] + pc - 1))
	    return true;
	}
      case OP_LOCATIONPATH:
	int opcodepc = pc;
	int numSteps = code[--pc];
	//pc -= numSteps;
	// FIXME - cloning the position is a bit inefficient.
	TreePosition save = (TreePosition) context.position.clone();
	try
	  {
	    return matchPath(context, numSteps, opcodepc);
	  }
	finally
	  {
	    context.position = save;
	  }
      case OP_FUNCTION_CALL:
	// handle id(key) FIXME
      }
      return false;
    }

  public boolean match(XPathContext context)
  {
    return match(context, codeLen - 1);
  }

  /*
    public boolean match(XPathContext context, Node node)
    {
    try {
    context.pushNode(node);
    return match(context, codeLen-1);
    }
    finally {
    context.popNode();
    }
    }
  */

  public static void main( String[] args )
    throws Exception
  {
    XPath xpath = new XPath();
    XPathParser parser = new XPathParser(args[0], xpath);
    parser.parse();

    System.out.print("Parsed len: "+xpath.codeLen+" {");
    for (int j = 0;  j < xpath.codeLen;  j++)
      System.out.print(" " + xpath.code[j]);
    System.out.println("}");

    TreeList doc = new TreeList();
    XMLParser xmlparser = new XMLParser(new java.net.URL(args[1]),
					new ParsedXMLToConsumer(doc));
    xmlparser.parse();

    XPath matcher = null;
    if (args.length > 2)
      {
	matcher = new XPath();
	XPathParser mparser = new XPathParser(args[2], matcher);
	mparser.parse();
	System.out.print("Matcher len: "+matcher.codeLen+" {");
	for (int j = 0;  j < matcher.codeLen;  j++)
	  System.out.print(" " + matcher.code[j]);
	System.out.println("}");
      }


    java.io.PrintWriter out = new java.io.PrintWriter(System.out);
    XMLPrinter printer = new XMLPrinter(out);
    /*
    out.println("Input document:");
    doc.consume(printer);
    out.flush();
    */
    // doc.dump();

    // TreePosition pos = new TreePosition(doc, 0); // ????
    TreePosition pos = new TreePosition(doc);
    XPathContext context = new XPathContext(pos);
    Object result = xpath.eval(context);
    if (result instanceof TreePositionList)
      {
	TreePositionList output = (TreePositionList) result;

	int count = output.size();
	System.err.println("#matches:"+count);
	for (int j = 0;  j < count;  j++)
	  {
	    output.get(j, pos);
	    int i0 = pos.ipos>>1;
	    int i1 = doc.nextDataIndex(i0);
	    System.err.println("#"+j+": "+i0+" .. "+i1);
	    doc.consumeRange(i0, i1, printer);
	    out.flush();
	    if (args.length > 2)
	      System.out.println("matches: "+matcher.match(context));
	  }
      }
    else
      System.err.println("result: "+result);
  }

}
