// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;

/** A SeqPosition used to represent a node in (usually) a TreeList.
 * This is special in that the represented node is the current position
 * of the SeqPosition - but when passed to a method it is only valid
 * during that method.  After the method returns, the caller is free to
 * change the position, so if the node is saved in a data structure it
 * must be copied. */

public class NodeType extends ClassType
{
  public NodeType(String name)
  {
    super(name);
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitInvokeStatic(coerceMethod);
  }

  public Object coerceFromObject (Object obj)
  {
    return coerce(obj);
  }

  public Type getImplementationType()
  {
    return typeSeqPosition;
  }

  public static SeqPosition coerce(Object obj)
  {
    if (obj instanceof AbstractSequence)
      return new SeqPosition((AbstractSequence) obj, 0, false);
    else if (obj instanceof SeqPosition)
      return (SeqPosition) obj;
    else
      return new SeqPosition(null, 0, obj);
  }

  public static final ClassType typeSeqPosition = ClassType.make("gnu.lists.SeqPosition");
  public static final ClassType typeNodeType = ClassType.make("gnu.kawa.xml.NodeType");
  public static final NodeType nodeType = new NodeType("gnu.lists.SeqPosition");
  static final Method coerceMethod = typeNodeType.getDeclaredMethod("coerce", 1);
}
