// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import java.io.*;

/** A SeqPosition used to represent a node in (usually) a TreeList.
 * This is special in that the represented node is the current position
 * of the SeqPosition - but when passed to a method it is only valid
 * during that method.  After the method returns, the caller is free to
 * change the position, so if the node is saved in a data structure it
 * must be copied. */

public class NodeType extends ClassType implements NodePredicate, Externalizable
{
  public static final int TEXT_OK = 1;
  public static final int GROUP_OK = 2;
  int kinds = -1;

  public NodeType(String name, int kinds)
  {
    super(name);
    this.kinds = kinds;
  }

  public NodeType(String name)
  {
    this(name, -1);
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitPushInt(kinds);
    code.emitInvokeStatic(coerceMethod);
  }

  public Object coerceFromObject (Object obj)
  {
    return coerceForce(obj, kinds);
  }

  public Type getImplementationType()
  {
    return typeSeqPosition;
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object xpos)
  {
    return isInstance(seq, ipos, xpos, kinds);
  }

  public static boolean isInstance(AbstractSequence seq, int ipos, Object xpos,
				   int kinds)
  {
    if (kinds >= 0)
      {
	int kind = seq.getNextKind(ipos, xpos);
	switch (kind)
	  {
	  case Sequence.EOF_VALUE:
	    return false;
	  case Sequence.INT_U8_VALUE:
	  case Sequence.INT_S8_VALUE:
	  case Sequence.INT_U16_VALUE:
	  case Sequence.INT_S16_VALUE:
	  case Sequence.INT_U32_VALUE:
	  case Sequence.INT_S32_VALUE:
	  case Sequence.INT_U64_VALUE:
	  case Sequence.INT_S64_VALUE:
	  case Sequence.FLOAT_VALUE:
	  case Sequence.DOUBLE_VALUE:
	  case Sequence.BOOLEAN_VALUE:
	  case Sequence.TEXT_BYTE_VALUE:
	  case Sequence.CHAR_VALUE:
	  case Sequence.OBJECT_VALUE:
	    return (kinds & TEXT_OK) != 0;
	  case Sequence.GROUP_VALUE:
	    return (kinds & GROUP_OK) != 0;
	  case Sequence.DOCUMENT_VALUE:
	  case Sequence.ATTRIBUTE_VALUE:
	    return false;
	  }
      }
    return true;
  }

  public static SeqPosition coerceForce(Object obj, int kinds)
  {
    SeqPosition pos = coerceOrNull(obj, kinds);
    if (pos == null)
      throw new ClassCastException("coerce from "+obj.getClass());
    return pos;
  }

  public static SeqPosition coerceOrNull(Object obj, int kinds)
  {
    SeqPosition pos;
    if (obj instanceof AbstractSequence)
      pos = new SeqPosition((AbstractSequence) obj, 0, false);
    else if (obj instanceof SeqPosition)
      pos = (SeqPosition) obj;
    else
      return null;
    return isInstance(pos.sequence, pos.ipos, pos.xpos, kinds) ? pos : null;
  }

  public static final ClassType typeSeqPosition = ClassType.make("gnu.lists.SeqPosition");
  public static final ClassType typeNodeType = ClassType.make("gnu.kawa.xml.NodeType");
  public static final NodeType nodeType = new NodeType("gnu.lists.SeqPosition");
  static final Method coerceMethod
    = typeNodeType.getDeclaredMethod("coerceForce", 2);

  public void writeExternal(ObjectOutput out) throws IOException
  {
    String name = getName();
    out.writeUTF(name == null ? "" : name);
    out.writeInt(kinds);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    String name = in.readUTF();
    if (name.length() > 0)
      setName(name);
    kinds = in.readInt();
  }
}
