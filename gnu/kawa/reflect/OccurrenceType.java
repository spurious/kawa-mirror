// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.Values;
import gnu.lists.*;
import java.io.*;
import gnu.expr.*;

/** A type that matches some number of repetitions of a basetype. */

public class OccurrenceType extends ObjectType
  implements Externalizable, TypeValue
{
  Type base;
  int minOccurs;
  int maxOccurs;

  public OccurrenceType (Type base, int minOccurs, int maxOccurs)
  {
    this.base = base;
    this.minOccurs = minOccurs;
    this.maxOccurs = maxOccurs;
  }

  public Type getImplementationType()
  {
    return Type.pointer_type;
  }

  public int compare(Type other)
  {
    return -2;
  }

  public Object coerceFromObject (Object obj)
  {
    if (! isInstance(obj))
      throw new ClassCastException();
    return obj;
  }

  public boolean isInstance (Object obj)
  {
    if (obj instanceof Values)
      {
	Values vals = (Values) obj;
	int pos = vals.startPos();
	int n = 0;
	if (base instanceof ElementPredicate)
	  {
	    ElementPredicate pred = (ElementPredicate) base;
	    for (;;)
	      {
		boolean matches;
		matches = pred.isInstancePos(vals, pos);
		pos = vals.nextPos(pos);
		if (pos == 0)
		  {
		    return n >= minOccurs
		      && (maxOccurs < 0 || n <= maxOccurs);
		  }
		if (! matches)
		  return false;
		n++;
	      }
	  }
	else
	  {

	    for (;;)
	      {
		pos = vals.nextPos(pos);
		if (pos == 0)
		  {
		    return n >= minOccurs
		      && (maxOccurs < 0 || n <= maxOccurs);
		  }
		Object value = vals.getPosPrevious(pos);
		if (! base.isInstance(value))
		  return false;
		n++;
	      }
	  }
      }
    else
      {
	if (minOccurs > 1 || maxOccurs == 0)
	  return false;
	return base.isInstance(obj);
      }
  }

  public void emitTestIf(Variable incoming, Declaration decl,
			 Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    if (decl != null)
      {
	code.emitDup();
	decl.compileStore(comp);
      }
    comp.compileConstant(this);
    code.emitSwap();
    code.emitInvokeVirtual(isInstanceMethod);
    code.emitIfIntNotZero();
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(base);
    out.writeInt(minOccurs);
    out.writeInt(maxOccurs);
  }

  public String toString ()
  {
    String b = base.toString();
    boolean parens = b == null || b.indexOf(' ') >= 0;
    StringBuffer sbuf = new StringBuffer();
    if (parens)
      sbuf.append('(');
    sbuf.append(b);
    if (parens)
      sbuf.append(')');
    if (minOccurs == 1 && maxOccurs == 1)
      ;
    else if (minOccurs == 0 && maxOccurs == 1)
      sbuf.append('?');
    else if (minOccurs == 1 && maxOccurs == -1)
      sbuf.append('+');
    else if (minOccurs == 0 && maxOccurs == -1)
      sbuf.append('*');
    else
      {
	sbuf.append('{');
	sbuf.append(minOccurs);
	sbuf.append(',');
	if (maxOccurs >= 0)
	  sbuf.append(maxOccurs);
	else
	  sbuf.append('*');
	sbuf.append('}');
      }
    return sbuf.toString();
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    base = (Type) in.readObject();
    minOccurs = in.readInt();
    maxOccurs = in.readInt();
  }

  public static final ClassType typeOccurrenceType
    = ClassType.make("gnu.kawa.reflect.OccurrenceType");
  static final Method isInstanceMethod
    = typeOccurrenceType.getDeclaredMethod("isInstance", 1);
}
