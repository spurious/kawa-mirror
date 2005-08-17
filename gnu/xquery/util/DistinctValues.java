package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.xml.*;
import gnu.kawa.xml.*;

public class DistinctValues
{
  public static void distinctValues$X (Object values, NamedCollator coll,
                                       CallContext ctx)
  {
    DistinctValuesConsumer out
      = new DistinctValuesConsumer(coll, ctx.consumer);
    Values.writeValues(values, out);
  }

  /*
  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Object collation = ctx.getNextArg(null);
    ctx.lastArg();
    DistinctValuesConsumer out
      = new DistinctValuesConsumer(ctx.consumer);
    if (collation != null)
      {
	// FIXME
      }
    Values.writeValues(arg, out);
  }
  */
}

/*
class FilterValueConsumer implements Consumer
{
  Convert convert;

  public void writeChar (int v)
  {
    writeObject(convert.charToObject(v));
  }
  
  public void writeBoolean (boolean v)
  {
    writeObject(convert.booleanToObject(v));
  }

  public void writeFloat (float v)
  {
    writeObject(convert.floatToObject(v));
  }

  public void writeDouble (double v)
  {
    writeObject(convert.doubleToObject(v));
  }

  public void writeInt (int v)
  {
    writeObject(convert.intToObject(v));
  }

  public void writeLong (long v)
  {
    writeObject(convert.longToObject(v));
  }
}
*/

class DistinctValuesHashTable extends GeneralHashTable
{
  NamedCollator collator;

  public DistinctValuesHashTable (NamedCollator collator)
  {
    this.collator = collator;
  }

  public int hash (Object key)
  {
    // This mostly-works, but isn't reliable FIXME.
    // Since DFloNum's hasCode returns the integer value,
    // we correctly hash decimal and integers together.
    // However, there are other problems, including that we ignore collation.
    return key == null ? 0 : key.hashCode();
  }

  public boolean matches (Object value1, Object value2)
  {
    if (value1 == value2)
      return true;
    return Compare.apply(Compare.TRUE_IF_EQU, value1, value2, collator);
  }
}

class DistinctValuesConsumer extends FilterConsumer implements PositionConsumer
{
  DistinctValuesHashTable table;

  public DistinctValuesConsumer (NamedCollator collator, Consumer out)
  {
    super(out);
    table = new DistinctValuesHashTable(collator);
  }

  public void consume(SeqPosition position)
  {
    writeObject(position);
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    writeObject(((NodeTree) seq).typedValue(ipos));
  }

  public void writeObject (Object value)
  {
    if (value instanceof Values)
      {
	Values.writeValues(value, this);
	return;
      }
    if (value instanceof KNode)
      {
        KNode node = (KNode) value;
        writeObject(((NodeTree) node.sequence).typedValue(node.ipos));
        return;
      }
    Object old = table.get(value, null);
    if (old != null)
      return;
    table.put(value, value);
    base.writeObject(value);
  }
}
