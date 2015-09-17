package gnu.kawa.xml;
import gnu.kawa.reflect.*;
import gnu.bytecode.*;
import gnu.expr.*;
import java.io.*;

public class NodeSetType extends OccurrenceType
{
  public NodeSetType (Type itemType)
  {
    super(itemType, 0, -1);
  }

  public static Type getInstance (Type base)
  {
    return new NodeSetType(base);
  }

  public String toString ()
  {
    return super.toString()+"node-set";
  }

    @Override
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(getBase());
    }

    @Override
    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        setBase((Type) in.readObject());
    }
}
