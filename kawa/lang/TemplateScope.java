package kawa.lang;
import gnu.expr.*;
import java.io.*;

/** A scope created when expanding a SyntaxTemplate.
 * This is used to ensure proper "hygiene". */

public class TemplateScope extends LetExp implements Externalizable
{
  public TemplateScope ()
  {
    super(null);
  }

  public TemplateScope (ScopeExp outer)
  {
    super(null);
    this.outer = outer;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(outer);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    outer = (ScopeExp) in.readObject();
  }
}
