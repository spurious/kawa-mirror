package kawa.lang;
import gnu.expr.*;
import gnu.mapping.Symbol;
import java.io.*;

/** A "syntatic closure" - a syntax form with its compilation environment. */

public class SyntaxForm implements Externalizable
{
  public Object form;

  public ScopeExp scope;

  // PairPosition pos;

  /*DEBUGGING:
  static int counter;
  int id = ++counter;
  */

  public SyntaxForm ()
  {
  }

  public static SyntaxForm make (Object form, ScopeExp scope)
  {
    SyntaxForm sf = new SyntaxForm();
    sf.form = form;
    sf.scope = scope;
    return sf;
  }

  public String toString ()
  {
    return "#<syntax "
      + form
      ///* DEBUGGING:
      // + " #" id +
      + " in #"+scope.id
      //*/
      +">";
  }

  /** Make a SyntaxForm object with the same contextual information as this.
   * @param the form used for the new syntax value.
   * Corresponds to the <code>datum->syntax-object</code> function.
   */
  public SyntaxForm fromDatum (Object form)
  {
    SyntaxForm sf = new SyntaxForm();
    sf.form = form;
    sf.scope = this.scope;
    return sf;
  }

  public SyntaxForm fromDatumIfNeeded (Object form)
  {
    if (form == this.form)
      return this;
    else if (form instanceof SyntaxForm)
      return (SyntaxForm) form;
    else
      return fromDatum(form);
  }

  public static Expression rewrite (Object x)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    return tr.rewrite(x);
  }

  public static Expression rewriteBody (Object x)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    return tr.rewrite_body(x);
  }

  public boolean isIdentifier ()
  {
    return form instanceof String || form instanceof Symbol;
  }

  public static boolean freeIdentifierEquals (SyntaxForm id1, SyntaxForm id2)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    return tr.lexical.lookup(id1.form, -1) == tr.lexical.lookup(id2.form, -1);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(form);
    out.writeObject(scope);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    form = in.readObject();
    scope = (ScopeExp) in.readObject();
  }
}
