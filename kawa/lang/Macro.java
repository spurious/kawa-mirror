package kawa.lang;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import java.io.*;

public class Macro extends Syntax implements Printable, Externalizable
{
  public Expression expander;

  public static Macro make (String name, Procedure expander)
  {
    Macro mac = new Macro(name, expander);
    return mac;
  }

  public void bind(Declaration decl)
  {
    decl.setSimple(false);
    decl.setFlag(Declaration.IS_CONSTANT | Declaration.IS_SYNTAX);
    decl.noteValue(new QuoteExp(this));
  }

  public void setExpander (Procedure expander)
  {
    this.expander = new QuoteExp(expander);
  }

  public Macro ()
  {
  }

  public Macro(String name, Procedure expander)
  {
    super(name);
    //setType(thisType);
    this.expander = new QuoteExp(expander);
  }

  public Macro(String name, Expression lexp)
  {
    super(name);
    this.expander = lexp;
  }

  public Macro(String name)
  {
    super(name);
  }

  public gnu.expr.Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.rewrite(expand(form, tr));
  }

  public String toString()
  {
    return "#<macro "+getName()+'>';
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<macro ");
    ps.print(getName());
    ps.print ('>');
  }

  public Object expand (Pair form, Translator tr)
  {
    try
      {
        Procedure pr = (Procedure) expander.eval(tr.getGlobalEnvironment());
        SyntaxForm sform = new SyntaxForm();
        sform.form = form;
        sform.tr = tr;
	Object expansion = pr.apply1(sform);
        return expansion;
      }
    catch (Exception ex)
      {
        return tr.syntaxError("evaluating syntax transformer `"
                              + getName() + "' threw " + ex);
      }
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                    ScopeExp defs, Translator tr)
  {
    String save_filename = tr.getFile();
    int save_line = tr.getLine();
    int save_column = tr.getColumn();
    Syntax saveSyntax = tr.currentSyntax;
    try
      {
	tr.setLine(st);
        tr.currentSyntax = this;
        return tr.scan_form(expand(st, tr), forms, defs);
      }
    finally
      {
	tr.setLine(save_filename, save_line, save_column);
        tr.currentSyntax = saveSyntax;
      }
  }

  /**
   * @serialData Write the name followed by the expansion procedure,
   *   both using writeObject.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(((QuoteExp) expander).getValue());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
    expander = new QuoteExp(in.readObject());
  }
}
