// Copyright (c) 2009 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.lang;
import gnu.expr.*;
import gnu.mapping.Symbol;
import gnu.lists.*;

/**
 * Helper method and implementation classes for SyntaxForm.
 * @author Per Bothner
 */
public class SyntaxForms {

  public static Object makeForm (Object datum, TemplateScope scope)
  {
    if (datum instanceof Pair)
      return new PairSyntaxForm((Pair) datum, scope);
    if (datum == LList.Empty)
        return datum;
    return new SimpleSyntaxForm(datum, scope);
  }

  /** Create a syntax object with specified datum, and given syntatic context.
   * Used to implement datum->syntax-object in the syntax-case API.
   * @param template If this is a SyntaxForm, use its scope;
   *   otherwise use the current Compilation's current scope.
   *   (This means just returning the datum as-is.)
   * @param form The value (S-expression datum) to use.
   */
  public static Object makeWithTemplate (Object template, Object form)
  {
    if (form instanceof SyntaxForm)
      return (SyntaxForm) form;
    if (template instanceof SyntaxForm)
      {
        SyntaxForm sform = (SyntaxForm) template;
	if (form == sform.getDatum())
	  return sform;
	return fromDatum(form, sform);
      }
    return form;
  }

  public static boolean freeIdentifierEquals (SyntaxForm id1, SyntaxForm id2)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    return tr.lexical.lookup(id1.getDatum(), -1) == tr.lexical.lookup(id2.getDatum(), -1);
  }

  public static boolean isIdentifier (SyntaxForm form)
  {
    return form.getDatum() instanceof Symbol;
  }

 /** Make a SyntaxForm object with the same contextual information as this.
   * @param datum which used for the new syntax value.
   * Corresponds to the <code>datum-&gt;syntax-object</code> function.
   */
  public static Object fromDatum (Object datum, SyntaxForm template)
  {
    return SyntaxForms.makeForm(datum, template.getScope());
  }

  public static Object fromDatumIfNeeded (Object datum, SyntaxForm template)
  {
    if (datum == template.getDatum())
      return template;
    else if (datum instanceof SyntaxForm)
      return (SyntaxForm) datum;
    else
      return SyntaxForms.fromDatum(datum, template);
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

  public static final boolean DEBUGGING = true;

  public static String toString (SyntaxForm sform, String id)
  {
    StringBuilder sbuf = new StringBuilder("#<syntax");
    if (DEBUGGING && id != null)
      {
        sbuf.append('#');
        sbuf.append(id);
      }
    sbuf.append(' ');
    sbuf.append(sform.getDatum());
    if (DEBUGGING)
      {
        TemplateScope scope = sform.getScope();
        if (scope == null)
          {
            sbuf.append(" in null");
          }
        else
          {
            sbuf.append(" in #");
            sbuf.append(scope.id);
          }
      }
    sbuf.append(">");
    return sbuf.toString();
  }

  static class SimpleSyntaxForm implements SyntaxForm {
    private Object datum;
    private TemplateScope scope;
    
    // DEBUGGING:
    static int counter;
    int id = ++counter;

    SimpleSyntaxForm (Object datum, TemplateScope scope)
    {
       this.datum = datum;
       this.scope = scope;
    }

    public Object getDatum()
    {
      return datum;
    }

    public TemplateScope getScope()
    {
      return scope;
    }

    public String toString ()
    {
      String sid = DEBUGGING ? Integer.toString(id) : null;
      return SyntaxForms.toString(this, sid);
    }
  }

  static class PairSyntaxForm extends ImmutablePair implements SyntaxForm
  {
    private Pair datum;
    private TemplateScope scope;

    public PairSyntaxForm(Pair datum, TemplateScope scope)
    {
      this.datum = datum;
      this.scope = scope;
    }

    public Object getDatum()
    {
      return datum;
    }

    public TemplateScope getScope()
    {
      return scope;
    }

    public Object getCar ()
    {
      if (car == null)
        car = SyntaxForms.makeForm(datum.getCar(), scope);
      return car;
    }
    public Object getCdr ()
    {
      if (cdr == null)
        cdr = SyntaxForms.makeForm(datum.getCdr(), scope);
      return cdr;
    }
    public String toString ()
    {
      //String sid = DEBUGGING ? Integer.toString(id) : null;
      return SyntaxForms.toString(this, null);
    }
  }

  // TODO: static class VectorSyntaxForm ...
}
