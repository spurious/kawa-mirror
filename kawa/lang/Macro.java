package kawa.lang;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.lists.*;
import java.io.*;
import java.util.Hashtable;

public class Macro extends Syntax implements Printable, Externalizable
{
  public Object expander;

  java.util.Vector capturedIdentifiers;

  String capturedFields;

  /** The identifiers in the templates that are not pattern variable.
   * These need re-naming to be "hygienic". */
  String[] templateIdentifiers;

  /** Declarations captured at macro definition time.
   * The binding (if any) for templateIdentifiers[i] is capturedDeclarations[i]. */
  Object[] capturedDeclarations;

  public static Macro make (Declaration decl)
  {
    Macro mac = new Macro(decl.getSymbol());
    mac.capturedIdentifiers = new java.util.Vector ();
    mac.bind(decl);
    return mac;
  }

  public static Macro make (Object name, Procedure expander)
  {
    Macro mac = new Macro(name, expander);
    return mac;
  }

  public static Macro make (Object name, Procedure expander,
			    String[] templateIdentifiers,
			    String capturedFields)
  {
    Macro mac = new Macro(name, expander);
    mac.templateIdentifiers = templateIdentifiers;
    mac.capturedFields = capturedFields;
    return mac;
  }

  /** Capture field declarations from current macro-definition scope.
   * Used by kawa.standard.require when a macro is loaded.
   * See getUsedFieldsList which is called when the macro is exported.
   * @param table maps field names for Declarations. */
  public void captureDecls(Hashtable table)
  {
    if (capturedFields == null)
      return;
    int numFields = 0;
    for (int i = capturedFields.length();  --i >= 0; )
      if (capturedFields.charAt(i) == ';')
        numFields++;
    Object[] decls = new Declaration[numFields];
    int start = 0;
    for (int i = 0;  i < numFields; i++)
      {
        int end = capturedFields.indexOf(';', start);
        if (end > start)
          {
            String fieldName = capturedFields.substring(start, end);
            decls[i] = (Declaration) table.get(fieldName);
          }

        start = end + 1;
      }
    capturedDeclarations = decls;
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

  /** Copy constructor. */
  public Macro (Macro old)
  {
    name = old.name;
    expander = old.expander;
    capturedIdentifiers = old.capturedIdentifiers;
    templateIdentifiers = old.templateIdentifiers;
    capturedFields = old.capturedFields;
  }

  public Macro(Object name, Procedure expander)
  {
    super(name);
    //setType(thisType);
    this.expander = new QuoteExp(expander);
  }

  public Macro(Object name, Expression lexp)
  {
    super(name);
    this.expander = lexp;
  }

  public Macro(Object name)
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
	Procedure pr;
	Object exp = expander;
	if (exp instanceof Procedure)
	  pr = (Procedure) exp;
	else
	  {
	    if (! (exp instanceof Expression))
	      {
		Macro savedMacro = tr.currentMacroDefinition;
		tr.currentMacroDefinition = this;
		try
		  {
		    exp = tr.rewrite(exp);
		    expander = exp;
		  }
		finally
		  {
		    tr.currentMacroDefinition = savedMacro;
		  }
	      }
	    pr = (Procedure)
	      ((Expression) exp).eval(tr.getGlobalEnvironment());
	  }
        SyntaxForm sform = new SyntaxForm();
        sform.form = form;
        sform.tr = tr;
	Object expansion = pr.apply1(sform);
        return expansion;
      }
    catch (Throwable ex)
      {
        return tr.syntaxError("evaluating syntax transformer '"
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

  /** Given a list of cpatured declarations, get a String containing
   * a list for field names.
   * This is called when compiling (exporting) a macro.
   * Set set up the lexical scope when a macro is loaded,
   * we call captureDecls. */
  private static String getUsedFieldsList(Object[] templateDecls)
  {
    StringBuffer fieldNames = new StringBuffer();
    if (templateDecls != null)
      {
	int numDecls = templateDecls.length;
	for (int i = 0;  i < numDecls;  i++)
	  {
	    Object d = templateDecls[i];
	    if (d instanceof Declaration)
	      {
		Declaration decl = (Declaration) d;
		if (decl.field != null
		    && ! decl.getFlag(Declaration.IS_UNKNOWN))
		  fieldNames.append(decl.field.getName());
	      }
	    fieldNames.append(';');
	  }
      }
    return fieldNames.toString();
  }

  /**
   * @serialData Write the name followed by the expansion procedure,
   *   both using writeObject.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(((QuoteExp) expander).getValue());
    out.writeObject(templateIdentifiers);
    out.writeObject(Macro.getUsedFieldsList(capturedDeclarations));
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
    expander = new QuoteExp(in.readObject());
    templateIdentifiers = (String[]) in.readObject();
    capturedFields = (String) in.readObject();
  }
}
