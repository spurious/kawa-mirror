package kawa.lang;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.util.*;

public class Macro extends Syntax
{
  public Object transformer;

  public static ClassType thisType;
  static public Method makeMethod;

  public static Macro make (String name, Procedure expander)
  {
    Macro mac = new Macro(name, expander);
    return mac;
  }

  public static Method getMakeMethod()
  {
    if (thisType == null)
      thisType = ClassType.make("kawa.lang.Macro");
    if (makeMethod == null)
      {
        Type[] args = { Compilation.javaStringType, Compilation.typeProcedure };
        makeMethod = thisType.addMethod("make", args,
                                        thisType, Access.STATIC|Access.PUBLIC);
      }
    return makeMethod;
  }

  public Macro(String name, Procedure expander)
  {
    super(name);
    this.value = new QuoteExp(expander);
  }

  public Macro(String name, Expression lexp)
  {
    super(name);
    this.value = lexp;
  }

  public Macro(String name, Object transformer)
  {
    super(name);
    this.transformer = transformer;
  }

  public gnu.expr.Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.rewrite(expand(form, tr));
  }

  public String toString()
  {
    return "#<macro "+getName()+'>';
  }

  public Object expand (Pair form, Translator tr)
  {
    try
      {
        Expression value = getValue();
        Procedure expander = (Procedure) value.eval(tr.getGlobalEnvironment());
        SyntaxForm sform = new SyntaxForm();
        sform.form = form;
        sform.tr = tr;
	Object expansion = expander.apply1(sform);
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
    String save_filename = tr.current_filename;
    int save_line = tr.current_line;
    int save_column = tr.current_column;
    Syntax saveSyntax = tr.currentSyntax;
    try
      {
        if (st instanceof PairWithPosition)
          {
            PairWithPosition ppair = (PairWithPosition) st;
            tr.current_filename = ppair.getFile ();
            tr.current_line = ppair.getLine ();
            tr.current_column = ppair.getColumn ();
          }
        tr.currentSyntax = this;
        return tr.scan_form(expand(st, tr), forms, defs);
      }
    finally
      {
        tr.current_filename = save_filename;
        tr.current_line = save_line;
        tr.current_column = save_column;
        tr.currentSyntax = saveSyntax;
      }
  }
}
