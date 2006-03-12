package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.*;

public class syntax extends kawa.lang.Quote
{
  public static final syntax syntax = new syntax("syntax", false);
  public static final syntax quasiSyntax = new syntax("quasisyntax", true);

  public syntax (String name, boolean isQuasi)
  {
    super(name, isQuasi);
  }

  protected boolean expandColonForms ()
  {
    return false;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (! (form.cdr instanceof Pair)
	|| (form = (Pair) (form.cdr)).cdr != LList.Empty)
      return tr.syntaxError("syntax forms requires a single form");
    return coerceExpression(expand(form.car, isQuasi ? 1 : Quote.QUOTE_DEPTH, tr), tr);
  }

  protected Expression leaf (Object val, Translator tr)
  {
    return makeSyntax(val, tr);
  }

  static Expression makeSyntax (Object form, Translator tr)
  {
    SyntaxTemplate template = new SyntaxTemplate(form, null, tr);
    Expression matchArray = QuoteExp.nullExp;
    PatternScope patternScope = tr.patternScope;
    if (patternScope != null && patternScope.matchArray != null)
      matchArray = new ReferenceExp(patternScope.matchArray);
    Expression[] args = { new QuoteExp(template), matchArray };
    return new ApplyExp(ClassType.make("kawa.lang.SyntaxTemplate")
			.getDeclaredMethod("execute", 1),
			args);
  }
}
