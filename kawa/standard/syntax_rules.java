package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;

/** Implement the standard Scheme "syntax-rules" form. */

public class syntax_rules extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair pair = (Pair) form.cdr;

    Object[] literal_identifiers
      = SyntaxPattern.getLiteralsList(pair.car, null, tr);
    SyntaxRules rules
      = new SyntaxRules (literal_identifiers, pair.cdr, tr);
    return new QuoteExp(rules);
  }
}
