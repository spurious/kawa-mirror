package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;

/** Implement the standard Scheme "syntax-rules" form. */

public class syntax_rules extends Syntax
{
  public static final syntax_rules syntax_rules = new syntax_rules();
  static { syntax_rules.setName("syntax-rules"); }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair pair = (Pair) form.getCdr();

    Object[] literal_identifiers
      = SyntaxPattern.getLiteralsList(pair.getCar(), null, tr);
    SyntaxRules rules
      = new SyntaxRules (literal_identifiers, pair.getCdr(), tr);
    return new QuoteExp(rules);
  }
}
