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

    Object literals_list = pair.car;
    int num_literals = LList.length (literals_list);
    if (num_literals < 0)
      return tr.syntaxError ("define-syntax:  invalid literals list");
    String[] literal_identifiers = new String [num_literals + 1];
    literal_identifiers[0] = null; // FIXME
    for (int i = 0;  i < num_literals;  i++)
      {
	Pair lit_pair = (Pair) literals_list;
	if (! (lit_pair.car instanceof String))
	  return tr.syntaxError ("define-syntax: non-symbol in literals list");
	literal_identifiers[i+1] = (String) lit_pair.car;
	literals_list = lit_pair.cdr;
      }

    SyntaxRules rules
      = new SyntaxRules (literal_identifiers, pair.cdr, tr);
    return new QuoteExp(rules);
  }
}
