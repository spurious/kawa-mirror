package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the "define-syntax" Scheme primitive.
 * Does not handle:
 * - literals are truly literal (we do not check that the binding at
 *   macro expansion time matches that at definition time)
 * @author	Per Bothner
 */

public class define_syntax extends Syntax
{
  public Expression rewrite (Object obj, Translator tr)
  {
    int obj_length = List.list_length (obj);
    if (obj_length != 2)
      return tr.syntaxError ("invalid define-syntax syntax");
    Pair pair = (Pair) obj;
    if (! (pair.car instanceof String))
      return tr.syntaxError ("define-syntax:  keyword is not a symbol");
    String keyword = (String) pair.car;

    pair = (Pair) pair.cdr;
    Object transformer_spec = pair.car;

    obj_length = List.list_length (transformer_spec);
    if (obj_length < 2)
      return tr.syntaxError ("define-syntax:  transformer spec missing or too short");

    pair = (Pair) transformer_spec;
    if (pair.car != SyntaxRules.syntaxRulesSymbol)
      return tr.syntaxError ("define-syntax:  no syntax-rules");
    pair = (Pair) pair.cdr;

    Object literals_list = pair.car;
    int num_literals = List.length (literals_list);
    if (num_literals < 0)
      return tr.syntaxError ("define-syntax:  invalid literals list");
    String[] literal_identifiers = new String [num_literals + 1];
    literal_identifiers[0] = keyword;
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
    // Add rules to translation environment.
    tr.addGlobal(keyword, rules);
    // Add rules to execution environment.
    SetExp result = new SetExp (keyword, new QuoteExp (rules));
    result.setDefining (true);
    return result;
  }
}
