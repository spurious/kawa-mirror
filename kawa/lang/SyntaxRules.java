package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import java.io.*;

public class SyntaxRules extends Procedure1 implements Printable, Externalizable 
{
  /** The list of literals identifiers.
   * The 0'th element is name of the macro being defined;
   * the rest are as specied in the syntax-rules form. */
  Object[] literal_identifiers;

  SyntaxRule[] rules;

  /* The largest (num_variables+template_identifier.length) for any rule. */
  int maxVars = 0;

  private void calculate_maxVars (int template_identifiers_length)
  {
    for (int i = rules.length;  --i >= 0; )
      {
	int size = rules[i].num_variables + template_identifiers_length;
	if (size > maxVars)
	  maxVars = size;
      }
  }

  public SyntaxRules ()
  {
  }

  public SyntaxRules (Object[] literal_identifiers, SyntaxRule[] rules,
		      int template_identifiers_length)
  {
    this.literal_identifiers = literal_identifiers;
    this.rules = rules;
    calculate_maxVars(template_identifiers_length);    
  }

  public SyntaxRules (Object[] literal_identifiers, Object rules,
		      Translator tr)
  {
    this.literal_identifiers = literal_identifiers;
    int rules_count = LList.listLength(rules, false);
    if (rules_count <= 0)
      {
	rules_count = 0;
	tr.syntaxError ("missing or invalid syntax-rules");
      }
    this.rules = new SyntaxRule [rules_count];
    Pair rules_pair;
    Macro macro = tr.currentMacroDefinition;
    java.util.Vector capturedIdentifiers = macro.capturedIdentifiers;
    for (int i = 0;  i < rules_count;  i++, rules = rules_pair.cdr)
      {
	rules_pair = (Pair) rules;

	Object syntax_rule = rules_pair.car;
	if (! (syntax_rule instanceof Pair))
	  {
	    tr.syntaxError ("missing pattern in " + i + "'th syntax rule");
	    return;
	  }
	Pair syntax_rule_pair = (Pair) syntax_rule;
	Object pattern = syntax_rule_pair.car;

	String save_filename = tr.getFile();
	int save_line = tr.getLine();
	int save_column = tr.getColumn();

	try
	  {
	    tr.setLine(syntax_rule_pair);
	    if (! (syntax_rule_pair.cdr instanceof Pair))
	      {
		tr.syntaxError ("missing template in " + i + "'th syntax rule");
		return;
	      }
	    syntax_rule_pair = (Pair) syntax_rule_pair.cdr;
	    if (syntax_rule_pair.cdr != LList.Empty)
	      {
		tr.syntaxError ("junk after "+i+"'th syntax rule");
		return;
	      }
	    Object template = syntax_rule_pair.car;

	    // For the i'th pattern variable, pattern_names.elementAt(i)
	    // is the name of the variable, and
	    // (int) pattern_nesting.charAt (i) is the nesting (in terms
	    // of number of ellipsis that indicate the variable is repeated).
	    StringBuffer pattern_nesting_buffer = new StringBuffer ();
	    java.util.Vector pattern_names = new java.util.Vector ();
	    if (! (pattern instanceof Pair)
		|| ! (((Pair)pattern).car instanceof String))
	      {
		tr.syntaxError ("pattern does not start with name");
		return;
	      }
            literal_identifiers[0] = ((Pair)pattern).car;
	    pattern = ((Pair) pattern).cdr;

	    Pattern translated_pattern
	      = translate_pattern (pattern, literal_identifiers,
				   pattern_names, pattern_nesting_buffer,
				   0, tr);
	    String pattern_nesting = pattern_nesting_buffer.toString ();

	    this.rules[i]
	      = new SyntaxRule (translated_pattern, pattern_nesting,
				pattern_names, template,
				capturedIdentifiers, tr);
	    /* DEBUGGING:
	    OutPort err = OutPort.errDefault();
	    err.println ("{translated template:");
	    this.rules[i].print_template_program (template_identifiers, err);
	    err.println ('}');
	    */
	  }
	finally
	  {
	    tr.setLine(save_filename, save_line, save_column);
	  }
      }
    int num_identifiers = capturedIdentifiers.size();
    calculate_maxVars (num_identifiers);
    macro.templateIdentifiers = new String[num_identifiers];
    capturedIdentifiers.copyInto(macro.templateIdentifiers);
    macro.capturedDeclarations = new Object[num_identifiers];
    for (int j = num_identifiers;  --j >= 0; )
      {
	String name = macro.templateIdentifiers[j];
	Object binding = tr.environ.get(name);
	if (binding instanceof Declaration)
	  {
	    Declaration decl = (Declaration) binding;
	    if (! decl.getFlag(Declaration.IS_UNKNOWN))
              {
                decl.setCanRead(true);
                decl.setFlag(Declaration.EXTERNAL_ACCESS);
	      }
	  }
	macro.capturedDeclarations[j] = binding;
      }
  }

  /** Recursively translate a pattern in a syntax-rule to a Pattern object.
   * @param pattern the the pattern to translate
   * @param literal_identifiers the literals of the syntax-rule
   * @param pattern_name (output) the pattern variables in the pattern
   * @param pattern_nesting (output) the nesting of each pattern variable
   * @param nesting the depth of ... we are inside
   * @param tr  the current Translator
   * @return the translated Pattern
   */
  public static Pattern translate_pattern (Object pattern,
					   Object[] literal_identifiers,
					   java.util.Vector pattern_names,
					   StringBuffer pattern_nesting,
					   int nesting, Translator tr )
  {
    if (pattern instanceof Pair)
      {
	Pair pair = (Pair) pattern;
	if (pair.cdr instanceof Pair)
	  {
	    Pair cdr_pair = (Pair) pair.cdr;
	    if (cdr_pair.car == SyntaxRule.dots3)
	      {
		if (cdr_pair.cdr != LList.Empty)
		  tr.syntaxError ("junk follows ... in syntax-rule pattern");
		Pattern car_pat
		  = translate_pattern (pair.car, literal_identifiers,
				       pattern_names, pattern_nesting,
				       nesting + 1, tr);
		return new ListRepeatPat (car_pat);
	      }
	  }
	Pattern car_pat = translate_pattern (pair.car, literal_identifiers,
					     pattern_names, pattern_nesting,
					     nesting, tr);
	return new PairPat (car_pat,
			    translate_pattern (pair.cdr, literal_identifiers,
					       pattern_names, pattern_nesting,
					       nesting, tr));
      }
    else if (pattern instanceof String || pattern instanceof Symbol)
      {
	for (int i = literal_identifiers.length;  --i >= 0; )
	  {
	    // NOTE - should also generate check that the binding of the
	    // pattern at macro definition time matches that at macro
	    // application type.  However, we currently only support
	    // define-syntax (and not let[rec]-syntax) so it is not necessary.
	    if (literal_identifiers[i] == pattern)
	      return new EqualPat (pattern);
	  }
	if (pattern_names.contains (pattern))
	  tr.syntaxError ("duplicated pattern variable " + pattern);
	pattern_names.addElement (pattern);
	pattern_nesting.append ((char) nesting);
	return new AnyPat ();
      }
    else
      return new EqualPat (pattern);
  }

  public Object apply1 (Object arg)
  {
    SyntaxForm sform;
    Pair form;
    try
      {
        sform = (SyntaxForm) arg;
        form = (Pair) sform.form;
      }
   catch (ClassCastException ex)
      {
        throw WrongType.make(ex, this, 0);
      }  
    return expand(form, sform.tr);
  }

  public Object expand (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    Object[] vars = new Object[maxVars];
    Macro macro = (Macro) tr.getCurrentSyntax();
    int num_identifiers = macro.templateIdentifiers.length;
    for (int i = 0;  i < rules.length;  i++)
      {
	SyntaxRule rule = rules[i];
	// check that literals have correct binding - FIXME!!
	if (rule.pattern.match (obj, vars, 0))
	  {
	    /* DEBUGGING:
	    OutPort err = OutPort.errDefault();
	    err.print ("{Matched variables: ");
	    for (int j = 0;  j < rule.num_variables;  j++)
	      {
		if (j > 0)  err.print ("; ");
		err.print (j + ": ");
		SFormat.print (vars[j], err);
	      }
	    err.println ('}');
	    */

	    int[] indexes = new int[rule.max_nesting];
	    for (int j = 0;  j < num_identifiers;  j++)
	      {
		String name = macro.templateIdentifiers[j];
		Symbol renamed_symbol = new Symbol(name);
		vars[rule.num_variables + j] = renamed_symbol;
		Object captured = macro.capturedDeclarations == null ? null
		  : macro.capturedDeclarations[j];
		tr.environ.put(renamed_symbol, captured == null ? name : captured);
	      }
	    Object expansion = rule.execute_template (0, vars, 0, indexes, tr, form);

	    /* DEBUGGING:
	    err.print("{Expansion of ");
	    err.print(literal_identifiers[0]);
	    err.print(": ");
	    SFormat.print(expansion, err);
	    err.println('}');
	    err.flush();
	    */
	    return expansion;
	  }
      }
    return tr.syntaxError ("no matching syntax-rule for "
			   + literal_identifiers[0]);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<macro " + literal_identifiers[0] + ">");
  }

  /**
   * @serialData Write literal_identifiers followed by rules,
   *   using writeObject.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(literal_identifiers);
    out.writeObject(rules);
    out.writeInt(maxVars);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    literal_identifiers = (Object[]) in.readObject();
    rules = (SyntaxRule[]) in.readObject();
    maxVars = in.readInt();
  }
}
