package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.bytecode.ArrayType;
import gnu.mapping.*;
import gnu.expr.*;

public class SyntaxRules extends Procedure1 implements Printable, Compilable
{
  /** The list of literals identifiers.
   * The 0'th element is name of the macro being defined;
   * the rest are as specied in the syntax-rules form. */
  String[] literal_identifiers;

  SyntaxRule[] rules;

  /* The largest (num_variables+template_identifier.length) for any rule. */
  int maxVars = 0;

  static public String syntaxRulesSymbol = "syntax-rules"; // OBSOLETE

  private void calculate_maxVars ()
  {
    for (int i = rules.length;  --i >= 0; )
      {
	int size = rules[i].num_variables +
	  rules[i].template_identifiers.length;
	if (size > maxVars)
	  maxVars = size;
      }
  }

  public SyntaxRules (String[] literal_identifiers, SyntaxRule[] rules)
  {
    this.literal_identifiers = literal_identifiers;
    this.rules = rules;
    calculate_maxVars ();    
  }

  public SyntaxRules (String[] literal_identifiers, Object rules,
		      Translator tr)
  {
    this.literal_identifiers = literal_identifiers;
    int rules_count = List.list_length (rules);
    if (rules_count <= 0)
      {
	rules_count = 0;
	tr.syntaxError ("missing or invalid syntax-rules");
      }
    this.rules = new SyntaxRule [rules_count];
    Pair rules_pair;
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

	String save_filename = tr.current_filename;
	int save_line = tr.current_line;
	int save_column = tr.current_column;

	try
	  {
	    if (syntax_rule_pair instanceof PairWithPosition)
	      {
		PairWithPosition pp = (PairWithPosition) syntax_rule_pair;
		tr.current_filename = pp.getFile ();
		tr.current_line = pp.getLine ();
		tr.current_column = pp.getColumn ();
	      }
	    if (! (syntax_rule_pair.cdr instanceof Pair))
	      {
		tr.syntaxError ("missing template in " + i + "'th syntax rule");
		return;
	      }
	    syntax_rule_pair = (Pair) syntax_rule_pair.cdr;
	    if (syntax_rule_pair.cdr != List.Empty)
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
            literal_identifiers[0] = (String) ((Pair)pattern).car;
	    pattern = ((Pair) pattern).cdr;

	    Pattern translated_pattern
	      = translate_pattern (pattern, literal_identifiers,
				   pattern_names, pattern_nesting_buffer,
				   0, tr);
	    String pattern_nesting = pattern_nesting_buffer.toString ();

	    this.rules[i]
	      = new SyntaxRule (translated_pattern, pattern_nesting,
				pattern_names, template, tr);
	    /* DEBUGGING:
	    OutPort err = OutPort.errDefault();
	    err.println ("{translated template:");
	    this.rules[i].print_template_program (err);
	    err.println ('}');
	    */
	  }
	finally
	  {
	    tr.current_filename = save_filename;
	    tr.current_line = save_line;
	    tr.current_column = save_column;
	  }
      }
    calculate_maxVars ();    
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
					   String[] literal_identifiers,
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
		if (cdr_pair.cdr != List.Empty)
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
    else if (pattern instanceof String)
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
	    Object expansion = rule.execute_template (vars, tr, form);
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

  static public ClassType thisType;
  static public Method initSyntaxRulesMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (SyntaxRule.thisType == null)
	SyntaxRule.thisType = ClassType.make("kawa.lang.SyntaxRule");
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.lang.SyntaxRules");
	Type[] argTypes = new Type[2];
	argTypes[0] = comp.symbolArrayType;
	argTypes[1] = new ArrayType (SyntaxRule.thisType);
	initSyntaxRulesMethod
	  = thisType.addMethod ("<init>", argTypes,
				      Type.void_type, Access.PUBLIC);
      }
    Literal literal = new Literal (this, thisType, comp);
    comp.findLiteral (literal_identifiers);
    for (int i = 0;  i < rules.length;  i++)
      comp.findLiteral (rules[i]);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    literal.check_cycle ();
    int len = rules.length;
    // Allocate the SyntaxRules object
    code.emitNew(thisType);
    code.emitDup(1);  // dup
    comp.emitLiteral (literal_identifiers);
    code.emitPushInt(len);
    code.emitNewArray(SyntaxRule.thisType);
    // Initialize the SyntaxRule elements.
    for (int i = 0;  i < len;  i++)
      {
	code.emitDup(1);  // dup
	code.emitPushInt(i);
	comp.emitLiteral (rules[i]);
	// Stack contents:  ..., this,this,literals, array, array, i, rules[i]
	code.emitArrayStore(SyntaxRule.thisType);
	// Stack contents:  ..., this, this, literals, array
      }

    // Stack contents:  ..., this, this, literals, array
    code.emitInvokeSpecial(initSyntaxRulesMethod);
  }
}
