package kawa.lang;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;
import codegen.ArrayType;

public class SyntaxRules extends Syntax implements Printable, Compilable
{
  /** The list of literals identifiers.
   * The 0'th element is name of the macro being defined;
   * the rest are as specied in the syntax-rules form. */
  Symbol[] literal_identifiers;

  SyntaxRule[] rules;

  /* The largest (num_variables+template_identifier.length) for any rule. */
  int maxVars = 0;

  static public Symbol syntaxRulesSymbol = Symbol.make ("syntax-rules");

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

  public SyntaxRules (Symbol[] literal_identifiers, SyntaxRule[] rules)
  {
    this.literal_identifiers = literal_identifiers;
    this.rules = rules;
    calculate_maxVars ();    
  }

  public SyntaxRules (Symbol[] literal_identifiers, Object rules,
		      Interpreter interp)
  {
    this.literal_identifiers = literal_identifiers;
    int rules_count = List.list_length (rules);
    if (rules_count <= 0)
      {
	rules_count = 0;
	interp.syntaxError ("missing or invalid syntax-rules");
      }
    this.rules = new SyntaxRule [rules_count];
    Pair rules_pair;
    for (int i = 0;  i < rules_count;  i++, rules = rules_pair.cdr)
      {
	rules_pair = (Pair) rules;

	Object syntax_rule = rules_pair.car;
	if (! (syntax_rule instanceof Pair))
	  {
	    interp.syntaxError ("missing pattern in " + i + "'th syntax rule");
	    continue;
	  }
	Pair syntax_rule_pair = (Pair) syntax_rule;
	Object pattern = syntax_rule_pair.car;
	if (! (syntax_rule_pair.cdr instanceof Pair))
	  {
	    interp.syntaxError ("missing template in " + i + "'th syntax rule");
	    continue;
	  }
	syntax_rule_pair = (Pair) syntax_rule_pair.cdr;
	if (syntax_rule_pair.cdr != List.Empty)
	  {
	    interp.syntaxError ("junk after " + i + "'th syntax rule");
	    continue;
	  }
	Object template = syntax_rule_pair.car;


	// For the i'th pattern variable, pattern_names.elementAt(i)
	// is the name of the variable, and
	// (int) patter_nesting.charAt (i) is the nesting (in terms
	// of number of ellipsis that indicate the variable is repeated).
	StringBuffer pattern_nesting_buffer = new StringBuffer ();
	java.util.Vector pattern_names = new java.util.Vector ();
	if (! (pattern instanceof Pair)
	    || ! (((Pair)pattern).car instanceof Symbol))
	  interp.syntaxError ("pattern does not start with name");
	else
	  pattern = ((Pair) pattern).cdr;


	Pattern translated_pattern
	  = translate_pattern (pattern, literal_identifiers,
			       pattern_names, pattern_nesting_buffer,
			       0, interp);
	String pattern_nesting = pattern_nesting_buffer.toString ();

	this.rules[i] = new SyntaxRule (translated_pattern, pattern_nesting,
					pattern_names,
					template, interp);
	/* DEBUGGING:
	System.err.println ("{translated template:");
	this.rules[i].print_template_program (System.err);
	System.err.println ('}');
	*/
      }
    calculate_maxVars ();    
  }

  /** Recursively translate a pattern in a syntax-rule to a Pattern object.
   * @param pattern the the pattern to translate
   * @param literal_identifiers the literals of the syntax-rule
   * @param pattern_name (output) the pattern variables in the pattern
   * @param pattern_nesting (output) the nesting of each pattern variable
   * @param nesting the depth of ... we are inside
   * @param interp  the current interpreter
   * @return the translated Pattern
   */
  public static Pattern translate_pattern (Object pattern,
					   Symbol[] literal_identifiers,
					   java.util.Vector pattern_names,
					   StringBuffer pattern_nesting,
					   int nesting, Interpreter interp)
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
		  interp.syntaxError ("junk follows ... in syntax-rule pattern");
		Pattern car_pat
		  = translate_pattern (pair.car, literal_identifiers,
				       pattern_names, pattern_nesting,
				       nesting + 1, interp);
		return new ListRepeatPat (car_pat);
	      }
	  }
	Pattern car_pat = translate_pattern (pair.car, literal_identifiers,
					     pattern_names, pattern_nesting,
					     nesting, interp);
	return new PairPat (car_pat,
			    translate_pattern (pair.cdr, literal_identifiers,
					       pattern_names, pattern_nesting,
					       nesting, interp));
      }
    else if (pattern instanceof Symbol)
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
	  interp.syntaxError ("duplicated pattern variable " + pattern);
	pattern_names.addElement (pattern);
	pattern_nesting.append ((char) nesting);
	return new AnyPat ();
      }
    else
      return new EqualPat (pattern);
  }

  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    Object[] vars = new Object[maxVars];
    for (int i = 0;  i < rules.length;  i++)
      {
	SyntaxRule rule = rules[i];
	// check that literals have correct binding !!
	if (rule.pattern.match (obj, vars, 0) >= 0)
	  {
	    /* DEBUGGING:
	    System.err.print ("{Matched variables: ");
	    for (int j = 0;  j < rule.num_variables;  j++)
	      {
		if (j > 0)  System.err.print ("; ");
		System.err.print (j + ": ");
		kawa.lang.print.print (vars[j], System.err);
	      }
	    System.err.println ('}');
	    */

	    Object expansion = rule.execute_template (vars, interp);
	    /* DEBUGGING:
	    System.err.print ("{Expanded macro: ");
	    kawa.lang.print.print (expansion, System.err);
	    System.err.println ('}');
	    */
	    return interp.rewrite (expansion);
	  }
      }
    return interp.syntaxError ("non-matching macro application");
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print ("#<macro " + literal_identifiers[0] + ">");
  }

  static public ClassType thisType;
  static public Method initSyntaxRulesMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (SyntaxRule.thisType == null)
	SyntaxRule.thisType = new ClassType ("kawa.lang.SyntaxRule");
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.lang.SyntaxRules");
	Type[] argTypes = new Type[2];
	argTypes[0] = comp.symbolArrayType;
	argTypes[1] = new ArrayType (SyntaxRule.thisType);
	initSyntaxRulesMethod
	  = thisType.new_method ("<init>", argTypes,
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
    literal.check_cycle ();
    int len = rules.length;
    // Allocate the SyntaxRules object
    comp.method.compile_new (thisType);
    comp.method.compile_dup (1);  // dup
    comp.emitLiteral (literal_identifiers);
    comp.method.compile_push_int (len);
    comp.method.compile_new_array (SyntaxRule.thisType);
    // Initialize the SyntaxRule elements.
    for (int i = 0;  i < len;  i++)
      {
	comp.method.compile_dup (1);  // dup
	comp.method.compile_push_int (i);
	comp.emitLiteral (rules[i]);
	// Stack contents:  ..., this,this,literals, array, array, i, rules[i]
	comp.method.compile_array_store (SyntaxRule.thisType);
	// Stack contents:  ..., this, this, literals, array
      }

    // Stack contents:  ..., this, this, literals, array
    comp.method.compile_invoke_nonvirtual (initSyntaxRulesMethod);

    literal.flags |= Literal.ALLOCATED|Literal.INITIALIZED;
  }
}
