package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import java.io.*;
import gnu.text.*;

public class SyntaxRules extends Procedure1 implements Printable, Externalizable 
{
  /** The list of literal identifiers.
   * The 0'th element is name of the macro being defined;
   * the rest are as specied in the syntax-rules form. */
  Object[] literal_identifiers;

  SyntaxRule[] rules;

  /* The largest (num_variables+template_identifier.length) for any rule. */
  int maxVars = 0;

  public SyntaxRules ()
  {
  }

  /** The compiler generates calls to this constructor. */
  public SyntaxRules (Object[] literal_identifiers, SyntaxRule[] rules,
		      int maxVars)
  {
    this.literal_identifiers = literal_identifiers;
    this.rules = rules;
    this.maxVars = maxVars;
  }

  public SyntaxRules (Object[] literal_identifiers, Object srules,
		      Translator tr)
  {
    this.literal_identifiers = literal_identifiers;
    int rules_count = Translator.listLength(srules);
    if (rules_count < 0)
      {
	rules_count = 0;
	tr.syntaxError ("missing or invalid syntax-rules");
      }
    this.rules = new SyntaxRule [rules_count];
    Pair rules_pair;
    // SyntaxForm, if any, wrapping rest of rules list.
    SyntaxForm rules_syntax = null;
    for (int i = 0;  i < rules_count;  i++, srules = rules_pair.getCdr())
      {
	while (srules instanceof SyntaxForm)
	  {
	    rules_syntax = (SyntaxForm) srules;
	    srules = rules_syntax.getDatum();
	  }
	rules_pair = (Pair) srules;

	// SyntaxForm, if any, wrapping the current rule.
	SyntaxForm rule_syntax = rules_syntax;
	Object syntax_rule = rules_pair.getCar();
	while (syntax_rule instanceof SyntaxForm)
	  {
	    rule_syntax = (SyntaxForm) syntax_rule;
	    syntax_rule = rule_syntax.getDatum();
	  }
	if (! (syntax_rule instanceof Pair))
	  {
	    tr.syntaxError ("missing pattern in " + i + "'th syntax rule");
	    return;
	  }
	// SyntaxForm, if any, wrapping the current rule's pattern.
	SyntaxForm pattern_syntax = rule_syntax;
	Pair syntax_rule_pair = (Pair) syntax_rule;
	Object pattern = syntax_rule_pair.getCar();

	String save_filename = tr.getFileName();
	int save_line = tr.getLineNumber();
	int save_column = tr.getColumnNumber();

	try
	  {
	    // SyntaxForm, if any, wrapping the current rule's template.
	    SyntaxForm template_syntax = rule_syntax;
	    tr.setLine(syntax_rule_pair);
	    syntax_rule = syntax_rule_pair.getCdr();
	    while (syntax_rule instanceof SyntaxForm)
	      {
		template_syntax = (SyntaxForm) syntax_rule;
		syntax_rule = template_syntax.getDatum();
	      }
	    if (! (syntax_rule instanceof Pair))
	      {
		tr.syntaxError ("missing template in " + i + "'th syntax rule");
		return;
	      }
	    syntax_rule_pair = (Pair) syntax_rule;
	    if (syntax_rule_pair.getCdr() != LList.Empty)
	      {
		tr.syntaxError ("junk after "+i+"'th syntax rule");
		return;
	      }
	    Object template = syntax_rule_pair.getCar();

	    PatternScope patternScope = PatternScope.push(tr);
	    tr.push(patternScope);

	    while (pattern instanceof SyntaxForm)
	      {
		pattern_syntax = (SyntaxForm) pattern;
		pattern = pattern_syntax.getDatum();
	      }

	    StringBuffer programbuf = new StringBuffer();

	    // In R5RS syntax-rules, the initial name is neither a
	    // pattern variable or a literal identifier, so ingore it.
	    if (pattern instanceof Pair)
	      {
		// ?? FIXME
		literal_identifiers[0] = ((Pair)pattern).getCar();

		Pair p = (Pair) pattern;
		programbuf.append((char) ((1 << 3) | SyntaxPattern.MATCH_PAIR));
		programbuf.append((char) SyntaxPattern.MATCH_IGNORE);
		pattern = p.getCdr();
	      }
	    else
	      {
		// Identifier macro? FIXME
		tr.syntaxError ("pattern does not start with name");
		return;
	      }
	    SyntaxPattern spattern = new SyntaxPattern(programbuf, pattern,
					     pattern_syntax, literal_identifiers, tr);

	    this.rules[i] = new SyntaxRule(spattern,
					   template, template_syntax, tr);

	    PatternScope.pop(tr);
	    tr.pop();
	  }
	finally
	  {
	    tr.setLine(save_filename, save_line, save_column);
	  }
      }

    // Calculate maxVars:
    for (int i = this.rules.length;  --i >= 0; )
      {
	int size = this.rules[i].patternNesting.length();
	if (size > maxVars)
	  maxVars = size;
      }
  }

  /* --- Recursively translate a pattern in a syntax-rule to a Pattern object.
   * @param pattern the the pattern to translate
   * @param literal_identifiers the literals of the syntax-rule
   * @param nesting the depth of ... we are inside
   * @param tr  the current Translator
   * @return the translated Pattern
   */

  public Object apply1 (Object arg)
  {
    if (arg instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) arg;
	Translator tr = (Translator) Compilation.getCurrent();
	ScopeExp save_scope = tr.currentScope();
	tr.setCurrentScope(sf.getScope());
	try
	  {
	    return expand(sf, tr);
	  }
	finally
	  {
	    tr.setCurrentScope(save_scope);
	  }
      }
    else
      return expand(arg, (Translator) Compilation.getCurrent());
  }

  /* DEBUGGING:
  private void printElement (Object el, StringBuffer sb)
  {
    if (el instanceof Object[])
      {
	Object[] arr = (Object[]) el;
	sb.append('{');
	for (int i = 0;  i < arr.length;  i++)
	  {
	    if (i != 0)
	      sb.append(", ");
	    printElement(arr[i], sb);
	  }
	sb.append('}');
      }
    else
      sb.append(el);
  }
  END DEBUGGING */

  public Object expand (Object obj, Translator tr)
  {
    Object[] vars = new Object[maxVars];
    Macro macro = (Macro) tr.getCurrentSyntax();
    /* DEBUGGING:
    System.err.println("match "+macro+" args:"+obj+" maxVars:"+maxVars);
    System.err.flush();
    */
    for (int i = 0;  i < rules.length;  i++)
      {
	SyntaxRule rule = rules[i];
	if (rule==null)
	  return new ErrorExp("error defining "+macro);
	// check that literals have correct binding - FIXME!!
	Pattern pattern = rule.pattern;
	boolean matched = pattern.match (obj, vars, 0);
	if (matched)
	  {
	    if (true)  // DEBUGGING
	      {
		/*
		OutPort err = OutPort.errDefault();
		StringBuffer sb = new StringBuffer();
		sb.append("{Expand "+macro + " rule#" + i
			  +" - matched variables: ");
		for (int j = 0;  j < rule.pattern.varCount;  j++)
		  {
		    if (j > 0)  sb.append("; ");
		    sb.append(j);  sb.append(": ");
		    printElement(vars[j], sb);
		  }
		sb.append('}');
		err.println(sb);
		err.flush();
		*/
	      }

	    if (true)
	      {
		/* DEBUGGING:
		OutPort err = OutPort.errDefault();
		err.print("Expanding ");  err.println(literal_identifiers[0]);
		rule.print_template_program(null, err);
		err.flush();
		*/
	      }
	    Object expansion = rule.execute(vars, tr, TemplateScope.make(tr));

	    if (false) // DEBUGGING:
	      {
		OutPort err = OutPort.errDefault();
		err.print("{Expansion of ");
		err.print(macro);
		err.println(":");
		err.startLogicalBlock("  ", "}", 2);
		kawa.standard.Scheme.writeFormat.writeObject(expansion, err);
		err.endLogicalBlock("}");
		err.println();
		err.flush();
	      }
	    return expansion;
	  }
      }
    /* DEBUGGING:
    System.err.println("no matching syntax-rule for "
		       + literal_identifiers[0]);
    System.err.flush();
    */
    return tr.syntaxError ("no matching syntax-rule for "
			   + literal_identifiers[0]);
  }

  public void print (Consumer out)
  {
    out.write("#<macro ");
    ReportFormat.print(literal_identifiers[0], out);
    out.write('>');
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
