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
    int rules_count = LList.listLength(srules, false);
    if (rules_count <= 0)
      {
	rules_count = 0;
	tr.syntaxError ("missing or invalid syntax-rules");
      }
    this.rules = new SyntaxRule [rules_count];
    Pair rules_pair;
    Macro macro = tr.currentMacroDefinition;
    for (int i = 0;  i < rules_count;  i++, srules = rules_pair.cdr)
      {
	rules_pair = (Pair) srules;

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

	    PatternScope patternScope = PatternScope.push(tr);
	    tr.push(patternScope);

	    if (! (pattern instanceof Pair)
		|| ! (((Pair)pattern).car instanceof String))
	      {
		tr.syntaxError ("pattern does not start with name");
		return;
	      }
	    // ?? FIXME
            literal_identifiers[0] = ((Pair)pattern).car;

	    StringBuffer programbuf = new StringBuffer();
	    SyntaxForm syntax = null;
	    while (pattern instanceof SyntaxForm)
	      {
		syntax = (SyntaxForm) pattern;
		pattern = syntax.form;
	      }

	    // In R5RS syntax-rules, the initial name is neither a
	    // pattern variable or a literal identifier, so ingore it.
	    if (pattern instanceof Pair)
	      {
		Pair p = (Pair) pattern;
		programbuf.append((char) ((1 << 3) | SyntaxPattern.MATCH_PAIR));
		programbuf.append((char) SyntaxPattern.MATCH_IGNORE);
		pattern = p.cdr;
	      }
	    SyntaxPattern spattern = new SyntaxPattern(programbuf, pattern,
					     syntax, literal_identifiers, tr);

	    this.rules[i] = new SyntaxRule(spattern, template, tr);

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
	tr.setCurrentScope(sf.scope);
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
	// check that literals have correct binding - FIXME!!
	Pattern pattern = rule.pattern;
	boolean matched = pattern.match (obj, vars, 0);
	if (matched)
	  {
	    /* DEBUGGING:
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
	    END DEBUGGING */

	    /* DEBUGGING:
	    err.print("Expanding ");  err.println(literal_identifiers[0]);
	    rule.print_template_program(null, err);
	    err.flush();
	    */
	    Object expansion = rule.execute(vars, tr);

	    /* DEBUGGING:
	    err.print("{Expansion of ");
	    err.print(macro);
	    err.print(": ");
	    err.print(expansion);
	    err.println('}');
	    err.flush();
	    */
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
