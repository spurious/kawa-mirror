package kawa.lang;
import gnu.lists.*;
import java.io.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.*;

/** The translated form of a <code>(syntax <var>template</var>)</code>. */

public class SyntaxTemplate implements Externalizable
{
  /** A <code>syntax</code> or <code>syntax-rules</code> template is
   * translated into a "template program."
   * The template program is a simple bytecode stored in a string.
   * The encoding is designed so that instructions are normally
   * in the range 1..127, which makes the <code>CONSTANT_Utf8</code> encoding
   * used in <code>.class</code> files compact.
   * The folowing <code>BUILD_XXX</code> are the "opcode" of the encoding,
   * stored in the low-order 3 bits of a <code>char</code>.
   */
  String template_program;

  /** Template instructions that don't have an operand value. */
  static final int BUILD_MISC = 0;

  /** Make following operand into a 1-element list. */
  static final int BUILD_LIST1 = (1<<3)+BUILD_MISC;

  static final int BUILD_NIL = (2<<3)+BUILD_MISC;

  /** Wrap following sub-expression in a SyntaxForm. */
  static final int BUILD_SYNTAX = (3<<3)+BUILD_MISC;

  /** Build a vector (an <code>FVector</code>) from following sub-expression.
   * The latter must evaluate to a list. */
  static final int BUILD_VECTOR = (5<<3)+BUILD_MISC;

  /** Instruction to creat a <code>Pair</code> from sub-expressions.
   * Instruction <code>BUILD_CONS+4*delta</code> is followed by a
   * sub-expression for the <code>car</code>
   * (whose length is <code>delta</code> chars),
   * followed by the expression for the <code>cdr</code>. */
  static final int BUILD_CONS = 1;

  /** Instruction BUILD_VAR+8*i pushes vars[i].
   * This array contains the values of pattern variables. */
  final static int BUILD_VAR = 2; // Must be an even number.

  /** Instruction BUILD_VAR_CAR+8*i pushes car(vars[i]).
   * It assumes that vars[i] is actually a pair whose car was the
   * matched pattern variable.  (This is done so we can preserve
   * <code>PairWithPosition</code> source positions). */
  final static int BUILD_VAR_CAR = BUILD_VAR+1;

  /** Instruction BUILD_LITERAL+8*i pushes literal_values[i]. */
  final static int BUILD_LITERAL = 4;

  /** Instruction <code>BUILD_DOTS+8*i</code> repeats a sub-expression.
   * The value <code>i</code> is a variable index of a pattern variable
   * of at least the needed depth.  The result is spliced in. */
  final static int BUILD_DOTS = 5;

  /** Unfinished support for "operand" values that need more tahn 13 bits. */
  final static int BUILD_WIDE = 7;

  /** Map variable to ellipsis nesting depth.
   * The nesting depth of the <code>i</code>'th pattern variable
   * is <code>(int) patternNesting.charAt(i)/2</code>.
   * The low-order bit indicates that if matched value is the <code>car</code>
   * of the value saved in the <code>vars</code> array.
   * (We use a <code>String</code> because it is compact both at runtime
   * and in <code>.class</code> files. */
  String patternNesting;

  int max_nesting;

  Object[] literal_values;

  static final String dots3 = "...";

  /* DEBUGGING:
  void print_template_program (java.util.Vector patternNames,
			       java.io.PrintWriter ps)
  {
    print_template_program(patternNames, ps,
			   0, template_program.length());
    ps.flush();
  }

  void print_template_program (java.util.Vector patternNames,
			       java.io.PrintWriter ps,
			       int start, int limit)
  {
    for (int i = start;  i < limit; i++)
      {
	char ch = template_program.charAt(i);
	ps.print("  " + i + ": " + (int)ch);
	if (ch == BUILD_LIST1)
	  ps.println (" - LIST1");
	else if (ch == BUILD_NIL)
	  ps.println (" - NIL");
	else if (ch == BUILD_SYNTAX)
	  ps.println (" - SYNTAX");
	else if ((ch & 7) == BUILD_DOTS)
	  {
	    int var_num = ch >> 3;
	    ps.print(" - DOTS (var: ");
	    ps.print(var_num);
	    if (patternNames != null
		&& var_num >= 0 && var_num < patternNames.size())
	      {
		ps.print(" = ");
		ps.print(patternNames.elementAt(var_num));
	      }
	    ps.println(')');
	  }
	else if (ch == BUILD_VECTOR)
	  ps.println (" - VECTOR");
	else if ((ch & 7) == BUILD_CONS)
	  ps.println (" - CONS "+(ch >> 3));
	else if ((ch & 7) == BUILD_LITERAL)
	  {
	    int lit_num = ch >> 3;
	    ps.print (" - literal[" + lit_num + "]: ");
	    if (literal_values == null || literal_values.length <= lit_num
		|| lit_num < 0)
	      ps.print("??");
	    else
	      kawa.standard.Scheme.writeFormat.writeObject(literal_values [lit_num], (Consumer) ps);
	    ps.println();
	  }
	else if ((ch & 6) == BUILD_VAR) // Also catches BUILD_VAR_CAR.
	  {
	    int var_num = ch >> 3;
	    ps.print(((ch & 7) == BUILD_VAR ? " - VAR[" : " - VAR_CAR[")
	             + var_num + "]");
	    if (patternNames != null
		&& var_num >= 0 && var_num < patternNames.size())
	      ps.print(": " + patternNames.elementAt(var_num));
	    ps.println();
	  }
	else
	  ps.println (" - ???");	  
      }
  }
  END DEBUGGING */


  protected SyntaxTemplate ()
  {
  }

  public SyntaxTemplate (String patternNesting, String template_program,
			 Object[] literal_values, int max_nesting)
  {
    this.patternNesting = patternNesting;
    this.template_program = template_program;
    this.literal_values = literal_values;
    this.max_nesting = max_nesting;
  }

  public SyntaxTemplate (Object template, SyntaxForm syntax, Translator tr)
  {
    this.patternNesting = tr == null || tr.patternScope == null ? ""
      : tr.patternScope.patternNesting.toString();
    StringBuffer program = new StringBuffer ();
    java.util.Vector literals_vector = new java.util.Vector ();
    /* #ifdef use:java.util.IdentityHashMap */ 
    IdentityHashMap seen = new IdentityHashMap();
    /* #else */
    // Object seen = null;
    /* #endif */
    convert_template(template, syntax,
		     program, 0, literals_vector, seen, false, tr);
    this.template_program = program.toString();
    this.literal_values = new Object[literals_vector.size ()];
    literals_vector.copyInto (this.literal_values);

    /* DEBUGGING:
    OutPort err = OutPort.errDefault();
    err.print("{translated template");
    Macro macro = tr.currentMacroDefinition;
    if (macro != null)
      {
	err.print(" for ");
	err.print(macro);
      }
    if (tr != null && tr.patternScope != null)
      {
        err.println(" - ");
        print_template_program (tr.patternScope.pattern_names, err);
      }
    err.println ('}');
    */
  }

  /** Recursively translate a syntax-rule template to a template program.
   * @param form the template from the syntax-rule
   * @param syntax if non-null, the closest surrounding <code>SyntaxForm</code>
   * @param template_program (output) the translated template
   * @param nesting the depth of ... we are inside
   * @param literals_vector (output) the literal data in the template
   * @param tr  the current Translator
   * @return the index of a pattern variable (in <code>pattern_names</code>)
   *   that is nested at least as much as <code>nesting</code>;
   *   if there is none such, -1 if there is any pattern variable or elipsis;
   *   and -2 if the is no pattern variable or elipsis.
   */
  public int convert_template (Object form,
			       SyntaxForm syntax,
			       StringBuffer template_program,
			       int nesting,
			       java.util.Vector literals_vector,
			       Object seen,
			       boolean isVector,
			       Translator tr)
  {
    while (form instanceof SyntaxForm)
      {
	syntax = (SyntaxForm) form;
	form = syntax.getDatum();
      }
    /* #ifdef use:java.util.IdentityHashMap */ 
    if (form instanceof Pair || form instanceof FVector)
      {
        IdentityHashMap seen_map = (IdentityHashMap) seen;
        if (seen_map.containsKey(form))
          {
            /* FIXME cycles are OK if data are literal. */
            tr.syntaxError("self-referential (cyclic) syntax template");
            return -2;
          }
        seen_map.put(form, form);
      }
    /* #endif */

  check_form:
    if (form instanceof Pair)
      {
	Pair pair = (Pair) form;
	int ret_cdr = -2;;
	int save_pc = template_program.length();
	Object car = pair.getCar();

        // Look for (... ...) and translate that to ...
        if (tr.matches(car, dots3))
          {
            Object cdr = Translator.stripSyntax(pair.getCdr());
            if (cdr instanceof Pair)
              {
                Pair cdr_pair = (Pair) cdr;
                if (cdr_pair.getCar() == dots3 && cdr_pair.getCdr() == LList.Empty)
                  {
                    form = dots3;
                    break check_form;
                  }
              }
          }

	int save_literals = literals_vector.size();
  
	// This may get patched to a BUILD_CONS.
	template_program.append((char) BUILD_LIST1);

	int num_dots3 = 0;
	Object rest = pair.getCdr();
	while (rest instanceof Pair)
	  {
	    Pair p = (Pair) rest;
	    if (! tr.matches(p.getCar(), dots3))
	      break;
	    num_dots3++;
	    rest = p.getCdr();
	    template_program.append((char) BUILD_DOTS); // to be patched.
	  }
	int ret_car = convert_template(car, syntax, template_program,
				       nesting + num_dots3,
				       literals_vector, seen, false, tr);

	if (rest != LList.Empty)
	  {
	    int delta = template_program.length() - save_pc - 1;
	    template_program.setCharAt(save_pc,
				       (char)((delta<<3)+BUILD_CONS));
	    ret_cdr = convert_template (rest, syntax,
					template_program, nesting,
					literals_vector, seen, isVector, tr);
	  }
	if (num_dots3 > 0)
	  {
	    if (ret_car < 0)
	      tr.syntaxError ("... follows template with no suitably-nested pattern variable");
	    for (int i = num_dots3;  --i >= 0; )
	      {
		char op = (char) ((ret_car << 3) + BUILD_DOTS);
		template_program.setCharAt(save_pc+i + 1, op);
		int n = nesting+num_dots3;
		if (n >= max_nesting)
		  max_nesting = n;
	      }
	  }
	if (ret_car >= 0)
	  return ret_car;
	if (ret_cdr >= 0)
	  return ret_cdr;
	if (ret_car == -1 || ret_cdr == -1)
	  return -1;
	if (isVector)
	  return -2;
	// There is no pattern variable in 'form', so treat it as literal.
	// This is optimization to group non-substrituted "chunks"
	// as a single literal and a single SyntaxForm value.
	literals_vector.setSize(save_literals);
	template_program.setLength(save_pc);
      }
    else if (form instanceof FVector)
      {
	template_program.append((char) BUILD_VECTOR);
	return convert_template(LList.makeList((FVector) form), syntax,
				template_program, nesting,
				literals_vector, seen, true, tr);
      }
    else if (form == LList.Empty)
      {
	template_program.append((char) BUILD_NIL);
	return -2;
      }
    else if (form instanceof Symbol
	     && tr != null && tr.patternScope != null)
      {
	int pattern_var_num = indexOf(tr.patternScope.pattern_names, form);
	if (pattern_var_num >= 0)
	  {
	    int var_nesting = patternNesting.charAt(pattern_var_num);
	    int op = (var_nesting & 1) != 0 ? BUILD_VAR_CAR : BUILD_VAR;
	    var_nesting >>= 1;
	    // R4RS requires that the nesting be equal.
	    // We allow an extension here, since it allows potentially-useful
	    // rules like (x (y ...) ...)  => (((x y) ...) ...)
	    if (var_nesting > nesting)
	      tr.syntaxError ("inconsistent ... nesting of " + form);
	    template_program.append((char) (op + 8 * pattern_var_num));
	    return var_nesting == nesting ? pattern_var_num : -1;
	  }
	// else treated quoted symbol as literal:
      }
    int literals_index = indexOf(literals_vector, form);
    if (literals_index < 0)
      {
	literals_index = literals_vector.size ();
	literals_vector.addElement(form);
      }
    if (form instanceof Symbol)
      tr.noteAccess(form, tr.currentScope());
    if (! (form instanceof SyntaxForm) && form != dots3)
      template_program.append((char) (BUILD_SYNTAX));
    template_program.append((char) (BUILD_LITERAL + 8 * literals_index));
    return  form == dots3 ? -1 : -2;
  }

  /** Similar to vec.indexOf(elem), but uses == (not equals) to compare. */
  static int indexOf(java.util.Vector vec, Object elem)
  {
    int len = vec.size();
    for (int i = 0;  i < len;  i++)
      {
	if (vec.elementAt(i) == elem)
	  return i;
      }
    return -1;
  }

  /** The the current repeat count. */
  private int get_count (Object var, int nesting, int[] indexes)
  {
    for (int level = 0;  level < nesting;  level++)
      var = ((Object[]) var) [indexes[level]];
    Object[] var_array = (Object[]) var;
    return var_array.length;
  }

  /** Expand this template
   * The compiler translates <code>(syntax <var>template</var>)</code>
   * to a call to this method.
   */
  public Object execute (Object[] vars, TemplateScope templateScope)
  {
    if (false)  // DEBUGGING
      {
	OutPort err = OutPort.errDefault();
	err.print("{Expand template in ");
	err.print(((Translator) Compilation.getCurrent()).getCurrentSyntax());
        err.print(" tscope: ");
        err.print(templateScope);
        if (false && vars != null)
          {
            err.print(" vars: ");
            for (int i = 0;  i < vars.length;  i++)
              {
                err.println();
                err.print("  " + i +" : ");
                kawa.standard.Scheme.writeFormat.writeObject(vars[i], err);
              }
          }
	err.println('}');
      }

    Object result = execute(0, vars, 0, new int[max_nesting],
                            (Translator) Compilation.getCurrent(),
                            templateScope);

    if (false) // DEBUGGING:
      {
	OutPort err = OutPort.errDefault();
	err.startLogicalBlock("", false, "}");
	err.print("{Expansion of syntax template ");
	err.print(((Translator) Compilation.getCurrent()).getCurrentSyntax());
	err.print(": ");
	err.writeBreakLinear();
	kawa.standard.Scheme.writeFormat.writeObject(result, err);
	err.endLogicalBlock("}");
	err.println();
	err.flush();
      }
    return result;
  }

  public Object execute (Object[] vars, Translator tr,
                         TemplateScope templateScope)
  {
    return execute(0, vars, 0, new int[max_nesting], tr, templateScope);
  }

  Object get_var (int var_num, Object[] vars, int[] indexes)
  {
    Object var = vars [var_num];
    if (var_num < patternNesting.length())
      {
	int var_nesting = (int) patternNesting.charAt(var_num) >> 1;
	for (int level = 0;  level < var_nesting;  level++)
	  var = ((Object[]) var) [indexes[level]];
      }
    return var;    
  }

  /** Similar to execute, but return is wrapped in a list.
   * Normally the result is a single Pair, BUILD_DOTS can return zero
   * or many Pairs. */
  LList executeToList (int pc, Object[] vars, int nesting, int[] indexes,
			Translator tr, TemplateScope templateScope)
  {
    int pc0 = pc;
    int ch = template_program.charAt(pc);
    while ((ch & 7) == BUILD_WIDE)
      ch = ((ch - BUILD_WIDE) << 13) |	template_program.charAt(++pc);
    if ((ch & 7) == BUILD_VAR_CAR)
      {
	Pair p = (Pair) get_var(ch >> 3, vars, indexes);
	return Translator.makePair(p, p.getCar(), LList.Empty);
      }
    else if ((ch & 7) == BUILD_DOTS)
      {
	int var_num = (int) (ch >> 3);
	Object var = vars[var_num];
	int count = get_count(var, nesting, indexes);
	LList result = LList.Empty;
	Pair last = null; // Final Pair of result list, or null.
	pc++;
	for (int j = 0;  j < count; j++)
	  {
	    indexes[nesting] = j;
	    LList list
	      = executeToList(pc, vars, nesting + 1, indexes, tr, templateScope);
	    if (last == null)
	      result = list;
	    else
	      last.setCdrBackdoor(list);
	    // Normally list is a single Pair, but if it is multiple Pairs,
	    // find the last Pair so we can properly splice everything.
	    while (list instanceof Pair)
	      {
		last = (Pair) list;
		list = (LList) last.getCdr();
	      }
	  }
	return result;
      }
    Object v = execute(pc0, vars, nesting, indexes, tr, templateScope);
    return new Pair(v, LList.Empty);
  }

  /**
   * @param nesting  number of levels of ... we are nested inside
   * @param indexes element i (where i in [0 .. nesting-1] specifies
   * the iteration index for the i'level of nesting
   */
  Object execute (int pc, Object[] vars, int nesting, int[] indexes,
		  Translator tr, TemplateScope templateScope)
  {
    int ch = template_program.charAt(pc);
    /* DEBUGGING:
    System.err.print ("{execute template pc:"+pc
		      + " ch:"+(int)ch+" nesting:[");
    for (int level=0;  level < nesting; level++)
      System.err.print ((level > 0 ? " " : "") + indexes[level]);
    System.err.println("]}");
    */
    while ((ch & 7) == BUILD_WIDE)
      ch = ((ch - BUILD_WIDE) << 13) |	template_program.charAt(++pc);
    if (ch == BUILD_LIST1)
      {
	return executeToList(pc+1, vars, nesting, indexes, tr, templateScope);
      }
    else if (ch == BUILD_NIL)
      return LList.Empty;
    else if (ch == BUILD_SYNTAX)
      {
	Object v = execute(pc+1, vars, nesting, indexes, tr, templateScope);
	return v == LList.Empty ? v : SyntaxForms.makeForm(v, templateScope);
      }
    else if ((ch & 7) == BUILD_CONS)
      {
	Pair p = null;
	Object result = null;
	for (;;)
	  {
	    pc++;
	    Object q
	      = executeToList(pc, vars, nesting, indexes, tr, templateScope);
	    if (p == null)
	      result = q;
            else
	      p.setCdrBackdoor(q);
	    while (q instanceof Pair)
	      {
		p = (Pair) q;
		q = p.getCdr();
	      }
	    pc += ch >> 3;
	    ch = template_program.charAt(pc);
	    if ((ch & 7) != BUILD_CONS)
	      break;
	  }
	Object cdr = execute(pc, vars, nesting, indexes, tr, templateScope);
	if (p == null)
	  result = cdr;
        else
	  p.setCdrBackdoor(cdr);
	return result;
      }
    else if (ch == BUILD_VECTOR)
      {
	Object el = execute(pc+1, vars, nesting, indexes, tr, templateScope);
	return new FVector((LList) el);
      }
    else if ((ch & 7) == BUILD_LITERAL)
      {
	int lit_no = ch >> 3;
	/* DEBUGGING:
	System.err.println("-- insert literal#"+lit_no
			   +": "+literal_values[lit_no]);
	*/
	return literal_values[lit_no];
      }
    else if ((ch & 6) == BUILD_VAR) // Also handles BUILD_VAR_CAR.
      {
	Object var = get_var(ch >> 3, vars, indexes);
	if ((ch & 7) == BUILD_VAR_CAR)
	  var = ((Pair) var).getCar();
	return var;
      }
    else
      throw new Error("unknown template code: "+((int) ch)+" at "+pc);
  }

  /**
   * @serialData 
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(patternNesting);
    out.writeObject(template_program);
    out.writeObject(literal_values);
    out.writeInt(max_nesting);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    patternNesting = (String) in.readObject();
    template_program = (String) in.readObject();
    literal_values = (Object[]) in.readObject();
    max_nesting = in.readInt();
  }
}
