package kawa.lang;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.lists.*;
import java.io.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.*;

/** The transated form of a <code>(syntax <var>template</var>)</code>. */

public class SyntaxTemplate implements Externalizable
{
  /** A <code>syntax</code> or <code>syntax-rules</code> template is
   * translated into a "template program."
   * The template program is a simple bytecode stored in a string.
   * The encoding is designed so that instructions are normally
   * in the range 1..127, which makes the <code>CONSTANT_Utf8</code> encoding
   * used in <code>.class</code> files compact.
   * The folowing <code>BUILD_XXX</code> are the "opcode" of the encoding,
   * stored in the low-order 2 bits of a <code>char</code>.
   */
  String template_program;

  /** Make following operand into a 1-element list. */
  static final int BUILD_LIST1 = (1<<2)+3;

  /** Wrap following sub-expression in a SyntaxForm. */
  static final int BUILD_SYNTAX = (2<<2)+3;

  /** A sub-template that gets repeated depending on a matched pattern.
   * Followed by a variable index of a pattern variable of at least
   * the needed depth, followed by a length, followed by the sub-template
   * to be repeated.  If the length is 0, the result is a list with one
   * pair for each match of the pattern variable.  If the length is > 0,
   * it is the length of the repeated sub-template, which is followed by
   * another subtemplate to be spliced (appended) to the repetition. */
  static final int BUILD_REPEAT = (3<<2)+3;

  /** Build a vector (an <code>FVector</code>) from following sub-expressions.
   * Followed by a length N, then 1+VAR_NO, then for each I, where 0<=I<N:
   * a length of the sub-expression for element I, followed by
   * the expression for element I itself.  If the pattern has elipsis,
   * then VAR_NO is a variable from which to get the repeat count; otherwise
   * VAR_NO is -1. */
  static final int BUILD_VECTOR = (5<<2)+3;

  /* A sub-expression to be repeated a pattern-determined number of times.
   * Each value is inserted into the curent vector.
   * Corresponds to '...' (except BUILD_VEC_REPEAT is a prefix operator,
   * not suffix) in a vector template. */
  static final int BUILD_VEC_REPEAT = (6<<2)+3;

  /** Instruction to creat a <code>Pair</code> from sub-expressions.
   * Instruction <code>BUILD_CONS+4*delta</code> is followed by a
   * sub-expression for the <code>car</code>
   * (whose length is <code<delta</code> chars),
   * followed by the expression for the <code>cdr</code>. */
  static final int BUILD_CONS = 0;

  /** Instruction BUILD_VARS+8*i pushes vars[i].
   * This array contains the values of pattern variables. */
  final static int BUILD_VAR = 1;

  /** Instruction BUILD_LITERAL+8*i pushes literal_values[i]. */
  final static int BUILD_LITERAL = 2;

  /** Map variable to ellipsis nesting depth.
   * The nesting depth of the <code>i</code>'th pattern variable
   * is <code>(int) pattern_nesting.charAt(i)</code>.
   * (We use a <code>String</code> because it is compact both at runtime
   * and in <code>.class</code> files. */
  String pattern_nesting;

  int max_nesting;

  Object[] literal_values;

  static final String dots3 = "...";

  /** The lexical context of template definition. */
  private ScopeExp currentScope;

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
	else if (ch == BUILD_SYNTAX)
	  ps.println (" - SYNTAX");
	else if (ch == BUILD_VEC_REPEAT)
	  ps.println (" - VEC_REPEAT");
	else if (ch == BUILD_REPEAT)
	  {
	    int var_num = template_program.charAt(++i);
	    int width = template_program.charAt(++i);
	    ps.println (" - BUILD_REPEAT var:"+var_num+" width:"+width);
	  }
	else if (ch == BUILD_VECTOR)
	  {
	    int count = template_program.charAt(++i);
	    int var_num = (short) template_program.charAt(++i);
	    ps.println (" - BUILD_VECTOR count:"+count+" var:"+(var_num-1));
	    int j = 0;
	    i++;
	    while (j < count)
	      {
 		int width = template_program.charAt(i);
		ps.println("  " + i + ": width of element "+j+": "+width);
		print_template_program(patternNames, ps,
				       ++i, i += width);
		j++;
	      }
	  }
	else if ((ch & 3) == BUILD_CONS)
	  ps.println (" - CONS "+(ch >> 3));
	else if ((ch & 3) == BUILD_LITERAL)
	  {
	    int lit_num = ch >> 2;
	    ps.print (" - literal[" + lit_num + "]: ");
	    if (literal_values == null || literal_values.length <= lit_num
		|| lit_num < 0)
	      ps.print("??");
	    else
	      kawa.standard.Scheme.writeFormat.writeObject(literal_values [lit_num], (Consumer) ps);
	    ps.println();
	  }
	else if ((ch & 3) == BUILD_VAR)
	  {
	    int var_num = ch >> 2;
	    ps.print(" - VAR[" + var_num + "]");
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

  public SyntaxTemplate (String pattern_nesting, String template_program,
			 Object[] literal_values, int max_nesting)
  {
    this.pattern_nesting = pattern_nesting;
    this.template_program = template_program;
    this.literal_values = literal_values;
    this.max_nesting = max_nesting;
  }

  public SyntaxTemplate (Object template, Translator tr)
  {
    this.currentScope = tr.currentScope();
    this.pattern_nesting = tr == null || tr.patternScope == null ? ""
      : tr.patternScope.pattern_nesting.toString();
    StringBuffer program = new StringBuffer ();
    java.util.Vector literals_vector = new java.util.Vector ();
    /* BEGIN JAVA1 */
    // Object seen = null;
    /* END JAVA1 */
    /* BEGIN JAVA2 */
    IdentityHashMap seen = new IdentityHashMap();
    /* END JAVA2 */
    convert_template(template, program, 0, literals_vector, seen, tr);
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
    err.println(':');
    print_template_program (tr.patternScope.pattern_names, err);
    err.println ('}');
    */
  }

  /** Recursively translate a syntax-rule template to a template program.
   * @param template the template from the syntax-rule
   * @param template_program (output) the translated template
   * @param nesting the depth of ... we are inside
   * @param literals_vector (output) the literal data in the template
   * @param quote_nesting if inside a quote: -1; if inside n levels
   *   of quasiquote: n;  otherwise: 0
   * @param tr  the current Translator
   * @return the index of a pattern variable (in <code>pattern_names</code>)
   *   that is nested at least as much as <code>nesting</code>;
   *   if there is none such, -1 if there is any pattern variable or elipsis;
   *   and -2 if the is no pattern variable or elipsis.
   */
  public int convert_template (Object form,
			   StringBuffer template_program,
			   int nesting,
			   java.util.Vector literals_vector,
			   Object seen,
			   Translator tr)
  {
    if (form instanceof Pair)
      {
	Pair pair = (Pair) form;
	int ret;
	int save_pc = template_program.length();
	if (pair.cdr instanceof Pair)
	  {
	    Pair cdr_pair = (Pair) pair.cdr;
	    if (cdr_pair.car == dots3)
	      {
		template_program.append((char) BUILD_REPEAT);
		template_program.append('\0'); /* var_num, to be patched */
		template_program.append('\0'); /* width, to be patched */
		ret = convert_template(pair.car, template_program, nesting + 1,
				       literals_vector, seen, tr);
		if (ret <= -1)
		  tr.syntaxError ("... follows template with no suitably-nested pattern variable");
		else
		  template_program.setCharAt(save_pc+1, (char) ret);
		if (cdr_pair.cdr != LList.Empty)
		  {
		    int width = template_program.length() - save_pc - 3;
		    template_program.setCharAt(save_pc+2, (char) width);
		    convert_template(cdr_pair.cdr, template_program, nesting,
				     literals_vector, seen, tr);
		  }
		if (nesting >= max_nesting)
		  max_nesting = nesting + 1;
		return ret;
	      }
	  }
	int save_literals = literals_vector.size();
	if (pair.cdr == LList.Empty)
	  {
	    template_program.append((char) BUILD_LIST1);
	    ret = convert_template(pair.car, template_program, nesting,
				   literals_vector, seen, tr);
	  }
	else
	  {
	    template_program.append((char) BUILD_CONS);
	    ret = convert_template (pair.car, template_program, nesting,
				    literals_vector, seen, tr);
	    int delta = template_program.length() - save_pc - 1;
	    template_program.setCharAt(save_pc, (char)((delta<<3)+BUILD_CONS));
	    int ret2 = convert_template (pair.cdr, template_program, nesting,
					 literals_vector, seen, tr);
	    if (ret < 0)
	      ret = ret2;
	  }
	if (ret >= -1)
	  return ret;
	literals_vector.setSize(save_literals);
	template_program.setLength(save_pc);
      }
    else if (form instanceof FVector)
      {
	int save_pc = template_program.length();
	int save_literals = literals_vector.size();
	int ret = -2;
	FVector vec = (FVector) form;
	int len = vec.size();
	int var_num = -1;
	template_program.append((char) BUILD_VECTOR);
	template_program.append((char) len);
	template_program.append('\0'); /* var_num if ellipsis, to be patched */
	int element_start = save_pc + 3;
	for (int i = 0;  i < len;  i++)
	  {
	    template_program.append((char) 0);
	    boolean repeat = i + 1 < len && vec.get(i+1) == dots3;
	    if (repeat)
	      template_program.append((char) BUILD_VEC_REPEAT);
	    int ret2 = convert_template (vec.get(i), template_program,
					 repeat ? (nesting + 1) : nesting,
					 literals_vector, seen, tr);
	    if (repeat)
	      {
		if (nesting >= max_nesting)
		  max_nesting = nesting + 1;
		if (var_num >= 0)
		  tr.syntaxError("more than one '...' in vector template");
		if (ret2 <= -1)	
		  tr.syntaxError ("... follows template with no suitably-nested pattern variable");
		else
		  var_num = ret2;
		template_program.setCharAt(save_pc + 1, (char) (len - 2));
		template_program.setCharAt(save_pc + 2, (char) (ret2 + 1));
		i++;
	      }
	    if (ret2 > ret)
	      ret = ret2;
	    int element_end = template_program.length();
	    template_program.setCharAt(element_start,
				       (char) (element_end - element_start - 1));
	    element_start = element_end;
	  }
	if (ret >= -1)
	  return ret;
	literals_vector.setSize(save_literals);
	template_program.setLength(save_pc);
      }
    else if (form instanceof String
	     && tr != null && tr.patternScope != null)
      {
	int pattern_var_num = indexOf(tr.patternScope.pattern_names, form);
	if (pattern_var_num >= 0)
	  {
	    // R4RS requires that the nesting be equal.
	    // We allow an extension here, since it allows potentially-useful
	    // rules like (x (y ...) ...)  => (((x y) ...) ...)
	    int var_nesting = pattern_nesting.charAt(pattern_var_num);
	    if (var_nesting > nesting)
	      tr.syntaxError ("inconsistent ... nesting of " + form);
	    template_program.append((char) (BUILD_VAR + 4 * pattern_var_num));
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
    if (form instanceof String || form instanceof Symbol)
      tr.noteAccess(form, tr.currentScope());
    if (true) // FIXME
      template_program.append((char) (BUILD_SYNTAX));
    template_program.append((char) (BUILD_LITERAL + 4 * literals_index));
    return -2;
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
  public Object execute (Object[] vars)
  {
    Object result = execute(vars, (Translator) Compilation.getCurrent());
    /* DEBUGGING:
    OutPort err = OutPort.errDefault();
    err.print("{Expansion of syntax template: ");
    err.print(result);
    err.println('}');
    err.flush();
    */
    return result;
  }

  public Object execute (Object[] vars, Translator tr)
  {
    ScopeExp templateScope = new LetExp(null);
    if (currentScope != null)
      templateScope.outer = currentScope;
    else
      {
	Syntax curSyntax = tr.getCurrentSyntax();
	if (curSyntax instanceof Macro)
	  templateScope.outer = ((Macro) curSyntax).capturedScope;
      }
    return execute(0, vars, 0, new int[max_nesting], tr, templateScope);
  }

  /**
   * @param nesting  number of levels of ... we are nested inside
   * @param indexes element i (where i in [0 .. nesting-1] specifies
   * the iteration index for the i'level of nesting
   */
  Object execute (int pc, Object[] vars, int nesting, int[] indexes,
		  Translator tr, ScopeExp templateScope)
  {
    char ch = template_program.charAt(pc);
    /* DEBUGGING:
    System.err.print ("{execute template pc:"+pc
		      + " ch:"+(int)ch+" nesting:[");
    for (int level=0;  level < nesting; level++)
      System.err.print ((level > 0 ? " " : "") + indexes[level]);
    System.err.println("]}");
    */
    if (ch == BUILD_LIST1)
      {
	Object v = execute(pc+1, vars, nesting, indexes, tr, templateScope);
	return new Pair(v, LList.Empty);
      }
    else if (ch == BUILD_SYNTAX)
      {
	Object v = execute(pc+1, vars, nesting, indexes, tr, templateScope);
	return v == LList.Empty ? v : SyntaxForm.make(v, templateScope);
      }
    else if ((ch & 3) == BUILD_CONS)
      {
	Pair p = null;
	Object result = null;
	for (;;)
	  {
	    Pair q = new Pair();
	    if (p == null)
	      result = q;
	    else
	      p.cdr = q;
	    pc++;
	    q.car = execute(pc, vars, nesting, indexes, tr, templateScope);
	    p = q;
	    pc += ch >> 3;
	    ch = template_program.charAt(pc);
	    if ((ch & 3) != BUILD_CONS)
	      break;
	  }
	p.cdr = execute(pc, vars, nesting, indexes, tr, templateScope);
	return result;
      }
    else if (ch == BUILD_REPEAT)
      {
	int var_num = (int) template_program.charAt(++pc);
	int width = (int) template_program.charAt(++pc);
	Object var = vars [var_num];
	int count = get_count(var, nesting, indexes);
	Pair last = null;
	Object result = LList.Empty;
	pc++;
	for (int j = 0;  j < count; j++)
	  {
	    indexes[nesting] = j;
	    Object element = execute(pc, vars, nesting + 1, indexes, tr, templateScope);
	    Pair pair = new Pair (element, LList.Empty);
	    if (last == null)
	      result = pair;
	    else
	      last.cdr = pair;
	    last = pair;
	  }
	Object cdr;
	if (width == 0)
	  cdr = LList.Empty;
	else
	  cdr = execute(pc+width, vars, nesting, indexes, tr, templateScope);
	if (last == null)
	  result = cdr;
	else
	  last.cdr = cdr;
	return result;
      }
    else if (ch == BUILD_VECTOR)
      {
	int count = (int) template_program.charAt(++pc);
	int var_num = (short) template_program.charAt(++pc) - 1;
	int var_count;
	int size;
	if (var_num < 0)
	  {
	    size = count;
	    var_count = 0;
	  }
	else
	  {
	    var_count = get_count(vars[var_num], nesting, indexes);
	  }
	size = count + var_count;
	FVector vec = new FVector(size);
	int i = 0;
	pc++;
	while (i < size)
	  {
	    int width = (int) template_program.charAt(pc++);
	    if (template_program.charAt(pc) == BUILD_VEC_REPEAT)
	      {
		for (int j = 0;  j < var_count;  j++)
		  {
		    indexes[nesting] = j;
		    Object el = execute (pc + 1, vars, nesting + 1,
					 indexes, tr, templateScope);
		    if (el instanceof SyntaxForm)
		      el = ((SyntaxForm) el).form;
		    vec.set(i++, el);
		  }
	      }
	    else
	      {
		Object el = execute(pc, vars, nesting, indexes, tr, templateScope);
		if (el instanceof SyntaxForm)
		  el = ((SyntaxForm) el).form;
		vec.set(i++, el);
	      }
	    pc += width;
	  }
	return vec;
      }
    else if ((ch & 3) == BUILD_LITERAL)
      {
	int lit_no = ch >> 2;
	/* DEBUGGING:
	System.err.println("-- insert literal#"+lit_no
			   +": "+literal_values[lit_no]);
	*/
	return literal_values[lit_no];
      }
    else if ((ch & 3) == BUILD_VAR)
      {
	int var_num = ch >> 2;
	Object var = vars [var_num];
	if (var_num < pattern_nesting.length ())
	  {
	    int var_nesting = (int) pattern_nesting.charAt (var_num);
	    for (int level = 0;  level < var_nesting;  level++)
	      var = ((Object[]) var) [indexes[level]];
	  }
	/* DEBUGGING:
	System.err.println("-- insert pattern var: "+var);
	*/
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
    out.writeObject(pattern_nesting);
    out.writeObject(template_program);
    out.writeObject(literal_values);
    out.writeInt(max_nesting);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    pattern_nesting = (String) in.readObject();
    template_program = (String) in.readObject();
    literal_values = (Object[]) in.readObject();
    max_nesting = in.readInt();
  }
}
