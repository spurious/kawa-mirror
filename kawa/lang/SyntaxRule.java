package kawa.lang;
import gnu.lists.*;
import java.io.*;
import gnu.kawa.lispexpr.LispInterpreter;

public class SyntaxRule implements Externalizable 
{
  Pattern pattern;
  String template_program;

  /** The number of pattern variables in the pattern. */
  int num_variables;

  /** Map variable to ellipsis nesting depth.
   * The nesting depth of the i'th pattern variable
   * is (int) pattern_nesting.charAt(i). */
  // We use a String because it is compact both at runtime and in .class files.
  String pattern_nesting;

  int max_nesting;

  Object[] literal_values;

  public SyntaxRule ()
  {
  }

  public SyntaxRule (Pattern pattern,
		     String pattern_nesting,
		     String template_program,
		     Object[] literal_values,
		     int max_nesting)
  {
    this.pattern = pattern;
    this.pattern_nesting = pattern_nesting;
    this.template_program = template_program;
    this.literal_values = literal_values;
    this.max_nesting = max_nesting;
    this.num_variables = pattern_nesting.length ();
  }

  public SyntaxRule (Pattern pattern, String pattern_nesting,
		     java.util.Vector pattern_names,
		     Object template, java.util.Vector template_identifiers,
		     Translator tr)
  {
    this.pattern = pattern;
    this.pattern_nesting = pattern_nesting;
    this.num_variables = pattern_nesting.length ();
    StringBuffer program = new StringBuffer ();
    java.util.Vector literals_vector = new java.util.Vector ();
    translate_template (template, program, pattern_names,
			0, literals_vector, template_identifiers, 0, tr);
    this.template_program = program.toString ();
    this.literal_values = new Object[literals_vector.size ()];
    literals_vector.copyInto (this.literal_values);
  }

  static final String dots3 = "...";

  // A syntax-rule template is translated into a "template program."
  // The template program is a simple bytecode stored in a string.
  // The encoding is designed so that instructions are normally
  // in the range 1..127, which makes the CONSTANT_Utf8 encoding used
  // in .class files compact.

  // Reversed cons - we evauate the cdr before the car, to reduce stack size.
  final static int RCONS = 1;
  // Make a Pair with a nil cdr.
  final static int LIST1 = 2;
  // Start of a sub-template that is followed by ellipsis.
  final static int START_REPEAT = 3;
  // End of a sub-template that is followed by ellipsis.
  final static int END_REPEAT = 4;
  // Instruction FIRST_VARS+2*i pushes vars[i].
  // This array contains num_variables values of pattern variables,
  // followed by renamed Symbols (matching template_identifiers).
  final static int FIRST_VARS = 5;
  // Instruction FIRST_LITERALS+2*i pushes literal_values[i].
  final static int FIRST_LITERALS = 6;

  /* DEBUGGING
  void print_template_program (java.util.Vector capturedIdentifiers,
			       java.io.PrintWriter ps)
  {
    for (int i = 0;  i < template_program.length (); i++)
      {
	char ch = template_program.charAt (i);
	ps.print ("  " + i + ": " + (int)ch);
	if (ch == RCONS)
	  ps.println (" - RCONS");
	else if (ch == LIST1)
	  ps.println (" - LIST1");
	else if (ch == START_REPEAT)
	  ps.println (" - START_REPEAT");
	else if (ch == END_REPEAT)
	  ps.println (" - END_REPEAT");
	else if ((ch & 1) == (FIRST_LITERALS & 1))
	  {
	    int lit_num = (ch - FIRST_LITERALS) >> 1;
	    ps.print (" - literal[" + lit_num + "]: ");
	    kawa.standard.Scheme.writeFormat.writeObject(literal_values [lit_num], (Consumer) ps);
	    ps.println ();
	  }
	else
	  {
	    int var_num = (ch - FIRST_VARS) >> 1;
	    int ident_num = var_num - num_variables;
	    if (ident_num < 0)
	      ps.println (" - var[" + var_num + "]");
	    else
	      ps.println (" - ident["+capturedIdentifiers.elementAt(ident_num)+"]");
	  }
      }
  }
  */

  /** Recursively translate a syntax-rule template to a template program.
   * @param template the template from the syntax-rule
   * @param template_program (output) the translated template
   * @param pattern_names the names of the pattern variables
   * @param nesting the depth of ... we are inside
   * @param literals_vector (output) the literal data in the template
   * @param quote_nesting if inside a quote: -1; if inside n levels
   *   of quasiquote: n;  otherwise: 0
   * @param tr  the current Translator
   */
  public void translate_template (Object template,
				  StringBuffer template_program,
				  java.util.Vector pattern_names,
				  int nesting,
				  java.util.Vector literals_vector,
				  java.util.Vector template_identifiers,
				  int quote_nesting,
				  Translator tr)
  {
    if (template instanceof Pair)
      {
	Pair pair = (Pair) template;
	if (pair.cdr instanceof Pair)
	  {
	    Pair cdr_pair = (Pair) pair.cdr;
	    if (cdr_pair.car == dots3)
	      {
		translate_template (cdr_pair.cdr, template_program,
				    pattern_names, nesting,
				    literals_vector, template_identifiers,
				    quote_nesting, tr);
		template_program.append ((char) START_REPEAT);
		if (nesting >= max_nesting)
		  max_nesting = nesting + 1;
		translate_template (pair.car, template_program,
				    pattern_names, nesting + 1,
				    literals_vector, template_identifiers,
				    quote_nesting, tr);
		template_program.append ((char) END_REPEAT);
		return;
	      }
	  }
	int code;
	if (pair.cdr == LList.Empty)
	  code = LIST1;
	else
	  {
	    code = RCONS;
	    int cdr_quote_nesting = quote_nesting;
	    if (pair.car == LispInterpreter.quote_sym)
	      cdr_quote_nesting = -1;
	    else if (pair.car == LispInterpreter.quasiquote_sym
		     && cdr_quote_nesting >= 0)
	      cdr_quote_nesting++;
	    else if ((pair.car == LispInterpreter.unquote_sym
		      || pair.car == LispInterpreter.unquotesplicing_sym)
		     && cdr_quote_nesting > 0)
	      cdr_quote_nesting--;
	    translate_template (pair.cdr, template_program,
				pattern_names, nesting,
				literals_vector, template_identifiers,
				cdr_quote_nesting, tr);
	  }
	translate_template (pair.car, template_program,
			    pattern_names, nesting,
			    literals_vector, template_identifiers,
			    quote_nesting, tr);
	template_program.append ((char) code);
	return;
      }
    if (template instanceof String)
      {
	int pattern_var_num = indexOf(pattern_names, template);
	if (pattern_var_num >= 0)
	  {
	    // R4RS requires that the nesting be equal.
	    // We allow an extension here, since it allows potentially-useful
	    // rules like (x (y ...) ...)  => (((x y) ...) ...)
	    if (pattern_nesting.charAt (pattern_var_num) > nesting)
	      tr.syntaxError ("inconsistent ... nesting of " + template);
	    template_program.append ((char) (FIRST_VARS + 2*pattern_var_num));
	    return;
	  }
	else if (quote_nesting == 0
		 && template != LispInterpreter.quote_sym
		 && template != LispInterpreter.quasiquote_sym
		 && template != LispInterpreter.unquote_sym
		 && template != LispInterpreter.unquotesplicing_sym)
	  {
	    int identifier_num = indexOf(template_identifiers, template);
	    if (identifier_num < 0)
	      {
		identifier_num = template_identifiers.size ();
		template_identifiers.addElement (template);
	      }
	    pattern_var_num = identifier_num + num_variables;
	    template_program.append ((char) (FIRST_VARS + 2*pattern_var_num));
	    return;
	  }
	// else treated quoted symbol as literal:
      }
    int literals_index = indexOf(literals_vector,template);
    if (literals_index < 0)
      {
	literals_index = literals_vector.size ();
	literals_vector.addElement (template);
      }
    template_program.append ((char) (FIRST_LITERALS + 2 * literals_index));
  }

  /** Similar to vec.indexOf(elem), but uses == (not equals) to compare. */
  static private int indexOf(java.util.Vector vec, Object elem)
  {
    int len = vec.size();
    for (int i = 0;  i < len;  i++)
      {
	if (vec.elementAt(i) == elem)
	  return i;
      }
    return -1;
  }

  /**
   * @param nesting  number of levels of ... we are nested inside
   * @param indexes element i (where i in [0 .. nesting-1] specifies
   * the interation index for the i'level of nesting
   */
  Object execute_template (int start_pc,
				  Object[] vars,
				  int nesting, int[] indexes,
				  Translator tr, Pair form)
  {
    
    java.util.Stack stack = new java.util.Stack ();
    int template_length = template_program.length();
    for (int i = start_pc;  i < template_length;  i++)
      {
	char ch = template_program.charAt (i);
	/* DEBUGGING:
	System.err.print ("{execute template pc:"+i
			  + " ch:"+(int)ch+" nesting:[");
	for (int level=0;  level < nesting; level++)
	  System.err.print ((level > 0 ? " " : "") + indexes[level]);
	System.err.println("]}");
	*/
	if (ch == RCONS)
	  {
	    Object car = stack.pop ();
	    Object cdr = stack.pop ();
            if (i + 1 == template_length
                && form instanceof PairWithPosition)
              cdr = new PairWithPosition((PairWithPosition) form, car, cdr);
            else
              cdr = new Pair(car, cdr);
	    stack.push(cdr);
	  }
	else if (ch == LIST1)
	  {
	    Object car = stack.pop ();
	    stack.push (new Pair (car, LList.Empty));
	  }
	else if (ch == START_REPEAT)
	  {
	    int count = -1;
	    start_pc = i + 1;
	    int push_count = 0;
	    for (i = start_pc; ; i++)
	      {
		ch = template_program.charAt (i);
		int var_num;
		if (ch == START_REPEAT)
		  push_count++;
		else if (ch == END_REPEAT)
		  {
		    if (push_count == 0)
		      break;
		    push_count--;
		  }
		else if (ch >= FIRST_VARS && (ch & 1) == (FIRST_VARS & 1)
			 && (var_num = (ch - FIRST_VARS) >> 1) < num_variables)
		  {
		    Object var = vars [var_num];
		    int var_nesting = (int) pattern_nesting.charAt (var_num);
		    if (var_nesting > nesting)
		      {
			for (int level = 0;  level < nesting;  level++)
			  var = ((Object[]) var) [indexes[level]];
			Object[] var_array = (Object[]) var;
			if (count == -1)
			  count = var_array.length;
			else if (count != var_array.length)
			  {
			    tr.syntaxError ("inconsistent lengths of repeated variables");
			    count = 0;
			  }
		      }
		  }
	      }
	    if (count < 0)
	      {
		// This check should be done by translate_template.  FIXME!
		tr.syntaxError ("... follows template with no suitably-nested pattern variable");
		count = 0;
	      }
	    Pair last = null;
	    Object following = stack.pop ();
	    Object result = LList.Empty;
	    for (int j = 0;  j < count; j++)
	      {
		indexes[nesting] = j;
		Object element = execute_template (start_pc,
						   vars,
						   nesting + 1, indexes,
						   tr, form);
		Pair pair = new Pair (element, LList.Empty);
		if (last == null)
		  result = pair;
		else
		  last.cdr = pair;
		last = pair;
	      }

	    if (last == null)
	      result = following;
	    else
	      last.cdr = following;
	    stack.push (result);
	  }
	else if (ch == END_REPEAT)
	  break;
	else if ((ch & 1) == (FIRST_LITERALS & 1))
	  stack.push (literal_values [(ch - FIRST_LITERALS) >> 1]);
	else
	  {
	    int var_num = (ch - FIRST_VARS) >> 1;
	    Object var = vars [var_num];
	    if (var_num < pattern_nesting.length ())
	      {
		int var_nesting = (int) pattern_nesting.charAt (var_num);
		for (int level = 0;  level < var_nesting;  level++)
		  var = ((Object[]) var) [indexes[level]];
	      }
	    stack.push (var);
	  }
      }
    return stack.pop ();
  }

  /**
   * @serialData 
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(pattern);
    out.writeObject(pattern_nesting);
    out.writeObject(template_program);
    out.writeObject(literal_values);
    out.writeInt(max_nesting);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    pattern = (Pattern) in.readObject();
    pattern_nesting = (String) in.readObject();
    template_program = (String) in.readObject();
    literal_values = (Object[]) in.readObject();
    max_nesting = in.readInt();
  }
}
