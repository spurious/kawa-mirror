package kawa.lang;
import gnu.expr.*;
import java.util.Vector;

/** Bindings from a <code>syntax-case</code>/<code>syntax-rules</code> pattern. */

public class PatternScope extends LetExp
{
  PatternScope previousSyntax;

  /** Currently visible macro pattern names.
   * For the i'th pattern variable, pattern_names.elementAt(i)
   * is the name of the variable,  */
  public Vector pattern_names;

  /** Nesting of currently visible macro pattern names.
   * For the <code>i</code>'th pattern variable,
   * <code>(int) pattern_nesting.charAt(i)</code> is the nesting (in terms of
   * number of ellipsis that indicate the variable is repeated). */
  public StringBuffer pattern_nesting;

  public Declaration matchArray;

  public PatternScope ()
  {
    super(null);
  }

  public static PatternScope push (Translator tr)
  {
    PatternScope newScope = new PatternScope();
    PatternScope oldScope = tr.patternScope;
    newScope.previousSyntax = oldScope;
    tr.patternScope = newScope;
    if (oldScope == null)
      {
	newScope.pattern_names = new Vector();
	newScope.pattern_nesting = new StringBuffer();
      }
    else
      {
	newScope.pattern_names = (Vector) oldScope.pattern_names.clone();
	newScope.pattern_nesting
	  = new StringBuffer(oldScope.pattern_nesting.toString());
      }
    return newScope;
  }

  public static void pop (Translator tr)
  {
    tr.patternScope = tr.patternScope.previousSyntax;
  }

  /*
  public void compile (Compilation comp, Target target)
  {
    throw new RuntimeException ("internal error - PatternScope.compile called");
  }
  */

}
