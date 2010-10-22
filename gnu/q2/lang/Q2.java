package gnu.q2.lang;
import gnu.mapping.*;
import kawa.standard.Scheme;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.ReadTable;

/** Support for the experimental Q2 language.
 * See the <a href="http://www.gnu.org/software/kawa/q2/">web site</a>
 * for information.
 */

public class Q2 extends Scheme
{
  static Q2 instance;
  static final Object emptyForm = new FString();

  public Q2 ()
  {
    instance = this;
    ModuleBody.setMainPrintValues(true);
  }

  public static Q2 getQ2Instance()
  {
    if (instance == null)
      new Q2 ();
    return instance;    
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    Q2Read lexer = new Q2Read(inp, messages);
    return lexer;
  }

  public Consumer getOutputConsumer(java.io.Writer out)
  {
    return new XMLPrinter(out, false);
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(new Q2());
  }

  public Expression makeBody(Expression[] exps)
  {
    return new ApplyExp(gnu.kawa.functions.AppendValues.appendValues, exps);
  }

  public Expression makeApply (Expression func, Expression[] args)
  {
    /*
    if (func instanceof QuoteExp
	&& ((QuoteExp) func).getValue() instanceof Procedure)
      return super.makeApply(func, args);
    */
    Expression[] exps = new Expression[args.length+1];
    exps[0] = func;
    System.arraycopy(args, 0, exps, 1, args.length);
    return new ApplyExp(Q2Apply.q2Apply, exps);
  }

  public Procedure getPrompter()
  {
    return new Prompter();
  }

  public ReadTable createReadTable ()
  {
    ReadTable rt = ReadTable.createInitial();
    rt.set('(', new Q2ReaderParens());
    rt.setFinalColonIsKeyword(true);
    return rt;
  }

  /** Compare two indentation amounts.
   * An indentation is @{code (numberOfTabs<<16)+numberOfSpaces}.
   * A comparison is indeterminate if it depends on tab-width - e.g.
   * the number of tabs in indentation1 is less than indentation2,
   * but it's the reverse when it comes to spaces.
   * @return Integer.MIN_VALUE if the comparison is indeterminate;
   *   otherwise returns the "difference" between the two (where
   *   tabs count as 8 spaces).
   */
  public static int compareIndentation (int indentation1, int indentation2)
  {
    int numTabs1 = indentation1 >>> 16;
    int numTabs2 = indentation1 >>> 16;
    int numSpaces1 = indentation1 & 0xFF;
    int numSpaces2 = indentation2 & 0xFF;
    if (numTabs1 == numTabs2)
      return numSpaces1 - numSpaces2;
    else if ((numTabs1 < numTabs2 && numSpaces1 <= numSpaces2)
             || (numTabs1 > numTabs2 && numSpaces1 >= numSpaces2))
      return 8 * (numTabs1 - numTabs2);
    else
      return Integer.MIN_VALUE;
  }
}

class Prompter extends Procedure1
{
  public Object apply1 (Object arg)
  {
    InPort port = (InPort) arg;
    int line = port.getLineNumber() + 1;
    char state = port.readState;
    if (state == ']')
      return "<!--Q2:"+line+"-->";
    else
      {
	if (state == '\n')
	  state = '-';
	return "#|--Q2:"+line+"|#"+state;
      }
  }
}
