package gnu.xquery.testsuite;
import gnu.expr.*;
import kawa.*;
import gnu.mapping.*;
import gnu.xquery.lang.*;
import gnu.text.*;

public class TestMisc
{
  static { XQuery.registerEnvironment(); }
  static Interpreter interp = Interpreter.getInterpreter();
  static Environment env = Environment.getCurrent();

  static int expectedPasses = 0;
  static int unexpectedPasses = 0;
  static int expectedFailures = 0;
  static int unexpectedFailures = 0;
  static boolean verbose = false;
  static String failureExpectedNext = null;

  public static void main(String[] args)
  {
    // gnu.expr.ModuleExp.dumpZipPrefix = "kawa-zip-dump-";
    // gnu.expr.ModuleExp.debugPrintExpr = true;
    // Compilation.debugPrintFinalExpr = true;

    evalTest("3.5+1", "4.5");
    evalTest("3.5+1 ,4*2.5", "4.5 10.0");
    evalTest("3<5", "true");
    evalTest("let $x:=3+4 return $x", "7");
    evalTest("let $x:=3+4 return <a>{$x}</a>", "<a>7</a>");

    evalTest("for $y in (4,5,2+4) return <b>{10+$y}</b>",
	     "<b>14</b><b>15</b><b>16</b>");
    evalTest("for $i in (1 to 10) where ($i mod 2)=1 return 20+$i",
	     "21 23 25 27 29");

    evalTest("(3,4,5)[3]", "5");
    evalTest("1,((2,3)[false()]),5", "1 5");
    evalTest("1,((2 to 4)[true()]),5", "1 2 3 4 5");
    evalTest("(for $y in (5,4) return <b>{10+$y}</b>)[2]", "<b>14</b>");

    evalTest("document(\"tab.xml\")/result",
	     "<result>\n" +
	     "<row>\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\">12</fld2>\n" +
	     "</row>\n" +
	     "<row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>\n" +
	     "<h:row>\n" +
	     "<j:fld1>c1</j:fld1>\n" +
	     "<h:fld2>33</h:fld2>\n" +
	     "<j:fld3>44</j:fld3>\n" +
	     "<k:fld1>c2</k:fld1>\n" +
	     "</h:row>\n" +
	     "</result>");
    evalTest("document(\"tab.xml\")/result/row/fld2",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");
    evalTest("document(\"tab.xml\")/result/row[fld2]",
	     "<row>\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\">12</fld2>\n" +
	     "</row><row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("document(\"tab.xml\")/result/row/*",
	     "<fld1>a1</fld1><fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1><fld2 align=\"right\">22</fld2>");

    evalTest("document(\"tab.xml\")/result/row[2]",
	     "<row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("for $x in document(\"tab.xml\")/result/row[2]/node()" +
	     "  return ('[',$x,']')",
	     "[\n][<fld1 align=\"left\">b1</fld1>][\n" +
	     "][<fld2 align=\"right\">22</fld2>][\n]");
    evalTest("for $x in document(\"tab.xml\")/result/row[2]/text()" +
	     "  return ('[',$x,']')",
	     "[\n][\n][\n]");
    evalTest("for $x in document(\"tab.xml\")/result/row[2]//text()" +
	     "  return ('[',$x,']')",
	     "[\n][b1][\n][22][\n]");
    evalTest("document(\"tab.xml\")/result/row/*[2]",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");

    evalTest("for $x in <T>r1<fld1>a1</fld1><fld3/>r2<fld2>12</fld2></T>" +
	     "  /node()" +
	     "    return ('[',$x,']')",
	     "[r1][<fld1>a1</fld1>][<fld3 />][r2][<fld2>12</fld2>]");

    evalTest("(document(\"tab.xml\")/result/row/*)[2]",
	     "<fld2 align=\"right\">12</fld2>");
    evalTest("(document(\"tab.xml\")/result/row/*)[2 to 3]",
	     "<fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1>");
    evalTest("(document(\"tab.xml\")/result/row/*)[position()>1]",
	     "<fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1><fld2 align=\"right\">22</fld2>");
    evalTest("(document(\"tab.xml\")/result/row/*)[position()>1][2]",
	     "<fld1 align=\"left\">b1</fld1>");

    evalTest("document(\"tab.xml\")/result/row/(fld2,fld1)",
	     "<fld2 align=\"right\">12</fld2><fld1>a1</fld1><fld2 align=\"right\">22</fld2><fld1 align=\"left\">b1</fld1>");

    evalTest("string-value(document('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\nc1\n33\n44\nc2\n\n\n");
    evalTest("string(document('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\nc1\n33\n44\nc2\n\n\n");
    evalTest("string(document('tab.xml')/result/row/fld1/@align)", "left");
    evalTest("string(document('tab.xml')/result/row/fld2/@align)",
	     "rightright");
 
    evalTest("for $x in children(<a>xy{3+4}kl<c>def</c>{9}{11}</a>)" +
	     "  return ('[',$x,']')",
	     "[xy 7 kl][<c>def</c>][9 11]");
    evalTest("children(<a>xy{3+4}kl<c>def</c>{9}{11}</a>)",
	     "xy 7 kl<c>def</c>9 11");

    evalTest("<a>aab</a> ='aab'", "true");
    evalTest("<a>abc</a>='abb'", "false");

    evalTest("document(\"tab.xml\")/result/row[fld1]",
	     "<row>\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\">12</fld2>\n" +
	     "</row><row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("document(\"tab.xml\")/result/row[fld3]", "");
    evalTest("document(\"tab.xml\")/result/row/fld1[@align]",
	     "<fld1 align=\"left\">b1</fld1>");
    evalTest("document(\"tab.xml\")/result/row/fld2[@align]",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");
    evalTest("'a',document(\"tab.xml\")/result/row/fld1[@align='left']",
	     "a<fld1 align=\"left\">b1</fld1>");
    evalTest("'a',document(\"tab.xml\")/result/row/fld1[@align='right']", "a");

    evalTest("let $x:=12,\n" +
	     "    $y:=<a>{$x+$x}</a>\n" +
	     "  return <b atr1='11' atr2=\"{$x}\">{($y,99,$y)}</b>",
	     "<b atr1=\"11\" atr2=\"12\"><a>24</a>99<a>24</a></b>");

    // Simple namespace tests.
    evalTest("namespace xx='XXX'\n <xx:a>XX</xx:a>", "<xx:a>XX</xx:a>");
    evalTest("namespace x1='XXX'\n namespace x2='XXX'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x2:ab)",
	     "X1X2");
    evalTest("namespace x1='XXX'\n namespace x2='YYY'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x2:ab)",
	     "X2");
    evalTest("namespace x1='XXX'\n namespace x2='YYY'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*)",
	     "X1X2");
    evalTest("namespace x1='XXX'\n namespace x2='YYY'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*:*)",
	     "X1X2");
    evalTest("namespace x1='XXX'\n namespace x2='YYY'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x1:*)",
	     "X1");
    evalTest("namespace x1='XXX'\n namespace x2='YYY'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*:ab)",
	     "X1X2");
    evalTest("namespace x1='XXX'\n namespace x2='YYY'\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:cd>X2</x2:cd></top>)/*:cd)",
	     "X2");
    evalTest("namespace h='H'\n"
	     + "string(document(\"tab.xml\")/result/h:row)",
	     "\nc1\n33\n44\nc2\n");
    evalTest("namespace xx='H'\n"
	     + "string(document(\"tab.xml\")/result/xx:row)",
	     "\nc1\n33\n44\nc2\n");
    evalTest("string(document(\"tab.xml\")/result/*:row)",
	     "\na1\n12\n\nb1\n22\n\nc1\n33\n44\nc2\n");
    evalTest("string(document(\"tab.xml\")/result/*:row/*:fld1)",
	     "a1b1c1c2");
    evalTest("namespace k='J'\n"
	     + "string(document(\"tab.xml\")/result/*:row/k:fld1)",
	     "c1c2");
    evalTest("namespace k='J'\n"
	     + "string(document(\"tab.xml\")/result/*:row[k:fld1])",
	     "\nc1\n33\n44\nc2\n");

    evalTest("document(\"tab.xml\")/result/row[1]/descendant::*",
	     "<fld1>a1</fld1><fld2 align=\"right\">12</fld2>");
    evalTest("document(\"tab.xml\")/result/row[1]/descendant::node()",
	     "<fld1>a1</fld1>a1<fld2 align=\"right\">12</fld2>"
	     + " align=\"right\"12");
    evalTest("document(\"tab.xml\")/result/row[1]/descendant::text()",
	     "a112");
    evalTest("document(\"tab.xml\")/result/row[1]/descendant-or-self::*",
	     "<row><fld1>a1</fld1>"
	     + "<fld2 align=\"right\">12</fld2></row>"
	     + "<fld1>a1</fld1><fld2 align=\"right\">12</fld2>");

    // Check for catching errors:
    evalTest("+ +", "*** syntax error - <string>:1:4: missing PrimaryExpr");

    evalTest("namespace x1='XXX",
	     "*** caught SyntaxException - <string>:1:18: "
	     + "unexpected end-of-file in string");

    evalTest("unescaped-data('<?--->'),let $x:=unescaped-data('an &oslash;') return <b>{unescaped-data('<![CDATA[saw]]>')} {$x}</b>",
	     "<?---><b><![CDATA[saw]]> an &oslash;</b>");
    printSummary();
  }

  public static boolean printSummary ()
  {
    System.out.println("# of expected passes      " + expectedPasses);
    if (expectedFailures > 0)
      System.out.println("# of expected failures    " + expectedFailures);
    if (unexpectedPasses > 0)
      System.out.println("# of unexpected passes    " + unexpectedPasses);
    if (unexpectedFailures > 0)
      System.out.println("# of unexpected failures  " + unexpectedFailures);
    return unexpectedFailures != 0;
  }

  /** True if the two string match, ignoring unquoted white-space. */
  public static boolean matches(String str1, String str2)
  {
    int i = 0;
    int j = 0;
    char quote = 0;
    for (;;)
      {
	char x, y;
	for (;;)
	  {
	    if (i >= str1.length())
	      {
		x = 0;
		break;
	      }
	    x = str1.charAt(i++);
	    if (quote != 0 || ! Character.isWhitespace(x))
	      break;
	  }
	for (;;)
	  {
	    if (j >= str2.length())
	      {
		y = 0;
		break;
	      }
	    y = str2.charAt(j++);
	    if (quote != 0 || ! Character.isWhitespace(y))
	      break;
	  }
	if (x != y)
	  return false;
	if (x == 0)
	  return true;
	if (x == '\'' || x == '\"')
	  {
	    if (quote == 0)
	      quote = x;
	    else if (x == quote)
	      quote = 0;
	  }
      }
  }

  public static void evalTest(String expr, String expected)
  {
    String result;
    Throwable throwable = null;
    try
      {
	result = eval(expr);
      }
    catch (Throwable ex)
      {
	if (ex instanceof WrappedException)
	  ex = ((WrappedException) ex).getException();
	throwable = ex;
	if (ex instanceof SyntaxException)
	  result = "*** caught SyntaxException - "
	    + ((SyntaxException) ex).getMessages().getErrors();
	else
	  result = "*** caught " + ex.getClass().getName() + " ***";
      }
    boolean failureExpected = failureExpectedNext != null;
    if (matches(expected, result))
      {
	if (failureExpected)
	  unexpectedPasses++;
	else
	  expectedPasses++;
	if (verbose || failureExpected)
	  System.out.println((failureExpected ? "XPASS: \"" : "PASS: \"")
			     + expr + "\" evaluated to \"" + result + "\"");
      }
    else
      {
	if (failureExpectedNext != null)
	  expectedFailures++;
	else
	  unexpectedFailures++;
	if (verbose || ! failureExpected)
	  System.out.println((failureExpected ? "XFAIL: \"" : "FAIL: \"")
			     + expr + "\" evaluated to \"" + result
			     + "\" but expected \"" + expected + "\"");
	if (verbose && ! failureExpected && throwable != null)
	  throwable.printStackTrace(System.out);
      }
    failureExpectedNext = null;
  }

  public static String eval(String expr)
    throws Throwable
  {
    CharArrayOutPort out = new CharArrayOutPort();
    InPort in = new CharArrayInPort(expr);
    OutPort err = OutPort.errDefault();
    SourceMessages messages = new SourceMessages();
    Lexer lexer = interp.getLexer(in, messages);
    CallContext ctx = CallContext.getInstance();
    ctx.consumer = interp.getOutputConsumer(out);

    for (;;)
      {
	ModuleExp mod = interp.parse(env, lexer);
	if (mod == null)
	  break; // return "*** end-of-file ***";
	mod.setName("atInteractiveLevel");  // FIXME
	SourceError firstError = messages.getErrors();
	if (firstError != null)
	  return "*** syntax error - " + firstError;

	/*
	int ch;
	for (;;)
	  {
	    ch = in.read();
	    if (ch < 0 || ch == '\r' || ch == '\n')
	      break;
	    if (ch != ' ' && ch != '\t')
	      {
		in.unread();
		break;
	      }
	  }
	*/

	mod.evalModule(env, ctx);
	ctx.runUntilDone();
      }
    /*
    if (ch >= 0)
      return "*** junk at end of input ***";
    */

    return new String(out.toCharArray());
  }
}
