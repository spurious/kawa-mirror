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
    evalTest("3.4+1", "4.4");
    evalTest("3.4+1 ,4*2.5", "4.4 10.0");
    evalTest("3<5", "true");
    evalTest("let $x:=3+4 return $x", "7");
    evalTest("let $x:=3+4 return <a>{$x}</a>", "<a>7</a>");

    evalTest("for $y in (4,5,2+4) return <b>{10+$y}</b>",
	     "<b>14</b><b>15</b><b>16</b>");

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
    evalTest("document(\"tab.xml\")/result/row/*[2]",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");

    evalTest("(document(\"tab.xml\")/result/row/*)[2]",
	     "<fld2 align=\"right\">12</fld2>");
    evalTest("(document(\"tab.xml\")/result/row/*)[2 to 3]",
	     "<fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1>");

    evalTest("document(\"tab.xml\")/result/row/(fld2,fld1)",
	     "<fld2 align=\"right\">12</fld2><fld1>a1</fld1><fld2 align=\"right\">22</fld2><fld1 align=\"left\">b1</fld1>");

    evalTest("string-value(document('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\n");
    evalTest("string(document('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\n");
    evalTest("string(document('tab.xml')/result/row/fld1/@align)", "left");
    evalTest("string(document('tab.xml')/result/row/fld2/@align)",
	     "rightright");

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

    System.out.println("# of expected passes      " + expectedPasses);
    if (expectedFailures > 0)
      System.out.println("# of expected failures    " + expectedFailures);
    if (unexpectedPasses > 0)
      System.out.println("# of unexpected passes    " + unexpectedPasses);
    if (unexpectedFailures > 0)
      System.out.println("# of unexpected failures  " + unexpectedFailures);
  }

  public static void evalTest(String expr, String expected)
  {
    String result;
    try
      {
	result = eval(expr);
      }
    catch (Throwable ex)
      {
	if (verbose)
	  ex.printStackTrace(System.out);
	result = "*** caught " + ex.getClass().getName() + " ***";
      }
    boolean failureExpected = failureExpectedNext != null;
    if (expected.equals(result))
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
    CallContext ctx = new CallContext();
    ctx.consumer = interp.getOutputConsumer(out);

    ModuleExp mod = interp.parse(env, lexer);
    if (mod == null)
      return "*** end-of-file ***";
    mod.setName("atInteractiveLevel");  // FIXME
    if (lexer.checkErrors(err, 20))
      return "*** syntax error ***";

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

    mod.evalModule(env, ctx);
    ctx.runUntilDone();
    if (ch >= 0)
      return "*** junk at end of input ***";

    return new String(out.toCharArray());
  }
}
