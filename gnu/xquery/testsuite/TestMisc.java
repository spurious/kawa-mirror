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
    evalTest("for $car at $i in ('Ford', 'Chevy')," +
	     "$pet at $j in ('Cat', 'Dog') " +
	     "return ($i, '/', $car, '/', $j, '/', $pet, ';')",
	     "1/Ford/1/Cat;1/Ford/2/Dog;2/Chevy/1/Cat;2/Chevy/2/Dog;");

    evalTest("(3,4,5)[3]", "5");
    evalTest("1,((2,3)[false()]),5", "1 5");
    evalTest("1,((2 to 4)[true()]),5", "1 2 3 4 5");
    evalTest("(for $y in (5,4) return <b>{10+$y}</b>)[2]", "<b>14</b>");

    evalTest("doc('tab.xml')/result",
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
    evalTest("doc('tab.xml')/result/row/fld2",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");
    evalTest("doc('tab.xml')/result/row[fld2]",
	     "<row>\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\">12</fld2>\n" +
	     "</row><row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("doc('tab.xml')/result/row/*",
	     "<fld1>a1</fld1><fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1><fld2 align=\"right\">22</fld2>");

    evalTest("doc('tab.xml')/result/row[2]",
	     "<row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("for $x in doc('tab.xml')/result/row[2]/node()" +
	     "  return ('[',$x,']')",
	     "[\n][<fld1 align=\"left\">b1</fld1>][\n" +
	     "][<fld2 align=\"right\">22</fld2>][\n]");
    evalTest("for $x in doc('tab.xml')/result/row[2]/text()" +
	     "  return ('[',$x,']')",
	     "[\n][\n][\n]");
    evalTest("for $x in doc('tab.xml')/result/row[2]//text()" +
	     "  return ('[',$x,']')",
	     "[\n][b1][\n][22][\n]");
    evalTest("doc('tab.xml')/result/row/*[2]",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");

    evalTest("for $x in <T>r1<fld1>a1</fld1><fld3/>r2<fld2>12</fld2></T>" +
	     "  /node()" +
	     "    return ('[',$x,']')",
	     "[r1][<fld1>a1</fld1>][<fld3 />][r2][<fld2>12</fld2>]");

    evalTest("(doc('tab.xml')/result/row/*)[2]",
	     "<fld2 align=\"right\">12</fld2>");
    evalTest("(doc('tab.xml')/result/row/*)[2 to 3]",
	     "<fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1>");
    evalTest("(doc('tab.xml')/result/row/*)[position()>1]",
	     "<fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1><fld2 align=\"right\">22</fld2>");
    evalTest("(doc('tab.xml')/result/row/*)[position()>1][2]",
	     "<fld1 align=\"left\">b1</fld1>");

    evalTest("doc('tab.xml')/result/row/(fld2,fld1)",
	     "<fld1>a1</fld1><fld2 align=\"right\">12</fld2><fld1 align=\"left\">b1</fld1><fld2 align=\"right\">22</fld2>");

    evalTest("string-value(doc('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\nc1\n33\n44\nc2\n\n\n");
    evalTest("string(doc('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\nc1\n33\n44\nc2\n\n\n");
    evalTest("string(doc('tab.xml')/result/row/fld1/@align)", "left");
    evalTest("string(doc('tab.xml')/result/row/fld2/@align)",
	     "rightright");
 
    evalTest("for $x in children(<a>xy{3+4}kl<c>def</c>{9}{11}</a>)" +
	     "  return ('[',$x,']')",
	     "[xy 7 kl][<c>def</c>][9 11]");
    evalTest("children(<a>xy{3+4}kl<c>def</c>{9}{11}</a>)",
	     "xy 7 kl<c>def</c>9 11");

    evalTest("<a>aab</a> ='aab'", "true");
    evalTest("<a>abc</a>='abb'", "false");

    evalTest("string(<a>{'aa''bb&#88;cc&#x5a;dd'}</a>)", "aa'bbXccZdd");

    evalTest("doc('tab.xml')/result/row[fld1]",
	     "<row>\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\">12</fld2>\n" +
	     "</row><row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("doc('tab.xml')/result/row[fld3]", "");
    evalTest("doc('tab.xml')/result/row/fld1[@align]",
	     "<fld1 align=\"left\">b1</fld1>");
    evalTest("doc('tab.xml')/result/row/fld2[@align]",
	     "<fld2 align=\"right\">12</fld2><fld2 align=\"right\">22</fld2>");
    evalTest("'a',doc('tab.xml')/result/row/fld1[@align='left']",
	     "a<fld1 align=\"left\">b1</fld1>");
    evalTest("'a',doc('tab.xml')/result/row/fld1[@align='right']", "a");

    evalTest("let $x:=12,\n" +
	     "    $y:=<a>{$x+$x}</a>\n" +
	     "  return <b atr1='11' atr2=\"{$x}\">{($y,99,$y)}</b>",
	     "<b atr1=\"11\" atr2=\"12\"><a>24</a>99<a>24</a></b>");

    evalTest("let $el := 'elm' return "
	     + "document{element {$el} {attribute at{\"abc\"}, \"data\"}}/elm",
	     "<elm at=\"abc\">data</elm>");

    evalTest("let $a := <a at1='val1'><b/><c/></a>,"
	     + "  $b0 := <b/>,"
	     + "  $b := $a/b return"
	     + " ($a is $a, $a << $b, $b >> $b,"
	     + "  $a isnot $b, $b, $b0, $b is $b0)",
	     "true true false true <b /> <b /> false");
    evalTest("let $a := <a at1='val1'><b/><c/></a>,"
	     + " $b := $a/b, $c := $a/c return"
	     + " for $n in distinct-nodes(($c, $a/@at1, $a, $c, $b, $b, $c))"
	     + " return ('[', $n, ']')",
	     "[<a at1=\"val1\"><b /><c /></a>][ at1=\"val1\"][<b />][<c />]");

    // Boundary whitsapce (xmlspace) tests:
    evalTest("declare xmlspace preserve;\n"
	     + "for $n in (<a> <b/> {' x '} </a>)/node() return ($n,';')",
	     " ;<b/>;  x  ;");
    evalTest("declare xmlspace skip;\n"
	     + "for $n in (<a> <b/> {' x '} </a>)/node() return ($n,';')",
	     "<b/>; x ;");
    evalTest("declare xmlspace skip;\n"
	     + "for $n in (<a> x <b/> y<c/>&#x20;</a>)/node() return ($n,';')",
	     " x ;<b/>; y;<c/>; ;");
    evalTest("for $n in (<a> <b/> </a>)/node() return ($n,';')",
	     "<b/>;");

    // Simple namespace tests.
    evalTest("declare namespace xx='XXX';\n <xx:a>XX</xx:a>", "<xx:a>XX</xx:a>");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='XXX';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x2:ab)",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x2:ab)",
	     "X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*)",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*:*)",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x1:*)",
	     "X1");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*:ab)",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:cd>X2</x2:cd></top>)/*:cd)",
	     "X2");
    evalTest("declare namespace h='H';\n"
	     + "string(doc('tab.xml')/result/h:row)",
	     "\nc1\n33\n44\nc2\n");
    evalTest("declare namespace xx='H';\n"
	     + "string(doc('tab.xml')/result/xx:row)",
	     "\nc1\n33\n44\nc2\n");
    evalTest("string(doc('tab.xml')/result/*:row)",
	     "\na1\n12\n\nb1\n22\n\nc1\n33\n44\nc2\n");
    evalTest("string(doc('tab.xml')/result/*:row/*:fld1)",
	     "a1b1c1c2");
    evalTest("declare namespace k='J';\n"
	     + "string(doc('tab.xml')/result/*:row/k:fld1)",
	     "c1c2");
    evalTest("declare namespace k='J';\n"
	     + "string(doc('tab.xml')/result/*:row[k:fld1])",
	     "\nc1\n33\n44\nc2\n");

    evalTest("doc('tab.xml')/result/row[1]/descendant::*",
	     "<fld1>a1</fld1><fld2 align=\"right\">12</fld2>");
    evalTest("for $x in doc('tab.xml')/result/row[1]/descendant::node() return ($x,';')",
	     "\n;<fld1>a1</fld1>;a1;\n;<fld2 align=\"right\">12</fld2>;12;\n;");
    evalTest("doc('tab.xml')/result/row[1]/descendant::text()",
	     "a112");
    evalTest("doc('tab.xml')/result/row[1]/descendant-or-self::*",
	     "<row><fld1>a1</fld1>"
	     + "<fld2 align=\"right\">12</fld2></row>"
	     + "<fld1>a1</fld1><fld2 align=\"right\">12</fld2>");

    // Based on bugs reported by Francois Leygues <vizawalou@wanadoo.fr>:
    evalTest("let $bx := <b x='xx'></b> return"
	     + " let $x := <a>{for $y in $bx return $y}</a>"
	     + "  return $x/b",
	     "<b x=\"xx\" />");
    evalTest("element r {let $y := <b x='1'/>"
	     + " let $x:=<a>{$y}</a> return $x/b/@x}",
	     "<r x=\"1\" />");
    evalTest("declare function x(){<a><b x='1'/><b x='2'/></a>};"
	     + " let $i := <a>{for $a in x()/b return $a}</a>  return $i/b/@x",
	     " x=\"1\" x=\"2\"");
    evalTest("declare function s(){ <a x='10'>{for $n in (<a x='2'/>) return ($n) }</a>};"
	     + " let $st := s()/a return ("
	     + " '[',$st/@x ,'] [',$st ,']')",
	     "[ x=\"2\"] [<a x=\"2\" />]");

    // Testcase from <Seshukumar_Adiraju@infosys.com>:
    evalTest("let $books := "
	     + "<books><book id='book1'/><book id='book2'/></books> "
	     + "for $book in $books/book return <p>{string($book/@id)}</p>",
	     "<p>book1</p><p>book2</p>");

    evalTest("for $n in children(<a>xx<b/>yy</a>) return $n instanceof node()",
	     "true true true");
    evalTest("for $n in children(<a>xx<b/>yy</a>) return $n instanceof text ( )",
	     "true false true");
    evalTest("for $n in children(<a>xx<b/>yy</a>) return $n instanceof element(a,*)",
	     "false false false");
    evalTest("for $n in <a>xx<b/>yy</a>/node() return $n instanceof element(b,*)",
	     "false true false");
    // FIXME: evalTest("<a>xx<b/>yy</a>/node() instanceof node()", "false");
    evalTest("<a>xx<b/>yy</a>/node() instanceof node()?", "false");
    evalTest("<a>xx<b/>yy</a>/node() instanceof node()+", "true");
    evalTest("<a>xx<b/>yy</a>/node() instanceof node()*", "true");
    evalTest("<a>xx<b/>yy</a>/node() instanceof item()+", "true");
    evalTest("(3,4,5) instanceof item()+", "true");
    evalTest("('a','b') instanceof string+", "true");
    evalTest("(2,3) instanceof string?", "false");
    evalTest("(2,3) instanceof string+", "false");
    evalTest("() instanceof string?", "true");
    evalTest("() instanceof string+", "false");
    evalTest("() instanceof string*", "true");
    evalTest("('2') instanceof string?", "true");
    evalTest("('2') instanceof string+", "true");
    evalTest("('2') instanceof string*", "true");
    evalTest("('2','3') instanceof string?", "false");
    evalTest("('2','3') instanceof string+", "true");
    evalTest("('2','3') instanceof string*", "true");

    evalTest("declare namespace Int='class:java.lang.Integer';\n"
	     + "Int:toHexString(266)", "10a");
    evalTest("declare namespace File='class:java.io.File';\n"
	     + "declare function make-file ($x as string) {File:new($x)};\n"
	     + "declare function parent ($x) {java.io.File:getParent($x)};\n"
	     + "parent(make-file('dir/mine.txt'))", "dir");
    evalTest("java.lang.Integer:toHexString(255)", "ff");

    // String functions
    evalTest("substring('motor car', 6)", "car");
    evalTest("substring('metadata', 4, 3)", "ada");
    // evalTest("substring('metadata', -INF, 3)", "met");

    evalTest("(1 to 20)[. mod 5 = 0]", "5 10 15 20");
    evalTest("(1 to 20)[. mod 5 ge 3]", "3 4 8 9 13 14 18 19");
    evalTest("(99 to 0)[5]", "95");
    evalTest("-10 to -2", "-10 -9 -8 -7 -6 -5 -4 -3 -2");

    // Check for catching errors:
    evalTest("+ +", "*** syntax error - <string>:1:3: missing expression");

    evalTest("declare namespace x1='XXX",
	     "*** caught SyntaxException - <string>:1:26: "
	     + "unexpected end-of-file in string");

    evalTest("unescaped-data('<?--->'),let $x:=unescaped-data('an &amp;oslash;') return <b>{unescaped-data('<![CDATA[saw]]>')} {$x}</b>",
	     "<?---><b><![CDATA[saw]]> an &oslash;</b>");

    evalTestIdAttrs("doc('outline.xml')/book/part/chapter/ancestor::*",
		    "b1;P1;");
    evalTestIdAttrs("doc('outline.xml')/book/part/"
		    +"chapter/ancestor-or-self::node()",
		    ";b1;P1;c1;c2;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s1']/following-sibling::*",
		    "s2;s3;");
    evalTestIdAttrs("doc('outline.xml')//chapter/self::*",
		    "c1;c2;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"para[@id='p31']/preceding::*",
		    "s1;s11;s2;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s5']/preceding-sibling::*",
		    "s4;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"chapter[@id='c1']/following::*",
		    "c2;s4;s5;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s1']/(/book)",
		    "b1;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s1']/(//chapter)",
		    "c1;c2;");

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

  private static void evalTestIdAttrs(String expr, String expected)
  {
    evalTest("for $x in (" + expr + ") return (string($x/@id),';')", expected);
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
	  {
	    throwable = ((WrappedException) ex).getException();
	    if (throwable != null)
	      ex = throwable;
	  }
	throwable = ex;
	// throwable.printStackTrace();
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
	Compilation comp = interp.parse(env, lexer);
	if (comp == null)
	  break; // return "*** end-of-file ***";
	ModuleExp mod = comp.getModule();
	mod.setName("atInteractiveLevel");  // FIXME
	ModuleExp.evalModule(env, ctx, comp);
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

	ctx.runUntilDone();
      }
    /*
    if (ch >= 0)
      return "*** junk at end of input ***";
    */

    return new String(out.toCharArray());
  }
}
