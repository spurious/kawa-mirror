package gnu.xquery.testsuite;
import java.io.*;
import java.util.Hashtable;
import gnu.lists.*;
import gnu.text.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.xml.*;
import gnu.kawa.xml.Document;
import gnu.mapping.Symbol;
import gnu.xquery.lang.*;
import org.xml.sax.helpers.AttributesImpl;
import java.util.Stack;

/** Run a suite of XQuery tests, as read from an xml file. */

public class RunXQTS extends FilterConsumer
{
  Hashtable sources = new Hashtable();
  Hashtable modules = new Hashtable();
  static XQuery xqueryLanguage = XQuery.getInstance();

  Hashtable expectedFailures = new Hashtable();
  ModuleManager manager = ModuleManager.getInstance();
  Object failExpected;

  boolean verbose = true;

  String directory;
  String catalog;
  String XQTSVersion;
  String ResultOffsetPath;
  String XQueryQueryOffsetPath;
  String XQueryXQueryOffsetPath;
  String XQueryFileExtension;
  String XQueryXFileExtension;
  String compare;

  int passCount;
  int xpassCount;
  int failCount;
  int xfailCount;
  int cannotTellCount;

  Stack outputFileAlts = new Stack();
  /** Set of expected error codes.  The format is "|Error1}...|ErrorN|". */
  StringBuffer expectedErrorsBuf = new StringBuffer("|");
  /** Same as expectedErrorBuf.toString() after collecting expected-errors. */
  String expectedErrors;

  String logFileName = "XQTS.log";
  XMLPrinter xqlog;

  private void summaryReport (int count, String label)
  {
    if (count > 0)
      {
        System.out.print(label);
        System.out.println(count);
      }
  }

  private void summaryReport ()
  {
    summaryReport(passCount, "# of expected passes      ");
    summaryReport(xfailCount, "# of expected failures    ");
    summaryReport(xpassCount, "# of unexpected successes ");
    summaryReport(failCount, "# of unexpected failures  ");
    summaryReport(cannotTellCount, "# of cannot-tell (Inspect) results  ");
  }

  public static final String XQTS_RESULT_NAMESPACE
  = "http://www.w3.org/2005/02/query-test-XQTSResult";

  static Object testSuiteResultGroupType;
  static
  {
    NamespaceBinding namespaceNodes
      = new NamespaceBinding(null, XQTS_RESULT_NAMESPACE,
            new NamespaceBinding("q",
                                 XQuery.QEXO_FUNCTION_NAMESPACE,
                                 NamespaceBinding.predefinedXML));
    Symbol sym = Symbol.make(XQTS_RESULT_NAMESPACE, "test-suite-result", "");
    testSuiteResultGroupType = new XName(sym, namespaceNodes);
  }
  static Object testRunGroupType
    = Symbol.make(XQTS_RESULT_NAMESPACE, "test-run", "");
  static Object testSuiteGroupType
    = Symbol.make(XQTS_RESULT_NAMESPACE, "test-suite", "");
  static Object testCaseGroupType
    = Symbol.make(XQTS_RESULT_NAMESPACE, "test-case", "");

  private void writeBeginGroup (String name)
  {
    xqlog.beginGroup(name, Symbol.make(XQTS_RESULT_NAMESPACE, name, ""));
  }

  private void writeBeginAttribute (String name)
  {
    xqlog.beginAttribute(name, name);
  }

  private void writeAttribute (String name, String value)
  {
    writeBeginAttribute(name);
    xqlog.writeChars(value);
    xqlog.endAttribute();
  }

  private void writeQexoAttribute (String name, String value)
  {
    xqlog.beginAttribute("q:"+name,
                         Symbol.make(XQuery.QEXO_FUNCTION_NAMESPACE,
                                     name, "q"));
    xqlog.writeChars(value);
    xqlog.endAttribute();
  }

  public static void main (String[] args)
  {
    gnu.xquery.lang.XQuery.registerEnvironment();
    for (int i = 0;  i < args.length;  i++)
      {
	try
	  {
            RunXQTS runner = new RunXQTS(new CharArrayOutPort());
            runner.directory = args[i];
            runner.catalog = runner.directory + "/XQTSCatalog.xml";
            System.err.println("catalog: "+runner.catalog);
            XMLPrinter xqlog
              = new XMLPrinter(new BufferedOutputStream(new FileOutputStream(runner.logFileName)),
                               runner.logFileName);
            runner.xqlog = xqlog;
            xqlog.setPrintXMLdecl(true);
            xqlog.setStyle("xml");
            xqlog.useEmptyElementTag = 1;
            Object saveIndent = XMLPrinter.indentLoc.get(null);
            XMLPrinter.indentLoc.set("pretty");
            xqlog.beginDocument();
            XMLPrinter.indentLoc.set(saveIndent);

	    Document.parse(runner.catalog, runner);
            xqlog.endDocument();
            runner.summaryReport();
            xqlog.close();
	  }
	catch (Throwable ex)
	  {
            ex.printStackTrace();
	    System.err.println("caught "+ex+" while processing "+args[i]);
	  }
      }
  }

  int nesting = 0;
  String currentTag;
  Object currentElementType;
  Symbol currentElementSymbol;
  Stack elementTypeStack = new Stack();
  boolean inStartTag;
  int attrValueStart;
  // Start in cout's buffer of current element, indexed by nesting level.
  int[] elementStartIndex = new int[20];
  AttributesImpl attributes = new AttributesImpl();

  String query = null;
  String expect = null;

  CharArrayOutPort cout;

  public RunXQTS(CharArrayOutPort out)
  {
    super(out);
    this.cout = out;

    //badFormatting("CastableAs200");
    expectedFailures.put("static-context-1", "unchecked unknownType in element(*,TypeName)");
    expectedFailures.put("NodTest003", "actually pass? different char encoding");    
    expectedFailures.put("op-subtract-dayTimeDuration-from-dateTime-1", "straddles time change");
    expectFailures("surrogates03|surrogates06|surrogates07|surrogates08|surrogates10", "surrogates not properly implemented");
    expectFailures("PathExprErr-2", "no check for mixed nodes+atomics from path expression");
    /* It's sort-of caught - but as a ClassFormatError.
    expectFailures("function-declaration-022|K-FunctionProlog-26|"
                   +"K-FunctionProlog-27|K-FunctionProlog-28",
                   "duplicate function definition not properly caught");
    */
  }

  private void badFormatting(String testName)
  {
    expectedFailures.put(testName, "incorrect double/float formating");    
  }

  private void expectFailures (String testNames, String reason)
  {
    while (testNames != null)
      {
        int dot = testNames.indexOf('|');
        String testName;
        if (dot >= 0)
          {
            testName = testNames.substring(0, dot);
            testNames = testNames.substring(dot+1);
          }
        else
          {
            testName = testNames;
            testNames = null;
          }
        if (testName.length() > 0)
          expectedFailures.put(testName, reason);
      }
  }

  public void beginGroup(String typeName, Object type)
  {
    if (inStartTag)
      handleStartTag();
    attributes.clear();
    inStartTag = true;
    currentTag = typeName;
    elementTypeStack.push(currentElementType);
    currentElementType = type;
    currentElementSymbol = type instanceof Symbol ? (Symbol) type : null;
    /*
    System.err.println("beginGroup "+typeName);
    if ("test-suite".equals(typeName) && nesting == 0)
      inTestSuite = true;
    else if ("test-group".equals(typeName))
      {
      }
    else if ("test".equals(typeName)
	&& (nesting == 0 || (inTestSuite && nesting == 1)))
      inTest = true;
    else if (inTestSuite ? nesting == 2 : nesting == 1)
      {
	cout.setLength(0);
	currentTag = typeName;
      }
    else if (currentTag == null)
      throw new RuntimeException("saw <"+typeName+"> not in <test>");
    else
      base.beginGroup(typeName, type);
    */
    nesting++;
  }

  boolean tagMatches (String localName)
  {
    if (localName.equals(currentElementSymbol.getLocalName()))
      // also check uri FIXME
      return true;
    return false;
  }

  public void handleStartTag ()
  {
    elementStartIndex[nesting] = cout.length();
    if (tagMatches("test-suite"))
      {
        XQueryQueryOffsetPath = attributes.getValue("XQueryQueryOffsetPath");
        XQueryXQueryOffsetPath = attributes.getValue("XQueryXQueryOffsetPath");
        XQueryFileExtension = attributes.getValue("XQueryFileExtension");
        XQueryXFileExtension = attributes.getValue("XQueryXFileExtension");
        ResultOffsetPath = attributes.getValue("ResultOffsetPath");
        XQTSVersion = attributes.getValue("version");
 
        xqlog.beginGroup("test-suite-result", testSuiteResultGroupType);
        writeBeginGroup("implementation");
        writeAttribute("name", "Qexo");
        writeAttribute("version", kawa.Version.getVersion());
        writeBeginGroup("organization");
        writeAttribute("name", "GNU / Per Bothner");
        xqlog.endGroup("organization");
        writeBeginGroup("submittor");
        String user = System.getProperty("user.name");
        if ("bothner".equals(user))
          {
            writeAttribute("name", "Per Bothner");
            writeAttribute("email", "per@bothner.com");
          }
        else
          writeAttribute("name", user);
        xqlog.endGroup("submittor");
        xqlog.endGroup("implementation");
        writeBeginGroup("syntax");
        xqlog.writeChars("XQuery");
        xqlog.endGroup("syntax");
        xqlog.beginGroup("test-run", testRunGroupType);
        StringBuffer sbuf = new StringBuffer();
        gnu.kawa.xml.XTimeType.dateTimeType.now().toStringDate(sbuf);
        writeAttribute("dateRun", sbuf.toString());
        xqlog.beginGroup("test-suite", testSuiteGroupType);
        writeAttribute("version", XQTSVersion);
        xqlog.endGroup("test-suite");
        xqlog.endGroup("test-run");

      }
    else if (tagMatches("test-group"))
      {
        xqlog.writeComment("test-group "+attributes.getValue("name"));
      }
    else if (tagMatches("test-case"))
      {
        testName = attributes.getValue("name");
        scenario = attributes.getValue("scenario");
        testFilePath = attributes.getValue("FilePath");
        testQueryName = null;
        outputFileAlts.clear();
        expectedErrorsBuf.setLength(1);
        manager.clear();
      }
    else if (tagMatches("query"))
      {
        testQueryName = attributes.getValue("name");
      }
    else if (tagMatches("source"))
      {
        String ID = attributes.getValue("ID");
        String filename = attributes.getValue("FileName");
        sources.put(ID, filename);
      }
    else if (testName == null && tagMatches("module"))
      {
        String ID = attributes.getValue("ID");
        String filename = attributes.getValue("FileName");
        modules.put(ID, filename);
      }
    inStartTag = false;
  }

  String testName;
  String scenario;
  String testQueryName;
  String testFilePath;
  String testQuery;

  int maxTests = -1;

  void report (String result, String comment)
  {
    boolean failed = "fail".equals(result);
    if (failExpected == null)
      {
        if (failed)
          failCount++;
        else if ("cannot tell".equals(result))
          cannotTellCount++;
        else
          passCount++;
      }
    else
      {
        if (failed)
          xfailCount++;
        else
          {
            System.out.println("XPASS: "+testName);
            xpassCount++;
          }
      }

    writeAttribute("result", result);

    if (failed && failExpected != null)
      {
        StringBuffer sbuf = new StringBuffer("(expected-to-fail: ");
        sbuf.append(failExpected.toString());
        sbuf.append(')');
        if (comment != null)
          {
            sbuf.append("; ");
            sbuf.append(comment);
          }
        comment = sbuf.toString();
      }
    if (comment != null)
      writeAttribute("comment", comment);
  }

  public void evalTest (String testName)
    throws Throwable
  {
    failExpected = expectedFailures.get(testName);
    Environment env = Environment.getCurrent();
    SourceMessages messages = new SourceMessages();
    String filename
      = directory + '/' + XQueryQueryOffsetPath + testFilePath
      + testQueryName + XQueryFileExtension;
    InPort in;
    expectedErrors = expectedErrorsBuf.toString();
    try
      {
        in = InPort.openFile(filename);
      }
    catch (java.io.FileNotFoundException ex)
      {
        String xfilename = directory + '/' + XQueryXQueryOffsetPath 
          + testFilePath + testQueryName + XQueryXFileExtension;
        if (new java.io.File(xfilename).exists())
          {
            report("fail", "xqueryx not implemented");
            return;
          }
        throw ex;
      }
    Compilation comp;
    try
      {
        comp = xqueryLanguage.parse(in, messages, Language.PARSE_IMMEDIATE);
        if (messages.seenErrors())
          throw new SyntaxException(messages);
      }
    catch (SyntaxException ex)
      {
        in.close();
        SourceError error = messages.getErrors();
        String errorString = error == null ? "" : "|" + error.code + "|";
        if (expectedErrors.indexOf(errorString) >= 0)
          {
            report("pass", null);
            return;
          }
        if (errorString.equals("|XQST0009|"))
          {

            if (failExpected == null)
              failExpected = "'import schema' not implemented";
            report("fail", null);
          }
        else if (expectedErrors.length() > 1)
          report("pass", "static error: "+error+" expected:"+expectedErrors);
        else
          report("fail", "static error: "+error.message);
        return;
      }
    in.close();

    CallContext ctx = CallContext.getInstance();
    gnu.lists.Consumer save = ctx.consumer;
    CharArrayOutPort out = new CharArrayOutPort();
    XMLPrinter xout = new XMLPrinter(out, false);
    xout.useEmptyElementTag = 1;
    xout.escapeNonAscii = false;
    xout.canonicalizeCDATA = true;
    ctx.consumer = xout;
    try
      {
        ModuleExp.evalModule(env, ctx, comp, null, null);
      }
    catch (Throwable ex)
      {
        if (ex instanceof NumberFormatException
            && expectedErrors.indexOf("|FORG0001|") >= 0)
          report("pass", "caught NumberFormatException expected:"+expectedErrors);
        else if (ex instanceof ClassCastException
                 && (expectedErrors.indexOf("|XPTY0004|") >= 0
                     || expectedErrors.indexOf("|XPTY0020|") >= 0
                     || expectedErrors.indexOf("|FORG0001|") >= 0
                     || expectedErrors.indexOf("|FOAR0002|") >= 0))
          report("pass", "caught ClassCastException expected:"+expectedErrors);
        else if (expectedErrors.length() > 1)
          report("pass", "caught "+ex+" expected:"+expectedErrors);
        else
          {
            report("fail", "caught "+ex);
            if (verbose)
              {
                CharArrayWriter wr = new CharArrayWriter();
                PrintWriter pr = new PrintWriter(wr);
                ex.printStackTrace(pr);
                pr.flush();
                writeQexoAttribute("stack", wr.toString());
                wr.close();
              }
          }
        return;
      }

    if (messages.seenErrors())
      {
        if (expectedErrors.length() > 1)
          report("pass", "error: "+messages.getErrors()+" expected: "+expectedErrors);
        else
          report("fail", "error: "+messages.getErrors());
        return;
      }

    if ("trivial".equals(scenario))
      {
        failExpected = "trivial embedding not implemented";
        report("fail", null);
        return;
      }

    String actual = new String(out.toCharArray());
    byte[] expectedBytes = new byte[1024];
    xout.close();
    ctx.consumer = save;
    
    int numOutputFileAlts = outputFileAlts.size();
    boolean foundMatchingOutput = false;
    String expected = null;
    for (int ialt = 0;  ialt < numOutputFileAlts;  ialt++)
      {
        String outname  = directory + '/' + ResultOffsetPath + testFilePath
          + outputFileAlts.elementAt(ialt);
        FileInputStream expectStream = new FileInputStream(outname);
        int expectedLength = 0;
        for (;;)
          {
            int avail = expectedBytes.length-expectedLength;
            if (avail < 1024)
              {
                byte[] tmp = new byte[2*expectedBytes.length];
                System.arraycopy(expectedBytes, 0, tmp, 0, expectedLength);
                expectedBytes = tmp;
              }
            int n = expectStream.read(expectedBytes, expectedLength, avail);
            if (n < 0)
              break;
            expectedLength += n;
          }
        expectStream.close();
        expected = new String(expectedBytes, 0, expectedLength, "UTF-8");
        expected = expected.replaceAll("\r", "");
        actual = actual.replaceAll("\r", "");
        if (expected.equals(actual))
          {
            report("pass", null);
            foundMatchingOutput = true;
            break;
          }
        else if ("Inspect".equals(compare))
          {
            report("cannot tell", null);
            foundMatchingOutput = true;
            break;
          }
        else if (("XML".equals(compare) || "Fragment".equals(compare))
                 && equalsXML(actual, expected))
          {
            report("pass", "(ignoring any spaces)");
            foundMatchingOutput = true;
            break;
          }
      }

    if (! foundMatchingOutput)
      {
        if (expectedErrors.length() > 1)
          {
            report("fail", "expected error: "+expectedErrors);
            return;
          }
        else
          {
            report("fail", null);
            if (verbose && expectedFailures.get(testName) == null)
              {
                writeQexoAttribute("compare", compare);
                writeQexoAttribute("expected", expected);
                writeQexoAttribute("actual", actual);
              }
          }
      }
  }

  public boolean equalsXML(String arg1 /* result */, String arg2 /*expected*/)
  {
    int len1 = arg1.length();
    int len2 = arg2.length();
    int i1 = 0, i2 = 0;
    for (;;)
      {
        if (i1 == len1 && i2 == len2)
          return true;
        int c1 = i1 == len1 ? -1 : arg1.charAt(i1);
        int c2 = i2 == len2 ? -1 : arg2.charAt(i2);
        if (c1 == c2)
          {
            i1++;
            i2++;
          }
        else if (c1 == ' ' || c1 == '\n' || c1 == '\t' || c1 == '\r')
          {
            i1++;
          }
        else if (c2 == ' ' || c2 == '\n' || c2 == '\t' || c2 == '\r')
          {
            i2++;
          }
        else
          return false;
      }
  }

  public void endGroup(String typeName)
  {
    if (inStartTag)
      handleStartTag();
    if (tagMatches("test-case"))
      {
        if (--maxTests == 0)  System.exit(0); // FIXME
        xqlog.beginGroup("test-case", testCaseGroupType);
        writeAttribute("name", testName);
        try
          {
            // Other attributes and <test-case> body written by evalTest.
            evalTest(testName);
          }
        catch (Throwable ex)
          {
            System.err.println("test-case name:"+testName);
            System.err.println("caught "+ex);
            ex.printStackTrace();
          }
        xqlog.endGroup("test-case");
        //xqlog.flush();
        testName = null;
      }
    else if (tagMatches("expected-error"))
      {
        expectedErrorsBuf.append(cout.toSubString(elementStartIndex[nesting]));
        expectedErrorsBuf.append('|');
      }
    else if (tagMatches("input-query"))
      {
        String variable = attributes.getValue("variable");
        Symbol symbol = Symbol.make("", variable);
        String name = attributes.getValue("name");
        String filename
          = directory + '/' + XQueryQueryOffsetPath + testFilePath
          + name + XQueryFileExtension;
        InPort in;
        try
          {
            in = InPort.openFile(filename);
            Object value = XQuery.getInstance().eval(in);
            in.close();
            Environment current = Environment.getCurrent();
            current.put(symbol, null, value);
          }
        catch (Throwable ex)
          {
            System.err.println("input-query: cannot open "+filename);
            System.err.println("caught "+ex);
            ex.printStackTrace();
            System.exit(-1);
          }
      }
    else if (tagMatches("input-file"))
      {
        String inputFile = cout.toSubString(elementStartIndex[nesting]);
        // KLUDGE around testsuite bug!
        if ("userdefined".equals(inputFile))
          inputFile = "emptydoc";
        String variable = attributes.getValue("variable");
        String path = directory + '/' + sources.get(inputFile);

        Symbol symbol = Symbol.make("", variable);
        Object value;
        try
          {
            value = gnu.kawa.xml.Document.parseCached(path);
            Environment current = Environment.getCurrent();
            current.put(symbol, null, value);
          }
        catch (Throwable ex)
          {
            System.err.println("caught "+ex);
            System.err.println("reading data file "+path);
            System.err.println("inputFile:"+inputFile+" variable:"+variable+" path:"+path);
            ex.printStackTrace();
            System.exit(-1);
          }
      }
    else if (tagMatches("input-URI"))
      {
        String inputFile = cout.toSubString(elementStartIndex[nesting]);
        String variable = attributes.getValue("variable");
        String path = "file://" + directory + '/' + sources.get(inputFile);
        Symbol symbol = Symbol.make("", variable);
        Environment.getCurrent().put(symbol, null,
                                     gnu.kawa.xml.XDataType.toURI(path));
      }
    else if (tagMatches("output-file"))
      {
        outputFileAlts.push(cout.toSubString(elementStartIndex[nesting]));
        compare = attributes.getValue("compare");
      }
    else if (tagMatches("test-suite"))
      {
        xqlog.endGroup("test-suite-result");
      }
    else if (testName != null && tagMatches("module"))
      {
        String uri = attributes.getValue("namespace");
        String module = cout.toSubString(elementStartIndex[nesting]);
        String mfile = (String) modules.get(module);
        String mpath = directory + '/' + mfile + XQueryFileExtension;
        String mclass = Compilation.mangleURI(uri)
          + '.' + XQuery.makeClassName(mpath);
        manager.register(mclass, mpath, uri);
      }
    /*
    else if ("test".equals(typeName)
	&& (nesting == 0 || (inTestSuite && nesting == 1)))
      {
	inTest = false;
	TestMisc.evalTest(query, expect);
      }
    else if (inTestSuite ? nesting == 2 : nesting == 1)
      {
	if ("query".equals(typeName))
	  query = sout.toString();
	else if ("expect".equals(typeName))
	  expect = sout.toString();
	currentTag = null;
      }
    else
      base.endGroup(typeName);
    */
    cout.setLength(elementStartIndex[nesting]);
    nesting--;
    Object type = elementTypeStack.pop();
    currentElementType = type;
    currentElementSymbol = type instanceof Symbol ? (Symbol) type : null;
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    super.beginAttribute(attrName, attrType);
    attrValueStart = cout.length();
  }

  public void endAttribute()
  {
    super.endAttribute();
    String attrValue = cout.toSubString(attrValueStart, cout.length()-1);
    String uri;
    String local;
    String qname;
    if (attributeType instanceof Symbol)
      {
        Symbol sym = (Symbol) attributeType;
        uri = sym.getNamespaceURI();
        local = sym.getLocalPart();
        String prefix = sym.getPrefix();
        if (prefix == null || prefix.length() == 0)
          qname = local;
        else
          qname = prefix+":"+local;
      }
    else
      {
        uri = null;
        local = attributeName;
        qname = attributeName;
      }
    cout.setLength(attrValueStart);
    attributes.addAttribute(uri, local, qname, "CDATA", attrValue);
  }

  public void beforeContent ()
  {
    if (! inAttribute && inStartTag)
      handleStartTag();
  }
}
