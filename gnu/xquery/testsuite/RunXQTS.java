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

  Stack outputFileAlts = new Stack();

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
            Object saveIndent = XMLPrinter.indentLoc.get(null);
            XMLPrinter.indentLoc.set("pretty");
            xqlog.beginDocument();
            XMLPrinter.indentLoc.set(saveIndent);
            xqlog.beginGroup("test-suite-result", "test-suite-result");
	    Document.parse(runner.catalog, runner);
            runner.summaryReport();
            xqlog.endGroup("test-suite-result");
            xqlog.endDocument();
            xqlog.close();
	  }
	catch (Throwable ex)
	  {
            ex.printStackTrace();
	    System.err.println("caught "+ex+" while processing "+args[i]);
	  }
      }
    TestMisc.printSummary();
  }

  int nesting = 0;
  String currentTag;
  boolean inStartTag;
  int attrValueStart;
  // Start in cout's buffer of current element, indexed by nesting level.
  int[] elementStartIndex = new int[20];
  String expectedError;
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
    expectedFailures.put("Axes085", "actually pass? different char encoding");    
    expectedFailures.put("NodTest003", "actually pass? different char encoding");    
    expectedFailures.put("op-subtract-dayTimeDuration-from-dateTime-1", "straddles time change");
    expectFailures("surrogates03|surrogates06|surrogates07|surrogates08|surrogates10", "surrogates not properly implemented");
    expectFailures("fn-lang-2|fn-lang-3|fn-lang-4|fn-lang-5|fn-lang-6|"
                   +"fn-lang-7|fn-lang-8|fn-lang-9|fn-lang-10|fn-lang-11",
                   "fn:lang not implemented");

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
    attributes.clear();
    inStartTag = true;
    currentTag = typeName;
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

  public void handleStartTag ()
  {
    elementStartIndex[nesting] = cout.length();
    if ("test-suite".equals(currentTag))
      {
        XQueryQueryOffsetPath = attributes.getValue("XQueryQueryOffsetPath");
        XQueryXQueryOffsetPath = attributes.getValue("XQueryXQueryOffsetPath");
        XQueryFileExtension = attributes.getValue("XQueryFileExtension");
        XQueryXFileExtension = attributes.getValue("XQueryXFileExtension");
        ResultOffsetPath = attributes.getValue("ResultOffsetPath");
      }
    else if ("test-group".equals(currentTag))
      {
        xqlog.writeComment("test-group "+attributes.getValue("name"));
      }
    else if ("test-case".equals(currentTag))
      {
        testName = attributes.getValue("name");
        testFilePath = attributes.getValue("FilePath");
        testQueryName = null;
        expectedError = null;
        outputFileAlts.clear();
        manager.clear();
      }
    else if ("query".equals(currentTag))
      {
        testQueryName = attributes.getValue("name");
      }
    else if ("source".equals(currentTag))
      {
        String ID = attributes.getValue("ID");
        String filename = attributes.getValue("FileName");
        sources.put(ID, filename);
      }
    else if (testName == null && "module".equals(currentTag))
      {
        String ID = attributes.getValue("ID");
        String filename = attributes.getValue("FileName");
        modules.put(ID, filename);
      }
    inStartTag = false;
  }

  String testName;
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

    xqlog.beginAttribute("result", "result");
    xqlog.writeChars(result);
    xqlog.endAttribute();

    if (failed && failExpected != null)
      {
        xqlog.beginAttribute("reason", "reason");
        xqlog.writeChars(failExpected.toString());
        xqlog.endAttribute();
      }
    if (comment != null)
      {
        xqlog.beginAttribute("comment", "comment");
        xqlog.writeChars(comment);
        xqlog.endAttribute();
      }
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
        if (error != null && expectedError != null
            && expectedError.equals(error.code))
          {
            report("pass", null);
            return;
          }
        if ("XQST0009".equals(error.code))
          {

            if (failExpected == null)
              failExpected = "'import schema' not implemented";
            report("fail", null);
          }
        else if (expectedError != null)
          report("pass", "static error: "+error+" expected:"+expectedError);
        else
          report("fail", "static error: "+error.message);
        return;
      }
    in.close();

    CallContext ctx = CallContext.getInstance();
    gnu.lists.Consumer save = ctx.consumer;
    CharArrayOutPort out = new CharArrayOutPort();
    XMLPrinter xout = new XMLPrinter(out, false);
    ctx.consumer = xout;
    try
      {
        ModuleExp.evalModule(env, ctx, comp, null, null);
      }
    catch (Throwable ex)
      {
        if (ex instanceof NumberFormatException
            && "FORG0001".equals(expectedError))
          report("pass", "caught NumberFormatException expected:"+expectedError);
        else if (ex instanceof ClassCastException
                 && ("XPTY0004".equals(expectedError)
                     || "XPTY0020".equals(expectedError)
                     || "FORG0001".equals(expectedError)
                     || "FOAR0002".equals(expectedError)))
          report("pass", "caught ClassCastException expected:"+expectedError);
        else if (expectedError != null)
          report("pass", "caught "+ex+" expected:"+expectedError);
        else
          {
            report("fail", "caught "+ex);
            if (verbose)
              {
                xqlog.beginComment();
                ex.printStackTrace(xqlog);
                xqlog.endComment();
              }
          }
        return;
      }

    if (messages.seenErrors())
      {
        if (expectedError != null)
          report("pass", "error: "+messages.getErrors()+" expected: "+expectedError);
        else
          report("fail", "error: "+messages.getErrors());
        return;
      }

    if (expectedError != null)
      {
        report("fail", "expected error: "+expectedError);
        return;
      }

    String actual = new String(out.toCharArray());
    byte[] expectedBytes = new byte[1024];
    xout.close();
    ctx.consumer = save;
    
    int numOutputFileAlts = outputFileAlts.size();
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
        String expected = new String(expectedBytes, 0, expectedLength, "UTF-8");
        expected = expected.replaceAll("\r", "");
        actual = actual.replaceAll("\r", "");
        if (expected.equals(actual))
          {
            report("pass", null);
            break;
          }
        else if (("XML".equals(compare) || "Fragment".equals(compare))
                 && equalsXML(actual, expected))
          {
            report("pass", "(ignoring any spaces)");
            break;
          }
        else if (ialt == numOutputFileAlts-1)
          {
            report("fail", null);
            if (verbose && expectedFailures.get(testName) == null)
              {
                xqlog.beginComment();
                xqlog.writeChars("compare: ");
                xqlog.writeChars(compare);
                xqlog.endComment();
                xqlog.beginComment();
                xqlog.writeChars(" expected: [");
                xqlog.writeChars(expected);
                xqlog.writeChar(']');
                xqlog.endComment();
                xqlog.beginComment();
                xqlog.writeChars(" actual: [");
                xqlog.writeChars(actual);
                xqlog.writeChar(']');
                xqlog.endComment();
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
    if ("test-case".equals(typeName))
      {
        if (--maxTests == 0)  System.exit(0); // FIXME
        xqlog.beginGroup("test-case", "test-case");
        xqlog.beginAttribute("name", "name");
        xqlog.writeChars(testName);
        xqlog.endAttribute();
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
    else if ("expected-error".equals(typeName))
      {
        expectedError = cout.toSubString(elementStartIndex[nesting]);
        //System.err.println("expected-error: '"+expectedError+"'");
      }
    else if ("input-query".equals(typeName))
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
            //System.err.println("input-query: evaluated to "+value);
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
   else if ("input-file".equals(typeName))
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
   else if ("input-URI".equals(typeName))
      {
        String inputFile = cout.toSubString(elementStartIndex[nesting]);
        String variable = attributes.getValue("variable");
        String path = "file://" + directory + '/' + sources.get(inputFile);
        Symbol symbol = Symbol.make("", variable);
        Environment.getCurrent().put(symbol, null,
                                     gnu.kawa.xml.XDataType.toURI(path));
      }
    else if ("output-file".equals(typeName))
      {
        outputFileAlts.push(cout.toSubString(elementStartIndex[nesting]));
        compare = attributes.getValue("compare");
      }
    else if (testName != null && "module".equals(typeName))
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
    if ("test-suite".equals(typeName) && nesting == 0)
      inTestSuite = false;
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
    String uri = null;
    String local = attributeName;
    cout.setLength(attrValueStart);
    attributes.addAttribute(uri, local, attributeName, "CDATA", attrValue);
  }

  public void beforeContent ()
  {
    if (! inAttribute && inStartTag)
      handleStartTag();
  }
}
