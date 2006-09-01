package gnu.xquery.testsuite;
import java.io.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.kawa.xml.Document;

/** Run a suite of XQuery tests, as read from an xml file. */

public class TestSuite extends FilterConsumer
{
  public static void main(String[] args)
  {
    gnu.xquery.lang.XQuery.registerEnvironment();
    for (int i = 0;  i < args.length;  i++)
      {
	try
	  {
	    Document.parse(args[i], new TestSuite());
	  }
	catch (Throwable ex)
	  {
	    System.err.println("caught "+ex+" while processing "+args[i]);
	  }
      }
    TestMisc.printSummary();
  }

  int nesting = 0;
  boolean inTestSuite = false;
  boolean inTest = false;
  String currentTag;

  StringWriter sout;
  StringBuffer sbuf;
  XMLPrinter xout;

  String query = null;
  String expect = null;

  public TestSuite()
  {
    this(new StringWriter());
    sbuf = sout.getBuffer();
  }

  private TestSuite(StringWriter sout)
  {
    this(sout, new XMLPrinter(sout));
    xout.escapeText = false;
  }

  private TestSuite(StringWriter sout, XMLPrinter xout)
  {
    super(xout);
    this.sout = sout;
    this.xout = xout;
  }

  public void beginGroup(String typeName, Object type)
  {
    if ("testsuite".equals(typeName) && nesting == 0)
      inTestSuite = true;
    else if ("test".equals(typeName)
	&& (nesting == 0 || (inTestSuite && nesting == 1)))
      inTest = true;
    else if (inTestSuite ? nesting == 2 : nesting == 1)
      {
	sbuf.setLength(0);
	currentTag = typeName;
      }
    else if (currentTag == null)
      throw new RuntimeException("saw <"+typeName+"> not in <test>");
    else
      base.beginGroup(typeName, type);
    nesting++;
  }

  public void endGroup(String typeName)
  {
    nesting--;
    if ("testsuite".equals(typeName) && nesting == 0)
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
  }
}
