package gnu.xquery.testsuite;
import java.io.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.mapping.CharArrayOutPort;
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

  CharArrayOutPort cout;
  XMLPrinter xout;

  String query = null;
  String expect = null;

  private TestSuite()
  {
    this(new CharArrayOutPort());
  }

  private TestSuite(CharArrayOutPort cout)
  {
    this(cout, new XMLPrinter(cout));
    xout.escapeText = false;
  }

  private TestSuite(CharArrayOutPort cout, XMLPrinter xout)
  {
    super(xout);
    this.cout = cout;
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
        cout.setLength(0);
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
          {
            xout.flush();
            query = cout.toString();
          }
	else if ("expect".equals(typeName))
          {
            xout.flush();
            expect = cout.toString();
          }
	currentTag = null;
      }
    else
      base.endGroup(typeName);
  }
}
