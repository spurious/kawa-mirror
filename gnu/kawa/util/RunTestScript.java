package gnu.kawa.util;
import gnu.expr.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;

/** A utility for simple one-file test cases.
 * The test case is a self-contained file contining the program/script to run,
 * with the expected out and/or diagnostic embedded as comments.
 * Here is a simple Scheme example:
 * <blockquote><pre>
 * (define xx (* 2 10))
 * (format #t "xx=~s~%" xx)
 * ;; Output: xx=20
 * </pre></blockquote>
 *
 * <h4>Directives</h4>
 * <p>The following assumes Scheme, with a line-comment {@code ";;"},
 * start-block-comment {@code "#|"} and end-block-comment {@code "|#"}.
 * <dl>
 * <dt><code>;;Output: </code><var>line</var></dt>
 * <dd>Expect <var>line</var> literally in the output stream.</dd>
 * <dt><code>;;Output-pattern: </code><var>pattern</var></dt>
 * <dd>Expect a line matching the regex <var>pattern</var> in the output.</dd>
 * <dt><code>#|Output:</code></dt>
 * <dd>The following lines, ending with a line containing just the
 * matching end-block-comment {@code |#}, are expected literally in the output stream.</dd>
 * <dt><code>;;Diagnostic: </code><var>line</var></dt>
 * <dd>Expect <var>line</var> literally in the error stream.</dd>
 * <dt><code>;;Diagnostic-pattern: </code><var>pattern</var></dt>
 * <dd>Expect a line matching the regex <var>pattern</var> in the error stream.</dd>
 * <dt><code>#|Diagnostic:</code></dt>
 * <dd>The following lines, ending with a line containing just the
 * matching end-block-comment {@code |#}, are expected literally in the error stream.</dd>
 * </dl>
 */

public class RunTestScript implements Runnable
{
  String filename;
  Language language;
  String lineComment;
  String startComment, endComment;
  Pattern outPattern;
  Pattern outRegexPattern;
  Pattern errPattern;
  Pattern errRegexPattern;
  Pattern outBlockStart;
  Pattern errBlockStart;

  boolean failed;

  List<String> expectedOut = new ArrayList<String>();
  List<String> expectedErr = new ArrayList<String>();

  String[] commentSyntaxTable = {
    "Scheme", ";;", "#|", "|#",
    "Q2", "#", null, null };

  public RunTestScript (String filename, Language language)
  {
    this.filename = filename;
    this.language = language;
    if (language != null)
      {
        String langname = language.getName();
        for (int i = 0; ; i += 4 )
          {
            if (i >= commentSyntaxTable.length)
              {
                language = null;
                break;
              }
            if (langname.equals(commentSyntaxTable[i]))
              {
                lineComment = commentSyntaxTable[i+1];
                startComment = commentSyntaxTable[i+2];
                endComment = commentSyntaxTable[i+3];
                break;
              }
          }
      }
    if (language == null)    
      error("unknown or unsupported language");
    if (lineComment != null)
      {
        outPattern = Pattern.compile(Pattern.quote(lineComment) + ".*Output: *(.*) *$");
        outRegexPattern = Pattern.compile(Pattern.quote(lineComment) + ".*Output-pattern: *(.*) *$");
        errPattern = Pattern.compile(Pattern.quote(lineComment) + ".*Diagnostic: *(.*) *$");
        errRegexPattern = Pattern.compile(Pattern.quote(lineComment) + ".*Diagnostic-pattern: *(.*) *$");
      }
    if (startComment != null)
      {
        outBlockStart = Pattern.compile(Pattern.quote(startComment) + ".*Output:.*$");
        errBlockStart = Pattern.compile(Pattern.quote(startComment) + ".*Diagnostic:.*$");
      }
  }

  public RunTestScript (String filename)
  {
    this(filename, Language.getInstanceFromFilenameExtension(filename));
  }

  public void run ()
  {
    try
      {
        BufferedReader file = new BufferedReader(new FileReader(filename));
        for (;;)
          {
            String line = file.readLine();
            if (line == null)
              break;
            Matcher matcher;
            if (lineComment != null)
              {
                matcher = outPattern.matcher(line);
                if (matcher.matches())
                  expectedOut.add(Pattern.quote(matcher.group(1)));
                matcher = outRegexPattern.matcher(line);
                if (matcher.matches())
                  expectedOut.add(matcher.group(1));
                matcher = errPattern.matcher(line);
                if (matcher.matches())
                  expectedErr.add(Pattern.quote(matcher.group(1)));
                matcher = errRegexPattern.matcher(line);
                if (matcher.matches())
                  expectedErr.add(matcher.group(1));
              }
            if (startComment != null)
              {
                matcher = outBlockStart.matcher(line);
                if (matcher.matches())
                  {
                    for (;;)
                      {
                        line = file.readLine();
                        if (line == null)
                          error("non-terminated output block comment");
                        if (line.trim().equals(endComment))
                          break;
                        expectedOut.add(Pattern.quote(line));
                      }
                  }
                matcher = errBlockStart.matcher(line);
                if (matcher.matches())
                  {
                    for (;;)
                      {
                        line = file.readLine();
                        if (line == null)
                          error("non-terminated disagnostic block comment");
                        if (line.trim().equals(endComment))
                          break;
                        expectedErr.add(Pattern.quote(line));
                      }
                  }
              }
          }
    
        ProcessBuilder kawa = new ProcessBuilder("java", "kawa.repl", "--diagnostic-strip-directories", filename);
        Process process = kawa.start();
        BufferedReader out = new BufferedReader(new InputStreamReader(process.getInputStream()));
        BufferedReader err = new BufferedReader(new InputStreamReader(process.getErrorStream()));
        process.waitFor();
        checkOutput(out, expectedOut, "output");
        checkOutput(err, expectedErr, "diagnostics");
        System.err.println("# "+getTestName()+" passes");
      }
    catch (Throwable ex)
      {
        System.err.println("caught "+ex);
        ex.printStackTrace();
        System.exit(-1);
      }
  }

  void checkOutput (BufferedReader out, List<String> expectedOut, String source)
    throws Throwable
  {
    int i = 0;
    while (! failed)
      {
        String line = out.readLine();
        if (line == null)
          {
            if (i < expectedOut.size())
              fail("expected more "+source+": "+expectedOut.get(i));
            break;
          }
        if (i >= expectedOut.size())
          fail("more "+source+" than expected: '"+line+"'");
        if (! Pattern.matches(expectedOut.get(i), line))
          fail(source+" line "+(i+1)+": expected: '"+expectedOut.get(i)
               +"' actual: '"+line+"'");
        i++;
      }
  }

  String getTestName ()
  {
    return new File(filename).getName();
  }

  void fail (String message)
  {
    System.err.println("FAIL "+getTestName()+": "+message);
    failed = true;
  }

  void error (String message)
  {
    System.err.println("ERROR "+getTestName()+": "+message);
    System.exit(-1);
  }

  public static void main (String[] args)
  {
    boolean failed = false;
    for (int i = 0;  i < args.length;  i++)
      {
        RunTestScript runner = new RunTestScript(args[i]);
        runner.run();
        failed = failed || runner.failed;
      }
    if (failed)
      System.exit(-1);
  }
}
