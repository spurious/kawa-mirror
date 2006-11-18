package gnu.kawa.servlet;
import gnu.text.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.PrintConsumer;
import java.io.*;
import gnu.xml.*;

/** The server state for a browser-based "read-eval-print-loop" session. */

public class ReplSession extends Writer
{
  Language language;
  Environment penvironment;
  QueueReader qreader;
  InPort in_p;
  OutBufferWriter err_p;
  OutBufferWriter out_p;
  OutBufferWriter prompt_p;
  Future thread;

  StringBuffer outBuffer = new StringBuffer();
  boolean outAvailable;

  void append1 (char c)
  {
    if (c == '\r' || c == '\n')
      outBuffer.append("<br/>");
    else
      outBuffer.append(c);
  }

  public void write (int c)
  {
    synchronized (this)
      {
        append1((char) c);
      }
  }

  public void write (char[] cbuf, int off, int len)
  {
    synchronized (this)
      {
        for (int i = 0;  i < len; i++)
          append1(cbuf[off+i]);
      }
  }

  public void write (String str, int off, int len)
  {
    synchronized (this)
      {
        for (int i = 0;  i < len; i++)
          append1(str.charAt(off+i));
      }
  }

  public void flush ()
  {
    synchronized (this)
      {
        if (outBuffer.length() > 0)
          outAvailable = true;
        notify();
      }
  }

  public void close ()
  {
    flush();
  }

  String grabOutput ()
  {
    synchronized (this)
      {
        return grabOutputRaw();
      }
  }

  String waitOutput ()
  {
    synchronized (this)
      {
        if (! outAvailable)
          {
            try
              {
                wait(30000);
              }
            catch (Throwable ex)
              {
                ex.printStackTrace();
              }
          }
        String out = grabOutputRaw();
        return out;
      }
  }

  String grabOutputRaw ()
  {
    String result = outBuffer.toString();
    outBuffer.setLength(0);
    outAvailable = false;
    //System.err.println("grabOutRaw ["+result+"]");
    return result;
  }

  public ReplSession ()
  {
    this(kawa.standard.Scheme.getInstance());
    //this(gnu.xquery.lang.XQuery.getInstance());
  }

  public ReplSession (Language language)
  {
    if (Language.getDefaultLanguage() == null)
      Language.setDefaults(language);
    penvironment = Environment.getCurrent();
    qreader = new QueueReader();

    out_p = new OutBufferWriter(this, 'O', "<stdout>");
    err_p = new OutBufferWriter(this, 'E', "<stderr>");
    prompt_p = new OutBufferWriter(this, 'P', "<prompt>");
    in_p = new MyTtyInPort(qreader, "<stdin>", out_p, this);

    thread = new Future (new kawa.repl(language),
                         penvironment, in_p, out_p, err_p);
    thread.start();
  }

  void appendInputLine (String line)
  {
    qreader.append(line);
    qreader.append('\n');
  }

  void appendInput (String line)
  {
    qreader.append(line);
  }
}

class MyTtyInPort extends TtyInPort
{
  ReplSession session;
  OutBufferWriter prompt_p;
  public MyTtyInPort (Reader in, String name, OutPort tie,
                      ReplSession session)
  {
    super(in, name, tie);
    this.session = session;
    this.prompt_p = session.prompt_p;
  }

  int pcount;

  public void lineStart (boolean revisited) throws java.io.IOException
  {
    if (! revisited && prompter != null)
      {
	try
	  {
	    tie.freshLine();
	    Object prompt = prompter.apply1(this);
	    if (prompt != null)
	      {
		String string = prompt.toString();
		if (string != null && string.length() > 0)
		  {
                    synchronized (session)
                      {
                        session.out_p.flushToSessionBuffer();
                        session.outBuffer.append("<div class=\"interaction\"><span std=\"prompt\">");
                        prompt_p.write(string);
                        prompt_p.flushToSessionBuffer();
                        session.outBuffer.append("</span><input std='input' value='' onchange='enterLine(this);'/></div>");
                        session.flush();
                      }
		    tie.clearBuffer();
		    promptEmitted = true;
		  }
	      }
	  }
	catch (Throwable ex)
	  { throw new java.io.IOException("Error when evaluating prompt:"
					  + ex); }
      }
  }
}

class OutBufferWriter extends OutPort
{
  ReplSession session;
  /** Which port this is:
   * 'O': output
   * 'E': error
   * 'p': prompt
   */
  char kind;
  int nesting = 0;
  XMLPrinter xout;

  public OutBufferWriter (ReplSession session, char kind, String name)
  {
    super(session, false, true, name);
    this.session = session;
    this.kind = kind;
    xout = new XMLPrinter(bout, true);
    out = xout;
  }

  public void write (int c)
  {
    if (c > 0x10000)
      Char.print(c, xout);
    else
      xout.append((char) c);
  }

  public void write (String str, int off, int len)
  {
    for (int i = 0;  i < len; i++)
      xout.append(str.charAt(off+i));
  }

  public void write (String str)
  {
    int len = str.length();
    for (int i = 0;  i < len; i++)
      xout.append(str.charAt(i));
  }

  public void beginGroup (Object type)
  {
    nesting++;
    xout.beginGroup(type);
  }

  public void endGroup ()
  {
    nesting--;
    xout.endGroup();
  }

  public void beginAttribute (Object attrType)
  {
    xout.beginAttribute(attrType);
  }

  public void endAttribute()
  {
    xout.endAttribute();
  }

  public void println ()
  {
    bout.write('\n');
    flush();
  }

  final void flushToSessionBuffer ()  throws java.io.IOException
  {
    bout.forcePrettyOutput();
  }

  public void flush ()
  {
    if (nesting > 0)
      return;
    synchronized (session)
      {
        if (kind == 'E')
          session.outBuffer.append("<span std=\"error\">");
        try
          {
            flushToSessionBuffer();
          }
        catch (IOException ex)
          {
            throw new RuntimeException(ex.toString());
          }
        if (kind == 'E')
          session.outBuffer.append("</span>");
        session.flush();
      }
  }
}
