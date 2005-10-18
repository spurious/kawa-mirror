package gnu.kawa.servlet;
import gnu.text.*;
import gnu.mapping.*;
import gnu.expr.*;

import java.io.*;

/** The server state for a browser-base "read-eval-print-loop" session. */

public class ReplSession
{
  Language language;
  Environment penvironment;
  QueueReader qreader;
  InPort in_p;
  OutPort err_p;
  OutPort out_p;
  OutBufferWriter prompt_p;
  Future thread;

  StringBuffer outBuffer = new StringBuffer();
  // 'O' if there is an enclosed <span std="output" ....
  // 'E' if there is an enclosed <span std="error" ....
  // 'P' if there is an enclosed <span std="prompt" ....
  // 'R' outputing raw unquoted html
  char outBufferState;
  boolean outAvailable;

  void appendOutput (char c, char kind)
  {
    synchronized (this)
      {
        appendOutputRaw(c, kind);
      }
  }

  void appendOutputRaw (char c, char kind)
  {
    if (outBufferState != '\0' && outBufferState != 'R'
        && outBufferState != kind)
      //|| c == '\n' || c == '\r'))
      {
        outBuffer.append("</span>");
        outBufferState = '\0';
      }
    if (kind == 'R')
      {
        outBuffer.append(c);
        outBufferState = kind;
      }
    else if (c == '\n' || c == '\r')
      {
        outBuffer.append("<br />");
      }
    else
      {
        if (outBufferState != kind)
          {
            if (kind == 'O')
              outBuffer.append("<span std=\"output\">");
            else if (kind == 'E')
              outBuffer.append("<span std=\"error\">");
            else if (kind == 'P')
              outBuffer.append("<span std=\"prompt\">");
            outBufferState = kind;
          }
        if (c == '&')
          outBuffer.append("&amp;");
        else if (c == '<')
          outBuffer.append("&lt;");
        else if (c == '>')
          outBuffer.append("&gt;");
        else
          outBuffer.append((char) c);
      }
  }

  public void appendOutputRaw (String str, int off, int len, char kind)
  {
    for (int i = 0;  i < len; i++)
      appendOutputRaw(str.charAt(off+i), kind);
  }

  public void appendOutputRaw (String str, char kind)
  {
    appendOutputRaw(str, 0, str.length(), kind);
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
    if (outBufferState != '\0' && outBufferState != 'R')
      {
        outBuffer.append("</span>");
        outBufferState = '\0';
      }
    String result = outBuffer.toString();
    outBuffer.setLength(0);
    outAvailable = false;
    return result;
  }

  public ReplSession ()
  {
    this(kawa.standard.Scheme.getInstance());
  }

  public ReplSession (Language language)
  {
    if (Language.getDefaultLanguage() == null)
      Language.setDefaults(language);
    penvironment = Environment.getCurrent();
    qreader = new QueueReader();

    out_p = new OutPort(new OutBufferWriter(this, 'O'), true, true, "<stdout>");
    err_p = new OutPort(new OutBufferWriter(this, 'E'), true, true, "<stderr>");
    prompt_p = new OutBufferWriter(this, 'P');
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
                        prompt_p.write(string);
                        session.appendOutputRaw("<input std='input' value='' onchange='enterLine(this);'/>", 'R');
                        tie.flush();
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

class OutBufferWriter extends Writer
{
  ReplSession session;
  char kind;

  public OutBufferWriter (ReplSession session, char kind)
  {
    this.session = session;
    this.kind = kind;
  }

  public void write (int c)
  {
    session.appendOutput((char) c, kind);
  }

  public void write (char[] cbuf, int off, int len)
  {
    for (int i = 0;  i < len; i++)
      session.appendOutput(cbuf[off+i], kind);
  }

  public void write (String str, int off, int len)
  {
    synchronized (session)
      {
        session.appendOutputRaw(str, off, len, kind);
      }
  }

  public void flush ()
  {
    // FIXME?
    synchronized (session)
      {
        if (session.outBuffer.length() > 0)
          session.outAvailable = true;
        session.notify();
      }
  }

  public void close ()
  {
    flush();
    // FIXME?
  }
}
