package kawa.lang;
import java.io.*;

/** An interactive input-port.
    Supports prompting, auto-flush of tied output port, transcripts. */

public class TtyInPort extends InPort
{
  private OutPort tie;

  private Procedure prompter;

  /** Get the current prompter function. */

  public Procedure getPrompter () { return prompter; }

  /** Set the prompter function.
   * The arguemnt is called when a new line is read.
   * It is passed one argument (this input port), and should return
   * a string.  That string is printed as teh prompt string.  */

  public void setPrompter (Procedure prompter)
  {
    this.prompter = prompter;
  }

  public TtyInPort (InputStream in, String name, OutPort tie)
  {
    super(in, name);
    this.tie = tie;
  }

  public TtyInPort (Reader in, String name, OutPort tie)
  {
    super(in, name);
    this.tie = tie;
  }

  private boolean promptEmitted;

  public int readHook (char[] buffer, int off, int len) throws java.io.IOException
  {
    if (tie != null)
      tie.echo(buffer, off, len);
    return len;
  }

  public void lineStart (boolean revisited) throws java.io.IOException
  {
    if (! revisited && prompter != null)
      {
	try
	  {
	    Object prompt = prompter.apply1(this);
	    if (prompt != null)
	      {
		String string = prompt.toString();
		if (string != null && string.length() > 0)
		  {
		    tie.print(string);
		    tie.flush();
		    promptEmitted = true;
		  }
	      }
	  }
	catch (Exception ex)
	  { throw new java.io.IOException("Error when evaluating prompt:"
					  + ex); }
      }
  }

  public int read () throws IOException
  {
    if (tie != null)
      tie.flush();
    int ch = super.read();
    if (ch < 0)
      {
	if (promptEmitted & tie != null)
	  tie.println();
      }
    promptEmitted = false;
    return ch;
  }

  public int read (char cbuf[], int off, int len) throws IOException
  {
    if (tie != null)
      tie.flush();
    int count = super.read(cbuf, off, len);
    if (count < 0 && promptEmitted & tie != null)
      tie.println();
    promptEmitted = false;
    return count;
  }

}
