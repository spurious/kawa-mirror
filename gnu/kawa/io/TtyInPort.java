package gnu.kawa.io;
import java.io.*;
import gnu.mapping.Procedure;
import gnu.mapping.ThreadLocation;

/** An interactive input-port.
    Supports prompting, auto-flush of tied output port, transcripts. */

public class TtyInPort extends InPort
{
  protected OutPort tie;

  protected Procedure prompter;

    boolean inDomTerm;
    public void setInDomTerm(boolean v) { inDomTerm = v; }

  /** Get the current prompter function. */

  public Procedure getPrompter () { return prompter; }

  /** Set the prompter function.
   * The argument is called when a new line is read.
   * It is passed one argument (this input port), and should return
   * a string.  That string is printed as the prompt string.  */

  public void setPrompter (Procedure prompter)
  {
    this.prompter = prompter;
  }

    /** Saved length of the expanded primary prompt. */
    int prompt1Length = 0;

    /** Default prompt calculation expands the prompt1 or prompt2 template. */
    public String defaultPrompt() {
        if (readState == '\n')
            return "";
        int line = getLineNumber() + 1;
        if (readState == ' ') {
            String pattern = CheckConsole.prompt1.get("");
            String str = expandPrompt(pattern, 0, line, "");
            prompt1Length = str.length();
            return str;
        } else {
            String pattern = CheckConsole.prompt2.get("");
            String m = new String(new char[] { readState });
            String str = expandPrompt(pattern, prompt1Length, line, m);
            return str;
        }
    }

    /** Expand a prompt1 or prompt2 template to yield an actual prompt. */
    public static String expandPrompt(String pattern, int padToWidth, int line,
                               String message) {
        StringBuilder sb = new StringBuilder();
        int plen = pattern.length();
        int padChar = -1;
        int padPos = -1;
        for (int i = 0; i < plen; ) {
            char ch = pattern.charAt(i++);
            if (ch == '%' && i < plen) {
                ch = pattern.charAt(i++);
                int count = -1;
                while (ch >= '0' && ch <= '9') {
                    count = (count < 0 ? 0 : 10 * count)
                        + (ch - '0');
                    ch = pattern.charAt(i++);
                }
                switch (ch) {
                case '%':
                    sb.append(ch);
                    break;
                case 'N':
                    sb.append(line);
                    break;
                case 'M':
                    if (message != null)
                        sb.append(message);
                    break;
                case 'P':
                    if (count >= 0)
                        padToWidth = count;
                    if (i < plen) {
                        padChar = pattern.charAt(i++);
                        // FIXME check surrogate
                    }
                    padPos = sb.length();
                    break;
                default:
                    
                    i--;
                }
            } else
                sb.append(ch);
        }
       String  str = sb.toString();
        // More precise calculation when using JLine3 - this
        // doesn't handle ANSI escape, wide characters, or surrogate
        int cols = str.length();
        if (padToWidth > cols) {
            int padCharCols = 1;
            int padCount = (padToWidth - cols) / padCharCols;
            while (--padCount >= 0)
                sb.insert(padPos, (char) padChar); // FIXME if wide
            str = sb.toString();
        }
        return str;
   }

  public TtyInPort (InputStream in, Path name, OutPort tie)
  {
    super(in, name);
    setConvertCR(true);
    this.tie = tie;
  }

  public TtyInPort (Reader in, Path name, OutPort tie)
  {
    super(in, name);
    setConvertCR(true);
    this.tie = tie;
  }

  protected boolean promptEmitted;

  @Override
  protected int fill (int len) throws java.io.IOException
  {
    int count = in.read(buffer, pos, len);
    if (tie != null && count > 0)
      tie.echo(buffer, pos, count);
    return count;
  }

    protected void afterFill(int count) throws java.io.IOException {
        if (tie != null && count > 0)
            tie.echo(buffer, pos, count);
    }

    public void emitPrompt(String prompt) throws java.io.IOException {
        tie.print(prompt);
        tie.flush();
        tie.clearBuffer();
    }

    public String wrapPromptForAnsi(String prompt) {
        return "\033[48;5;120m" + prompt + "\033[0m";
    }
    public String wrapPromptForDomTerm(String prompt) {
        if (inDomTerm) {
            boolean haveDomTermEscapes = false;
            // If we see ESC '[' N N 'u' we already have domterm escapes.
            for (int i = prompt.length();
                 --i >= 4 && ! haveDomTermEscapes; ) {
                if (prompt.charAt(i) == 'u' && prompt.charAt(i-4) == '\033'
                    && prompt.charAt(i-3) == '[')
                    haveDomTermEscapes = true;
            }
            if (! haveDomTermEscapes)
                prompt = "\033[14u"+prompt+"\033[15u";
        }
        return prompt;
    }

  public void lineStart(boolean revisited) throws java.io.IOException {
      if (! revisited) {
          promptEmitted = false;
          if (prompter != null) {
              try {
                  Object prompt = readState == '\n' ? null
                      : readState == ' ' ? prompter.apply1(this)
                      : defaultPrompt();
                  if (prompt != null) {
                      String string = prompt.toString();
                      if (string != null && string.length() > 0) {
                          if (tie != null)
                              tie.freshLine();
                          emitPrompt(wrapPromptForDomTerm(string));
                          promptEmitted = true;
                      }
                  }
              } catch (Throwable ex) {
                  throw new java.io.IOException("Error when evaluating prompt:"
                                                + ex);
              }
          }
          if (tie != null && ! promptEmitted) {
              tie.flush();
              tie.clearBuffer();
          }
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
    public static TtyInPort make(InputStream in, Path name, OutPort tie) {
        try {
            return (TtyInPort)
                Class.forName("gnu.kawa.io.JLineInPort")
                .getConstructor(java.io.InputStream.class,
                                gnu.kawa.io.Path.class,
                                gnu.kawa.io.OutPort.class)
                .newInstance(in, name, tie);
        } catch (Throwable ex) {
        }
        return new TtyInPort(in, name, tie);
    }
}

