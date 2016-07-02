package gnu.kawa.io;
import java.io.*;
import gnu.mapping.Procedure;

/** An interactive input-port.
    Supports prompting, auto-flush of tied output port, transcripts. */

public class TtyInPort extends InPort
{
  protected OutPort tie;

  protected Procedure prompter;

    boolean inDomTerm;
    public void setInDomTerm(boolean v) { inDomTerm = v; }

    /** Callback when SIGINT or ctrl-C is typed. */
    public Runnable sigIntHandler;

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

    public String promptTemplate1() {
        String str = CheckConsole.prompt1.get("");
        if (inDomTerm && ! haveDomTermEscapes(str))
            str = "%{\033[14u%}"+str+"%{\033[15u%}";
        return str;
    }

    public String promptTemplate2() {
        String str = CheckConsole.prompt2.get("");
        if (inDomTerm && ! haveDomTermEscapes(str))
            str = "%{\033[24u%}"+str+"%{\033[18u\033[15u%}";
        return str;
    }

    /** Default prompt calculation expands the prompt1 or prompt2 template. */
    public String defaultPrompt() {
        if (readState == '\n')
            return "";
        int line = getLineNumber() + 1;
        if (readState == ' ') {
            String pattern = promptTemplate1();
            int[] width = new int[1];
            String str = expandPrompt(pattern, 0, line, "", width);
            prompt1Length = width[0];
            return str;
        } else {
            String pattern = promptTemplate2();
            String m = new String(new char[] { readState });
            return expandPrompt(pattern, prompt1Length, line, m, null);
        }
    }

    /** Expand a prompt1 or prompt2 template to yield an actual prompt. */
    public String expandPrompt(String pattern, int padToWidth, int line,
                               String message, int[] width) {
        StringBuilder sb = new StringBuilder();
        int plen = pattern.length();
        int cols = 0;
        int padChar = -1;
        int padPos = -1;
        int escapeStartCol = -1;
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
                    cols++;
                    break;
                case 'N':
                    int oldw = sb.length();
                    sb.append(line);
                    cols += sb.length() - oldw;
                    break;
                case 'M':
                    if (message != null) {
                        sb.append(message);
                        cols += message.length();
                    }
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
                case '{':
                    escapeStartCol = cols;
                    break;
                case '}':
                    cols = escapeStartCol;
                    escapeStartCol = -1;
                    break;
               default:
                    i--;
                }
            } else {
                cols += 1; // width of chi
                if (Character.isLowSurrogate(ch))
                    cols--;
                sb.append(ch);
            }
        }
        String str = sb.toString();
        if (padToWidth > cols) {
            int padCharCols = 1;
            int padCount = (padToWidth - cols) / padCharCols;
            cols += padCount;
            while (--padCount >= 0)
                sb.insert(padPos, (char) padChar); // FIXME if wide
            str = sb.toString();
        }
        if (width != null)
            width[0] = cols;
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
        return "\033[38;5;120m" + prompt + "\033[39m";
    }

    /** If we see ESC '[' N N 'u' we already have domterm escapes. */
    public static boolean haveDomTermEscapes(String prompt) {
        for (int i = prompt.length(); --i >= 4 ; ) {
            if (prompt.charAt(i) == 'u' && prompt.charAt(i-4) == '\033'
                && prompt.charAt(i-3) == '[')
                return true;
            }
        return false;
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
                          emitPrompt(string);
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
        if (CheckConsole.useJLine() >= 0) {
            try {
                return (TtyInPort)
                    Class.forName("gnu.kawa.io.JLineInPort")
                    .getConstructor(java.io.InputStream.class,
                                    gnu.kawa.io.Path.class,
                                    gnu.kawa.io.OutPort.class)
                    .newInstance(in, name, tie);
            } catch (Throwable ex) {
                //ex.printStackTrace();
            }
        }
        return new TtyInPort(in, name, tie);
    }
}
