package gnu.kawa.io;

import java.io.*;
import java.util.List;
/* #ifdef with:jline3 */
import java.util.ArrayList;
import java.util.List;
/* #endif */
import gnu.expr.CommandCompleter;
import gnu.expr.Compilation;
import gnu.expr.Language;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
/* #ifdef with:jline3 */
import org.jline.reader.Candidate;
import org.jline.reader.Completer;
import org.jline.reader.EndOfFileException;
import org.jline.reader.EOFError;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.ParsedLine;
import org.jline.reader.Parser;
import org.jline.reader.UserInterruptException;
import org.jline.reader.SyntaxError;
import org.jline.reader.impl.DefaultParser;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
/* #else */
// import jline.console.completer.Completer;
/* #endif */

/** A variation of TtyInPort that uses the JLine2 library for input editing. */

public class JLine2InPort extends TtyInPort
    implements Completer
               /* #ifdef with:jline3 */
               , Parser
               /* #endif */
{
    /* #ifdef with:jline3 */
    LineReader jlreader;
    org.jline.terminal.Terminal terminal;
    String prompt;
    /* #else */
    // jline.console.ConsoleReader jlreader;
    /* #endif */
    String stringRest;
    /** Remaining available characters in stringRest. */
    private int charsRest;
    Language language;

    public JLine2InPort(InputStream in, Path name, OutPort tie)
        throws java.io.IOException {
        super(in, name, tie);
        /* #ifdef with:jline3 */
        Terminal terminal = TerminalBuilder.terminal();
        jlreader = LineReaderBuilder.builder()
            .terminal(terminal)
            .completer(this)
            .parser(this)
            .build();
        language = Language.getDefaultLanguage();
        /* #else */
        // jlreader = new jline.console.ConsoleReader();
        // jlreader.addCompleter(this);
        /* #endif */
    }

    /* #ifdef with:jline3 */
    public ParsedLine parse(String line, int cursor) throws SyntaxError {
        CharArrayInPort cin = CharArrayInPort.make(line, "\n");
        cin.setLineNumber(this.getLineNumber());
        cin.setPath(this.getPath());
        try {
            SourceMessages messages = new SourceMessages();
            Lexer lexer = language.getLexer(cin, messages);
            lexer.setInteractive(true);
            Compilation comp =
                language.parse(lexer,
                               Language.PARSE_FOR_EVAL|Language.PARSE_INTERACTIVE_MODULE,
                               null);
            return new KawaParsedLine(this, comp, line, cursor);
        } catch (SyntaxException ex) {
            if (cin.eofSeen())
                throw new EOFError(-1, -1, "unexpected end-of-file", "");
            throw ex;
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
    public void complete(LineReader reader, final ParsedLine commandLine,
                         List<Candidate> candidates) {
        String buffer = commandLine.line();
        int cursor = commandLine.cursor();
        List<CharSequence> scandidates = new ArrayList<CharSequence>();
        int buflen = buffer.length();
        char[] tbuf = new char[buflen + 1 + pos];
        System.arraycopy(this.buffer, 0, tbuf, 0, pos);
        buffer.getChars(0, cursor, tbuf, pos);
        tbuf[pos+cursor] = CommandCompleter.COMPLETE_REQUEST;
        buffer.getChars(cursor, buflen, tbuf, pos+cursor+1);
        CharArrayInPort cin = new CharArrayInPort(tbuf);
        int r = CommandCompleter.complete(cin, scandidates);
        for (CharSequence cstr : scandidates) {
            String str = cstr.toString();
            candidates.add(new Candidate(str, str,
                                         null, null, null, null, true));
        }
    }
    /* #else */
    // public int complete(String buffer, int cursor,
    //                     List<CharSequence> scandidates) {
    //     int buflen = buffer.length();
    //     char[] tbuf = new char[buflen + 1 + pos];
    //     System.arraycopy(this.buffer, 0, tbuf, 0, pos);
    //     buffer.getChars(0, cursor, tbuf, pos);
    //     tbuf[pos+cursor] = CommandCompleter.COMPLETE_REQUEST;
    //     buffer.getChars(cursor, buflen, tbuf, pos+cursor+1);
    //     CharArrayInPort cin = new CharArrayInPort(tbuf);
    //     int r = CommandCompleter.complete(cin, scandidates);
    //     return r >= 0 ? cursor - r : r;
    // }
    /* #endif */

    @Override
    protected int fill(int len) throws java.io.IOException {
        String line;
        int count;
        if (charsRest > 0)
            line = stringRest;
        else {
            /* #ifdef with:jline3 */
            try {
                line = jlreader.readLine(prompt, null, null, null);
            } catch (UserInterruptException ex) {
                return -1;
            } catch (EndOfFileException ex) {
                promptEmitted = false;  // Disable redundant newline.
                return -1;
            }
            /* #else */
            // line = jlreader.readLine();
            /* #endif */
            if (line == null)
                return -1;
            charsRest = line.length();
        }
        int start = line.length()-charsRest;
        if (charsRest < len) {
            line.getChars(start, line.length(), buffer, pos);
            buffer[pos+charsRest] = '\n';
            count = charsRest + 1;
            charsRest = 0;
            stringRest = null;
        } else {
            line.getChars(start, start+len, buffer, pos);
            stringRest = line;
            charsRest -= len;
            count = len;
        }
        afterFill(count);
        return count;
    }

    @Override
    public void emitPrompt(String prompt) throws java.io.IOException {
        /* #ifdef with:jline3 */
        this.prompt = prompt;
        /* #else */
        // jlreader.setPrompt(prompt);
        /* #endif */
    }

    public static class KawaParsedLine implements ParsedLine {
        JLine2InPort inp;
        Compilation comp;
        String source;
        int cursor;

        public KawaParsedLine(JLine2InPort inp, Compilation comp, String source, int cursor) {
            this.inp = inp;
            this.comp = comp;
            this.source = source;
            this.cursor = cursor;
        }

        // This method is called using reflection
        public static Compilation parse(Language language, Lexer lexer)
            throws java.io.IOException {
	    int opts = Language.PARSE_FOR_EVAL|Language.PARSE_ONE_LINE|Language.PARSE_INTERACTIVE_MODULE;
            JLine2InPort inp = (JLine2InPort) lexer.getPort();
            if (inp.tie != null)
                inp.tie.freshLine();
            int line = inp.getLineNumber() + 1;
            Object p = null;
            char saveState = inp.getReadState();
            inp.readState = ' ';
            try {
                if (inp.prompter != null)
                    p = inp.prompter.apply1(inp);
            } catch (Throwable ex) {
            }
            String prompt = p == null ? "["+line+"] " : p.toString();
            prompt = inp.wrapPromptForAnsi(prompt);
            inp.prompt = prompt;
            LineReader jlreader = inp.jlreader;
            jlreader.setVariable(LineReader.LINE_OFFSET, line);
            String pattern2 = prompt2.get("");
            jlreader.setVariable(LineReader.SECONDARY_PROMPT_PATTERN,
                                 inp.wrapPromptForAnsi(pattern2));
            inp.readState = saveState;
            try {
                jlreader.readLine(inp.prompt, null, null, null);
                KawaParsedLine parsedLine = (KawaParsedLine) jlreader.getParsedLine();
                inp.setLineNumber(line - 1 + parsedLine.lineCount());
                return parsedLine.comp;
            } catch (org.jline.reader.EndOfFileException ex) {
                return null;
            }
            
        }
        public String word() {
            return null;
        }

        public int wordCursor() {
            return 0;
        }

        public int wordIndex() {
            return 0;
        }

        public List<String> words() {
            return null;
        }

        public String line() {
            return source;
        }

        public int lineCount() {
            int n = 1;
            for (int i = 0; (i = source.indexOf('\n', i) + 1) > 0; )
                n++;
            return n;
        }

        public int cursor() {
            return cursor;
        }
    }
}
