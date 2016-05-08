package gnu.kawa.io;

import java.io.*;
import java.util.List;
/* #ifdef with:jline3 */
// import java.util.ArrayList;
/* #endif */
import gnu.expr.CommandCompleter;
import gnu.expr.Compilation;
import gnu.expr.Language;
import gnu.text.SourceMessages;
/* #ifdef with:jline3 */
// import org.jline.reader.Candidate;
// import org.jline.reader.Completer;
// import org.jline.reader.EndOfFileException;
// import org.jline.reader.LineReader;
// import org.jline.reader.LineReaderBuilder;
// import org.jline.reader.ParsedLine;
// import org.jline.reader.UserInterruptException;
// import org.jline.terminal.Terminal;
// import org.jline.terminal.TerminalBuilder;
/* #else */
import jline.console.completer.Completer;
/* #endif */

/** A variation of TtyInPort that uses the JLine2 library for input editing. */

public class JLine2InPort extends TtyInPort implements Completer {
    /* #ifdef with:jline3 */
    // org.jline.reader.LineReader jlreader;
    // org.jline.terminal.Terminal terminal;
    // String prompt;
    /* #else */
    jline.console.ConsoleReader jlreader;
    /* #endif */
    String stringRest;
    /** Remaining available characters in stringRest. */
    private int charsRest;

    public JLine2InPort(InputStream in, Path name, OutPort tie)
        throws java.io.IOException {
        super(in, name, tie);
        /* #ifdef with:jline3 */
        // Terminal terminal = TerminalBuilder.terminal();
        // jlreader = LineReaderBuilder.builder()
        //     .terminal(terminal)
        //     .completer(this)
        //     .build();
        /* #else */
        jlreader = new jline.console.ConsoleReader();
        jlreader.addCompleter(this);
        /* #endif */
    }

    // public void complete(LineReader reader, final ParsedLine commandLine, final List<Candidate> candidates) {

    /* #ifdef with:jline3 */
    // public void complete(LineReader reader, final ParsedLine commandLine,
    //                      List<Candidate> candidates) {
    //     String buffer = commandLine.word().substring(0, commandLine.wordCursor());
    //     int cursor = buffer.length(); // ???
    //     List<CharSequence> scandidates = new ArrayList<CharSequence>();
    //     int buflen = buffer.length();
    //     char[] tbuf = new char[buflen + 1 + pos];
    //     System.arraycopy(this.buffer, 0, tbuf, 0, pos);
    //     buffer.getChars(0, cursor, tbuf, pos);
    //     tbuf[pos+cursor] = CommandCompleter.COMPLETE_REQUEST;
    //     buffer.getChars(cursor, buflen, tbuf, pos+cursor+1);
    //     CharArrayInPort cin = new CharArrayInPort(tbuf);
    //     int r = CommandCompleter.complete(cin, scandidates);
    //     for (CharSequence cstr : scandidates) {
    //         String str= cstr.toString();
    //         candidates.add(new Candidate(str, str,
    //                                      null, null, null, null, true));
    //     }
    // }
    /* #else */
    public int complete(String buffer, int cursor,
                        List<CharSequence> scandidates) {
        int buflen = buffer.length();
        char[] tbuf = new char[buflen + 1 + pos];
        System.arraycopy(this.buffer, 0, tbuf, 0, pos);
        buffer.getChars(0, cursor, tbuf, pos);
        tbuf[pos+cursor] = CommandCompleter.COMPLETE_REQUEST;
        buffer.getChars(cursor, buflen, tbuf, pos+cursor+1);
        CharArrayInPort cin = new CharArrayInPort(tbuf);
        int r = CommandCompleter.complete(cin, scandidates);
        return r >= 0 ? cursor - r : r;
    }
    /* #endif */

    @Override
    protected int fill(int len) throws java.io.IOException {
        String line;
        int count;
        if (charsRest > 0)
            line = stringRest;
        else {
            /* #ifdef with:jline3 */
            // try {
            //     line = jlreader.readLine(prompt, null, null, null);
            // } catch (UserInterruptException ex) {
            //     return -1;
            // } catch (EndOfFileException ex) {
            //     promptEmitted = false;  // Disable redundant newline.
            //     return -1;
            // }
            /* #else */
            line = jlreader.readLine();
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
        // this.prompt = prompt;
        /* #else */
        jlreader.setPrompt(prompt);
        /* #endif */
    }
}
