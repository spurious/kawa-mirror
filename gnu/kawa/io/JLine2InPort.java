package gnu.kawa.io;

import java.io.*;
import java.util.List;
import gnu.expr.CommandCompleter;
import gnu.expr.Compilation;
import gnu.expr.Language;
import gnu.text.SourceMessages;
import jline.console.completer.Completer;

/** A variation of TtyInPort that uses the JLine2 library for input editing. */

public class JLine2InPort extends TtyInPort implements Completer {
    jline.console.ConsoleReader jlreader;
    String stringRest;
    /** Remaining available characters in stringRest. */
    private int charsRest;

    public JLine2InPort(InputStream in, Path name, OutPort tie)
        throws java.io.IOException {
        super(in, name, tie);
        jlreader = new jline.console.ConsoleReader();
        jlreader.addCompleter(this);
    }

    public int complete(String buffer, int cursor, List<CharSequence> candidates) {
        int buflen = buffer.length();
        char[] tbuf = new char[buflen + 1 + pos];
        System.arraycopy(this.buffer, 0, tbuf, 0, pos);
        buffer.getChars(0, cursor, tbuf, pos);
        tbuf[pos+cursor] = CommandCompleter.COMPLETE_REQUEST;
        buffer.getChars(cursor, buflen, tbuf, pos+cursor+1);
        CharArrayInPort cin = new CharArrayInPort(tbuf);
        int r = CommandCompleter.complete(cin, candidates);
        return r >= 0 ? cursor - r : r;
    }

    @Override
    protected int fill(int len) throws java.io.IOException {
        String line;
        int count;
        if (charsRest > 0)
            line = stringRest;
        else {
            line = jlreader.readLine();
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
        jlreader.setPrompt(prompt);
    }
}
