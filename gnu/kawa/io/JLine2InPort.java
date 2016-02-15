package gnu.kawa.io;

import java.io.*;

/** A variation of TtyInPort that uses the JLine2 library for input editing. */

public class JLine2InPort extends TtyInPort {
    jline.console.ConsoleReader jlreader;
    String stringRest;
    /** Remaining available characters in stringRest. */
    private int charsRest;

    public JLine2InPort(InputStream in, Path name, OutPort tie)
        throws java.io.IOException {
        super(in, name, tie);
        jlreader = new jline.console.ConsoleReader();
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
