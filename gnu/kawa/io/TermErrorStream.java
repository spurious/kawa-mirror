package gnu.kawa.io;
// Copy of org.domterm.util.DomTermErrorStream. FIXME

import java.io.*;

/** The standard error stream, when running under DomTerm or an ANSI terminal.
 * This forwards to another stream (normally System.out),
 * but surrounds each write with special escape sequences.
 * These cause DomTerm to place the error output inside
 * a {@code <span std="err">} element, which by default
 * is colored red.
 */

public class TermErrorStream extends PrintStream {
    public static final byte[] DOMTERM_START_ERR_MARKER = {
        27 /* escape */,
        (byte) '[',
        (byte) '1',
        (byte) '2',
        (byte) 'u'
    };
    public static final byte[] DOMTERM_END_ERR_MARKER = {
        27 /* escape */,
        (byte) '[',
        (byte) '1',
        (byte) '1',
        (byte) 'u'
    };
    public static final byte[] ANSI_START_ERR_MARKER = {
        27 /* escape */,
        (byte) '[',
        (byte) '3',
        (byte) '1',
        (byte) 'm'
    };
    public static final byte[] ANSI_END_ERR_MARKER = {
        27 /* escape */,
        (byte) '[',
        (byte) '3',
        (byte) '9',
        (byte) 'm'
    };
    byte[] startErrMarker;
    byte[] endErrMarker;

    private PrintStream out;

    public TermErrorStream(PrintStream out, boolean ansi) {
        super(out, true);
        this.out = out;
        if (ansi) {
            startErrMarker = ANSI_START_ERR_MARKER;
            endErrMarker = ANSI_END_ERR_MARKER;
        } else {
            startErrMarker = DOMTERM_START_ERR_MARKER;
            endErrMarker = DOMTERM_END_ERR_MARKER;
        }
    }

    public static void setSystemErr(boolean ansi) {
        // KLUDGE because our DomTermErrorStream is a copy of the
        // one in domterm.jar (and currently in a different package),
        // so this handles either version.  FIXME.
        if (System.err.getClass().getName().indexOf("DomTermErrorStream") < 0)
        //if (! (System.err instanceof DomTermErrorStream))
            System.setErr(new TermErrorStream(System.out, ansi));
    }

    @Override
    public void write(int b) {
        synchronized (out) {
            out.write(startErrMarker, 0, startErrMarker.length);
            out.write(b);
            out.write(endErrMarker, 0, endErrMarker.length);
            if (b == '\n')
                out.flush();
        }
    }

    @Override
    public void write(byte buf[], int off, int len) {
        if (len > 0) {
            synchronized (out) {
                out.write(startErrMarker, 0, startErrMarker.length);
                out.write(buf, off, len);
                out.write(endErrMarker, 0, endErrMarker.length);
                out.flush();
            }
        }
    }
}
