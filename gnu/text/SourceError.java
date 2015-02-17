package gnu.text;
import gnu.kawa.io.InPort;
import java.io.File;

/** Represents an error message from processing a "source" file.
 */

public class SourceError implements SourceLocator
// FIXME: If JAVA6, should implement: javax.tools.Diagnostic<Path>
{
    /** Used to chain to the "next" message. */
    public SourceError next;

    /** The seriousness of the error - one of 'i' (for informational),
     * 'w' (for warning), 'e' (for error), or 'f' (for fatal error). */
    public char severity;

    /** The name or URL of the file containing the error. */
    public String filename;

    /** If non-null, an error code, as might be specified by a standard. */
    public String code;

    /** The line number of the error, with 1 being the top line.
     * The value 0 means unknown or not applicable (such as the entire file). */
    /** The (1-origin) location of the error. */
    public int line;

    /** The column number of the error, with 1 being the left-most column.
     * The value 0 means unknown or not applicable (such as the entire line). */
    public int column;

    /** The actual error message.
     * This is post-localization and -formatting.
     * It can contain multiple lines, separated by '\n'.*/
    public String message;

    /** Provides optional stack trace.
     * Filled when --debug-error-prints-stack-trace or
     * --debug-warning-prints-stack-trace option is used.*/
    public Throwable fakeException;

    public SourceError(char severity, String filename, int line, int column, 
                       String message) {
        this.severity = severity;
        this.filename = filename;
        this.line = line;
        this.column = column;
        this.message = message;
    }

    public SourceError(char severity, SourceLocator location, String message) {
        this(severity, location.getFileName(), location.getLineNumber(),
             location.getColumnNumber(), message);
    }

    /** Create a new SourceError using the current line/column from
     * a <code>InPort</code>. */
    public SourceError(InPort port, char severity, String message) {
        this(severity, port.getName(),
             port.getLineNumber() + 1, port.getColumnNumber(),
             message);
        if (column >= 0)
            column++;
    }

    /** Convert the error to a String.
     * The String starts with filename, line and option column,
     * followed by the message.  Warning messages are indicated as such. */
    public String toString() {
        return toString(false);
    }

    /** Convert the error to a String.
     * The String starts with filename, line and option column,
     * followed by the message.  Warning messages are indicated as such. */
    public String toString(boolean stripDirectories) {
        StringBuilder buffer = new StringBuilder();
        appendTo(buffer, stripDirectories, null);
        return buffer.toString ();
    }
   
    public void appendTo(Appendable out, boolean stripDirectories,
                         String newLine) {
        try {
            String fname;
            if (filename == null)
                fname = "<unknown>";
            else {
                fname = filename;
                if (stripDirectories)
                    fname = new File(fname).getName();
            }
            out.append(fname);
            if (line > 0 || column > 0) {
                out.append(':');
                out.append(Integer.toString(line));
                if (column > 0) {
                    out.append(':');
                    out.append(Integer.toString(column));
                }
            }
            out.append(": ");
            if (severity == 'w')
                out.append("warning - ");
            else if (severity == 'i')
                out.append("note - ");
            out.append(message);
            if (code != null) {
                out.append(" [");
                out.append(code);
                out.append("]");
            }

            if (fakeException != null) {
                StackTraceElement[] stackTrace = fakeException.getStackTrace();
                for (int i = 0; i < stackTrace.length; i++) {
                    out.append(newLine != null ? newLine : "\n");
                    out.append("    ");
                    out.append(stackTrace[i].toString());
                }
            }
            if (newLine != null)
                out.append(newLine);
        } catch (java.io.IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public void print(Appendable out) {
        appendTo(out, false, null);
    }

    public void println(Appendable out, boolean stripDirectories) {
        appendTo(out, stripDirectories,
                 System.getProperty("line.separator", "\n"));
    }

    public int getLineNumber () { return line == 0 ? -1 : line; }
    public int getColumnNumber () { return column == 0 ? -1 : column; }
    public String getPublicId() { return null; }
    public String getSystemId() { return filename; }
    public String getFileName() { return filename; }
    public boolean isStableSourceLocation() { return true; }
}
