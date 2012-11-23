package gnu.mapping;
import java.io.*;
import gnu.text.*;

/** An OutPort that wraps an OutputStream using straight-through copying.
 * This is an optimization of using Latin-1 conversion; any char greater
 * than '\xFF' is converted to '?', and others are copied straight through.
 * However, there is guaranteed no local buffering, so you can
 * switch back and forth with the underlying OutputStream without flushing.
 * The underlying OutputStream should therefore be buffered, for performance.
 */

public class BinaryOutPort extends OutPort {
    OutputStream strm;

    public OutputStream getOutputStream() { return strm; }

    public BinaryOutPort(OutputStream strm, Path path) {
        super(new OutputStreamWriterLatin1(strm), path);
        this.strm = strm;
    }

    public static BinaryOutPort openFile(Object fname)
        throws IOException {
        Path path = Path.valueOf(fname);
        java.io.OutputStream strm = path.openOutputStream();
        BinaryOutPort op =
            new BinaryOutPort(new BufferedOutputStream(strm), path);
        op.finalizeAction = CLOSE_ON_FINALIZE;
        return op;
    }

    public static OutputStream asOutputStream(Object obj) {
        if (obj instanceof BinaryOutPort)
            return ((BinaryOutPort) obj).getOutputStream();
        else
            return (OutputStream) obj;
    }

    /** Like an OutputStreamWriter, but optimized for Latin1.
     * This converter performs no buffering, so it is recommended
     * that the underlying OutputStream be buffered. */
    public static class OutputStreamWriterLatin1 extends Writer {
        OutputStream strm;

        public OutputStreamWriterLatin1(OutputStream strm) {
            this.strm = strm;
        }

        public void write(char[] cbuf, int off, int len) throws IOException {
            for (int i = 0;  i < len;  i++) {
                char ch = cbuf[off+i];
                strm.write(ch <= 255 ? (int) ch : '?');
            }
        }

        public void write(int ch) throws IOException {
            strm.write(ch <= 255 ? (int) ch : '?');
        }

        public void write(String str, int off, int len)throws IOException {
            for (int i = 0;  i < len;  i++) {
                char ch = str.charAt(off+i);
                strm.write(ch <= 255 ? (int) ch : '?');
            }
        }

        public void flush()  throws IOException {
            strm.flush();
        }

        public void close()  throws IOException {
            strm.close();
        }
    }
}
