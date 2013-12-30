package gnu.mapping;
import java.io.*;
import gnu.text.*;
import gnu.lists.ByteVector;

/** An InPort that wraps an InputStream using straight-through copying.
 * This is an optimization of using Latin-1 conversion.
 */

public class BinaryInPort extends InPort {

    private BinaryInputStream bstrm;
    
    public BinaryInPort(InputStream strm, Path path) {
        super((Reader) null, path);
        // Use a fixed-size buffer.  This prevents really-long "lines"
	// from causing the buffer to grow to accomodate them.
	try
	  {
	    setBuffer(new char[2048]);
	  }
	catch (java.io.IOException ex) { /* ignored */ }

        bstrm = new BinaryInputStream(strm);
    }

    public BinaryInPort(byte[] buffer, int length, Path path) {
        super((Reader) null, path);
        // Use a fixed-size buffer.  This prevents really-long "lines"
	// from causing the buffer to grow to accomodate them.
	try
	  {
	    setBuffer(new char[2048]);
	  }
	catch (java.io.IOException ex) { /* ignored */ }

        // It would be more element to use a ByteArrayInputStream,
        // but that doesn't support some extensions we need.
        bstrm = new BinaryInputStream(buffer, length);
    }

    public InputStream getInputStream() {
        return bstrm;
    }

    @Override
    protected int fill(int len) throws java.io.IOException {
        return bstrm.fillTo(buffer, pos, len);
    }

    public int readByte() throws IOException {
        return bstrm.read();
    }

    public int peekByte() throws IOException {
        return bstrm.peek();
    }

    public int readBytes(byte[] buf, int offset, int count)
            throws IOException {
        return bstrm.read(buf, offset, count);
    }

    @Override
    public void close() throws IOException {
        if (bstrm != null)
            bstrm.close();
        bstrm = null;
        super.close();
    }

    @Override
    protected boolean sourceReady() throws IOException {
        return bstrm.sourceReady();
    }

    public static BinaryInPort openFile(Object fname)
        throws IOException {
        return (BinaryInPort) InPort.openFile(fname, Boolean.FALSE);
    }

    static class BinaryInputStream extends BufferedInputStream {
        public BinaryInputStream(InputStream strm) {
            super(strm);
        }
        public BinaryInputStream(byte[] buffer, int length) {
            super(nullInputStream, 1);
            buf = buffer;
            count = length;
        }

        public static final InputStream nullInputStream
            = new ByteArrayInputStream(new byte[0]);

        public synchronized boolean sourceReady() throws IOException {
            return pos < count || in.available() > 0;
        }

        public synchronized int peek() throws IOException {
            int r = read();
            if (r >= 0)
                pos--;
            return r;
        }

        synchronized int fillTo(char[] cbuffer, int cstart, int len)
                throws IOException {
            int r = read();
            if (r < 0)
                return r;
            int p = pos-1;
            int n = count - p;
            if (n < len)
                n = len;
            for (int i = 0;  i < n;  i++) {
                cbuffer[pos+i] = (char) ((buf[p++]) & 0xFF);
            }
            pos = p;
            return n;
        }
    }
}
