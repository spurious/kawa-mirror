package gnu.mapping;
import java.io.*;
import gnu.text.*;
import gnu.lists.ByteVector;

/** An InPort that wraps an InputStream using straight-through copying.
 * This is an optimization of using Latin-1 conversion.
 */

public class BinaryInPort extends InPort {
    /** Underlying InputStream. */
    private InputStream strm;

    private byte[] bbuffer;
    private int bpos;
    private int blimit;
    
    public BinaryInPort(InputStream strm, Path path) {
        super((Reader) null, path);
        this.strm = strm;
        // Use a fixed-size buffer.  This prevents really-long "lines"
	// from causing the buffer to grow to accomodate them.
	try
	  {
	    setBuffer(new char[2048]);
	  }
	catch (java.io.IOException ex) { /* ignored */ }
        bbuffer = new byte[8192];
    }

    @Override
    protected int fill (int len) throws java.io.IOException {
        if (! bfill())
            return -1;
        int n = 0;
        while (n < len && bpos < blimit) {
            buffer[pos+(n++)] = (char) ((bbuffer[bpos++]) & 0xFF);
        }
        return n;
    }

    /** Read some bytes into byte buffer bbuffer.  Return false on EOF. */
    private boolean bfill() throws IOException {
        while (bpos >= blimit) {
            int n = strm.read(bbuffer, 0, bbuffer.length);
            bpos = 0;
            if (n < 0) {
                blimit = 0;
                return false;
            }
            blimit = n;
        }
        return true;
    }

    public int readByte() throws IOException {
        if (pos < limit)
            return read();
        else if (bfill())
            return bbuffer[bpos++] & 0xFF;
        else
            return -1;
    }

    public int peekByte() throws IOException {
        if (pos < limit)
            return peek();
        else if (bfill())
            return bbuffer[bpos] & 0xFF;
        else
            return -1;
    }

    public int readBytes(byte[] buf, int offset, int count)
            throws IOException {
        int rcount = 0;
        while (pos < limit) {
            if (rcount == count)
                return rcount;
            int r = read();
            if (r < 0)
                return rcount > 0 ? rcount : -1;
            buf[offset+(rcount++)] = (byte) r;
        }
        if (rcount > 0)
            return rcount;
        if (! bfill())
            return -1;
        int n = blimit - bpos;
        if (n > count)
            n = count;
        System.arraycopy(bbuffer, bpos, buf, offset, n);
        bpos += n;
        return n;
    }

    public int readByteVector(ByteVector bvector, int offset, int count)
            throws IOException {
        int rcount = 0;
        while (pos < limit) {
            if (rcount == count)
                return rcount;
            int r = read();
            if (r < 0)
                return rcount > 0 ? rcount : -1;
            bvector.setByteAt(offset+(rcount++), (byte) r);
        }
        if (rcount > 0)
            return rcount;
        if (! bfill())
            return -1;
        int n = blimit - bpos;
        if (n > count)
            n = count;
        bvector.copyFrom(bbuffer, bpos, offset, n);
        bpos += n;
        return n;
    }

    @Override
    public void close() throws IOException {
        strm.close();
        bbuffer = null;
    }

    @Override
    protected boolean sourceReady() throws IOException {
        return bpos < blimit || strm.available() > 0;
    }
}
