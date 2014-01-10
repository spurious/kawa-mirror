package gnu.kawa.io;
import java.io.*;
import gnu.text.*;
import gnu.lists.ByteVector;
import java.nio.*;
import java.nio.charset.*;

/** An InPort that wraps an InputStream.
 * Similar functionality as using an InputStreamReader, but provides hooks
 * to read at the byte level, possibly before setting the charset.
 * Optionally uses java.nio.charset directly, for extra flexibility
 * and a possible (but slight and unverified) performance improvement.
 */

public class BinaryInPort extends InPort {

    private BinaryInputStream bstrm;
    char[] carr;
    CharBuffer cbuf = null;

    Charset cset;
    CharsetDecoder decoder;

    public void setCharset(Charset cset) {
        this.cset = cset;
        this.decoder = cset.newDecoder();
    }

    public void setCharset(String name) {
        Charset cset = Charset.forName(name);
        if (this.cset == null)
            setCharset(cset);
        else if (! cset.equals(this.cset))
            throw new RuntimeException("encoding "+name+" does not match previous "+this.cset);
    }

    private BinaryInPort(BinaryInputStream bstrm, Path path) {
        super(bstrm, path);
        this.bstrm = bstrm;
        setKeepFullLines(false);
    }
    
    public BinaryInPort(InputStream strm) {
        this(new BinaryInputStream(strm), null);
    }

    public BinaryInPort(InputStream strm, Path path) {
        this(new BinaryInputStream(strm), path);
    }

    public BinaryInPort(byte[] buffer, int length, Path path) {
        this(new BinaryInputStream(buffer, length), path);
    }

    public InputStream getInputStream() {
        return bstrm;
    }

    public void resetStart(int pos) throws java.io.IOException {
        bstrm.bbuf.position(pos);
    }

    @Override
    protected int fill(int len) throws java.io.IOException {
        if (cset == null)
            setCharset("UTF-8");
        if (buffer != carr) {
            cbuf = CharBuffer.wrap(buffer);
            carr = buffer;
        }
        cbuf.limit(pos+len);
        cbuf.position(pos);
        boolean eof = false;
        int count;
        for (;;) {
            CoderResult cres = decoder.decode(bstrm.bbuf, cbuf, eof);
            count = cbuf.position() - pos;
            if (count > 0 || ! cres.isUnderflow())
                break;
            int rem = bstrm.bbuf.remaining();
            if (rem > 0) {
                bstrm.bbuf.compact();
            }
            int n = bstrm.fillBytes(rem);
            if (n < 0) {
                eof = true;
                break;
            }
        }
        return count == 0 && eof ? -1 : count;
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
        return bstrm.ready();
    }

    public static BinaryInPort openFile(Object fname)
        throws IOException {
        Path path = Path.valueOf(fname);
        BinaryInPort p = new BinaryInPort(path.openInputStream(), path);
        p.setCharset("ISO-8859-1");
        return p;
    }

    static class BinaryInputStream extends InputStream {
        InputStream base;
        byte[] barr = new byte[8192];
        ByteBuffer bbuf;

        public BinaryInputStream(InputStream base) {
            this.base = base;
            bbuf = ByteBuffer.wrap(barr);
            bbuf.position(barr.length);
        }

        public BinaryInputStream(byte[] buffer, int length) {
            bbuf = ByteBuffer.wrap(buffer, 0, length);
        }

        public synchronized int peek() throws IOException {
            int r = read();
            if (r >= 0)
                bbuf.position(bbuf.position()-1);
            return r;
        }

        public synchronized int read() throws java.io.IOException {
            if (! bbuf.hasRemaining()) {
                int n = fillBytes(0);
                if (n <= 0)
                    return -1;
            }
            return bbuf.get() & 0xFF;
        }

        public synchronized int read(byte[] buf, int offset, int count)
            throws IOException {
            int remaining = bbuf.remaining();
            if (remaining == 0) {
                int n = fillBytes(0);
                if (n <= 0)
                    return -1;
                remaining = bbuf.remaining();
            }
            if (count > remaining)
                count = remaining;
            bbuf.get(buf, offset, count);
            return count;
        }

        private synchronized int fillBytes(int remaining)
                throws java.io.IOException {
            if (base == null)
                return -1;
            int n = base.read(barr, remaining, barr.length-remaining);
            bbuf.position(0);
            bbuf.limit(remaining + (n < 0 ? 0 : n));
            return n;
        }

        public synchronized boolean ready() throws IOException {
            return bbuf.hasRemaining()
                || (base != null && base.available() > 0);
        }

        public synchronized int available() throws IOException {
            return bbuf.remaining() + (base == null ? 0 : base.available());
        }
    }
}
