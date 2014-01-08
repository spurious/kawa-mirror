package kawa.standard;

import gnu.kawa.io.InPort;
import gnu.lists.*;
import gnu.mapping.Procedure0or1;
import gnu.mapping.WrongType;
import gnu.text.Char;
import gnu.kawa.io.InPort;
import java.io.Reader;
import java.io.InputStream;

public class readchar extends Procedure0or1
{
    public static final readchar readChar = new readchar(false);
    public static final readchar peekChar = new readchar(true);

    boolean peeking;

    public readchar(boolean peeking) {
        super(peeking ? "peek-char" : "read-char");
        this.peeking = peeking;
    }

    final Object readChar(Reader port) {
        try {
            int ch;
            if (peeking)
                ch = InPort.peekCodePoint(port);
            else
                ch = InPort.readCodePoint(port);
            return ch < 0 ? Sequence.eofValue : Char.make(ch);
        } catch (java.io.IOException e) {
            throw new RuntimeException ("IO Exception caught");
        }
    }

    public static int readByte(InputStream port, boolean peeking) {
        try {
            int ch;
            if (peeking) {
                port.mark(1);
                ch = port.read();
                port.reset();
            } else
                ch = port.read();
            return ch;
        } catch (java.io.IOException e) {
            throw new RuntimeException(e);
        }
    }

    final Object readChar(InputStream port) {
        int ch = readByte(port, peeking);
        if (ch < 0)
            return Sequence.eofValue;
	return Char.make(ch);
    }

    public final Object apply0() {
        return readChar (InPort.inDefault());
    }

    public final Object apply1(Object arg1) {
        if (arg1 instanceof Reader)
            return readChar((Reader) arg1);
        if (arg1 instanceof InputStream)
            return readChar ((InputStream) arg1);
        throw new WrongType (this, 1, arg1, "input-port");
    }
}
