// Copyright (c) 2013  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.Lexer;
import gnu.text.SyntaxException;

public class ReaderConstituent extends ReadTableEntry {
    int kind;

    public ReaderConstituent(int kind) { this.kind = kind; }

    public int getKind() { return kind; }

    public Object read(Lexer in, int ch, int count, int sharingIndex)
	throws java.io.IOException, SyntaxException {
        LispReader reader = (LispReader) in;
        int startPos = reader.tokenBufferLength;
        ReadTable rtable = ReadTable.getCurrent();
        Object result = reader.readAndHandleToken(ch, startPos, rtable);
	return reader.bindSharedObject(sharingIndex, result);
    }
}
