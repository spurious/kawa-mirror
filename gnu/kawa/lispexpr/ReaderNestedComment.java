// Copyright (c) 2016  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;

import gnu.kawa.io.InPort;
import gnu.mapping.Values;
import gnu.text.Lexer;
import gnu.text.SyntaxException;

public class ReaderNestedComment extends ReadTableEntry {
    char start1;
    char start2;
    char end1;
    char end2;

    static ReaderNestedComment lispInstance
        = new ReaderNestedComment('#', '|', '|', '#');
    public static ReaderNestedComment getLispInstance() { return lispInstance; }

    public ReaderNestedComment(char start1, char start2, char end1, char end2) {
        this.start1 = start1;
        this.start2 = start2;
        this.end1 = end1;
        this.end2 = end2;
    }

    public Object read(Lexer in, int ch, int count)
        throws java.io.IOException, SyntaxException {
        readNestedComment((LispReader) in);
        return Values.empty;
    }

    public void readNestedComment(LispReader reader)
            throws java.io.IOException, SyntaxException {
	InPort port = reader.getPort();
        char saveReadState = '\0';
	if (port instanceof InPort) {
	    saveReadState = ((InPort) port).readState;
	    ((InPort) port).readState = start1;
        }
	try {
	    reader.readNestedComment(start1, start2, end1, end2);
        } finally {
	    if (port instanceof InPort)
                ((InPort) port).readState = saveReadState;
        }
    }
    
}

