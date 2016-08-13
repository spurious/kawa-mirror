// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.kawa.io.OutPort;
import java.io.*;
import gnu.kawa.io.PrettyWriter;

/** A Consumer that extends a PrintWriter.  Useful for formatting. */

public class PrintConsumer extends PrintWriter
    implements Appendable, XConsumer
{
    protected boolean skipping;
    protected Consumer base;
    private static Writer dummyWriter = new ConsumerWriter(null);

    public PrintConsumer(Writer out) {
        this(out, false);
    }

    public PrintConsumer(Writer out, boolean autoFlush) {
        super(out==null ? dummyWriter : out, autoFlush);
        if (out == null) {
            this.lock = this;
            this.out = null;
        } else if (out instanceof Consumer)
            this.base = (Consumer) out;
    }

    public PrintConsumer(Consumer out, boolean autoFlush) {
        this(out instanceof Writer ? (Writer) out
             : new ConsumerWriter(out),
             autoFlush);
        this.base = out;
    }

    public PrintConsumer(OutputStream out, boolean autoFlush) {
        super(out, autoFlush);
    }

    public PrettyWriter getPrettyWriter() {
        PrintConsumer cur = this;
        for (;;) {
            if (cur instanceof PrettyWriter)
                return (PrettyWriter) cur;
            Writer next = cur.out;
            if (next instanceof PrintConsumer)
                cur = (PrintConsumer) next;
            else
                return null;
        }
    }

    protected void startNumber() {
        writeWordStart();
    }

    protected void endNumber() {
        writeWordEnd();
    }

  public PrintConsumer append (char c)
  {
    print(c);
    return this;
  }

  public PrintConsumer append (CharSequence csq)
  {
    if (csq == null)
      csq = "null";
    append(csq, 0, csq.length());
    return this;
  }

  public PrintConsumer append (CharSequence csq, int start, int end)
  {
      write(csq == null ? "null" : csq,
            start, end-start);
    return this;
  }

    public void write(CharSequence csq, int start, int length) {
        if (length == 0)
            csq = "";
        if (csq instanceof String)
            write((String) csq, start, length);
        else {
            synchronized (lock) {
                int end = start+length;
                for (int i = start; i < end;  i++)
                    write(csq.charAt(i));
            }
        }
    }

    public void freshLine() {
        if (base instanceof PrintConsumer)
            ((PrintConsumer) base).freshLine();
    }

    public void writeSpace(int kind) {
        write(' ');
        writeBreak(kind);
    }

    protected void writeBreak(int kind) {
        if (base instanceof PrintConsumer)
            ((PrintConsumer) base).writeBreak(kind);
    }

    public static void writeBreakFill(Consumer out) {
        if (out instanceof PrintConsumer)
             ((PrintConsumer) out).writeBreakFill();
    }

    public void writeBreakFill() {
        writeBreak(PrettyWriter.NEWLINE_FILL);
    }

    public static void writeSpaceFill(Consumer out) {
        if (out instanceof PrintConsumer)
             ((PrintConsumer) out).writeSpaceFill();
        else
            out.write(' ');
    }

    public void writeSpaceFill() {
        writeSpace(PrettyWriter.NEWLINE_FILL);
    }

    public void writeSpaceLinear() {
        writeSpace(PrettyWriter.NEWLINE_LINEAR);
    }

    /** Write a new-line iff the containing section cannot be printed
     * on one line.  Either all linear-style newlines in a logical
     * block becomes spaces (if it all fits in a line), or none
     * of them do. */
    public void writeBreakLinear() {
        writeBreak(PrettyWriter.NEWLINE_LINEAR);
    }

    public void setIndentation(int amount, boolean current) {
        if (base instanceof PrintConsumer)
            ((PrintConsumer) base).setIndentation(amount, current);
    }

    public static void startLogicalBlock(String prefix, boolean perLine,
                                         String suffix, Consumer out) {
        if (out instanceof PrintConsumer)
            ((PrintConsumer) out).startLogicalBlock(prefix, perLine, suffix);
        else
            out.write(prefix);
    }

    public void startLogicalBlock(String prefix, boolean perLine,
                                  String suffix) {
        if (base instanceof PrintConsumer)
            ((PrintConsumer) base).startLogicalBlock(prefix, perLine, suffix);
        else
            writeRaw(prefix);
    }
    public void startLogicalBlock(String prefix, String suffix,
                                  int indent) {
        if (base instanceof PrintConsumer)
            ((PrintConsumer) base).startLogicalBlock(prefix, suffix, indent);
        else
            writeRaw(prefix);
    }

    public static void endLogicalBlock(String suffix, Consumer out) {
        if (out instanceof PrintConsumer)
            ((PrintConsumer) out).endLogicalBlock(suffix);
        else
            out.write(suffix);
    }

    public void endLogicalBlock(String suffix) {
        if (base instanceof PrintConsumer)
            ((PrintConsumer) base).endLogicalBlock(suffix);
        else
            writeRaw(suffix);
    }

    protected void beforeContent() {
    }

    protected void beforeNode() {
    }

    public void writeWordStart() {
        if (out instanceof PrintConsumer)
            ((PrintConsumer) out).writeWordStart();
    }

    public void writeWordEnd() {
        if (out instanceof PrintConsumer)
            ((PrintConsumer) out).writeWordEnd();
    }

    protected void clearWordEnd() {
        if (out instanceof PrintConsumer)
            ((PrintConsumer) out).clearWordEnd();
    }

    public void writeBoolean(boolean v) {
        if (skipping)
            return;
        synchronized (lock) {
            writeWordStart();
            if (base != null)
                base.writeBoolean(v);
            else
                print(v);
            writeWordEnd();
        }
    }

    public void writeFloat(float v) {
        if (skipping)
            return;
        synchronized (lock) {
            startNumber();
            if (base != null)
                base.writeFloat(v);
            else
                print(v);
            endNumber();
        }
    }

    public void writeDouble(double v) {
        if (skipping)
            return;
        synchronized (lock) {
            startNumber();
            if (base != null)
                base.writeDouble(v);
            else
                print(v);
            endNumber();
        }
    }

    public void writeInt(int v) {
        if (skipping)
            return;
        synchronized (lock) {
            startNumber();
            if (base != null)
                base.writeInt(v);
            else
                print(v);
            endNumber();
        }
    }

    public void writeLong(long v) {
         if (skipping)
            return;
        synchronized (lock) {
            startNumber();
            if (base != null)
                base.writeLong(v);
            else
                print(v);
            endNumber();
        }
    }
    /*
    public void print(Object v) {
        if (out instanceof Consumer)
            ((Consumer) out).writeObject(v);
        else
            super.print(v);
    }
    */

    public void startDocument() {
        if (base != null && ! skipping)
            base.startDocument();
    }

    public void endDocument() {
        if (base != null && ! skipping)
            base.endDocument();
     }

    public void startElement(Object type) {
        if (base != null && ! skipping)
            base.startElement(type);
    }

    public void endElement() {
        if (base != null && ! skipping)
            base.endElement();
    }

    public void startAttribute (Object attrType) { }

  public void endAttribute() { }

    public void writeComment(char[] chars, int offset, int length) {
        if (skipping)
            return;
        beforeNode();
        if (base instanceof XConsumer)
            ((XConsumer) base).writeComment(chars, offset, length);
    }
    public void writeProcessingInstruction(String target, char[] content,
                                           int offset, int length) {
        if (skipping)
            return;
        beforeNode();
        if (base instanceof XConsumer)
            ((XConsumer) base)
                .writeProcessingInstruction(target, content, offset, length);
    }
    public void writeCDATA(char[] chars, int offset, int length) {
        beforeContent();
        if (skipping)
            return;
        if (base instanceof XConsumer)
          ((XConsumer) base).writeCDATA(chars, offset, length);
        else
            writeRaw(chars, offset, length);
    }
    public void beginEntity(Object baseUri) {
        if (skipping)
            return;
        beforeNode();
        if (base instanceof XConsumer)
            ((XConsumer) base).beginEntity(baseUri);
    }
    public void endEntity() {
        if (skipping)
            return;
        if (base instanceof XConsumer)
            ((XConsumer) base).endEntity();
    }

    protected void writeRaw(int v) {
        try {
            out.write(v);
        } catch (IOException ex) {
            setError();
        }
    }

    protected void writeRaw(String str) {
        try {
            out.write(str, 0, str.length());
        } catch (IOException ex) {
            setError();
        }
    }

    protected void writeRaw(String str, int start, int length) {
        try {
            out.write(str, start, length);
        } catch (IOException ex) {
            setError();
        }
    }

     protected void writeRaw(char[] chars, int start, int length) {
        try {
            out.write(chars, start, length);
        } catch (IOException ex) {
            setError();
        }
    }

    public void writeObject(Object v) {
        if (out instanceof Consumer)
            ((Consumer) out).writeObject(v);
        else
            print(v);
    }

  public boolean ignoring()
  {
    return false;
  }
}
