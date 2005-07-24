package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.expr.Language;
import java.io.*;

public class ReplMode extends ProcessMode
{
  BufferWriter processWriter;
  InPort in;
  OutPort out;
  OutPort err;
  Future thread;

  public ReplMode (Buffer buffer, Language language, Environment environment)
    throws java.io.IOException
  {
    lineMode = true;
    this.buffer = buffer;
    processMark = new Marker(buffer.pointMarker);
    processWriter = new BufferWriter(processMark, true);
    out = new OutPort(processWriter, true, true);
    err = new OutPort(processWriter, true, true);
    PipedReader preader = new PipedReader();
    toInferior = new PipedWriter(preader);
    in = new TtyInPort(preader, "<stdin>", out);
    thread = new Future (new kawa.repl(language),
			 environment, in, out, err);
    thread.setPriority(Thread.currentThread().getPriority() + 1);
    thread.start();
  }

  public static void make (Buffer buffer, String language)
    throws java.io.IOException
  {
    make(buffer, Language.getInstance(language));
  }

  public static void make (Buffer buffer, Language language)
    throws java.io.IOException
  {
    buffer.modes = new ReplMode (buffer, language, Environment.getGlobal());
  }


}
