package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.expr.Interpreter;
import kawa.lang.*;
import javax.swing.text.*;
import gnu.text.QueueReader;

/** A Buffer for "read-eval-print"-style command processors. */

public class ReplBuffer extends Buffer
{
  Marker processMark;
  BufferWriter processWriter;
  QueueReader in_r;
  InPort in;
  OutPort out;
  OutPort err;
  Future thread;

  public ReplBuffer(Interpreter interp, Environment environment)
  {
    super(interp.getName() + " interaction");

    inputStyle = redStyle;

    processMark = new Marker(this, 0, false);
    repl = this;
    processWriter = new BufferWriter(processMark, true);
    in_r = new QueueReader ();
    out = new OutPort(processWriter, true, true);
    err = new OutPort(processWriter, true, true);
    // GuiInPort proves better cut/paste support.
    in = new TtyInPort(in_r, "<stdin>", out);

    thread = new Future (new kawa.repl(interp),
			 environment, in, out, err);
    thread.start();
  }

  public void enter()
  {
    // FIXME goto end of line.
    insert('\n', 1, null);
    int pos = getDot();
    int markPos = processMark.getOffset();
    Segment segment = new Segment();
    try
      {
        getText(markPos, pos - markPos, segment);
      }
    catch (BadLocationException ex)
      {
        throw new RuntimeException("internal error: "+ex);
      }
    processMark.setDot(pos);
    in_r.append(segment.array, segment.offset, segment.count);
  }

  public static ReplBuffer make(String language)
  {
   return make(Interpreter.getInstance(language));
  }

  public static ReplBuffer make(Interpreter interpreter)
  {
   return new ReplBuffer(interpreter, Environment.getCurrent());
  }
}
