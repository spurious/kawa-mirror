package gnu.jemacs.buffer;
import gnu.mapping.*;
import kawa.lang.*;
import kawa.standard.Scheme;
import javax.swing.text.*;

/** A Buffer for "read-eval-print"-style command processors. */

public class ReplBuffer extends Buffer
{
  Marker processMark;
  BufferWriter processWriter;
  kawa.lang.QueueReader in_r;
  InPort in;
  OutPort out;
  OutPort err;
  Future thread;

  public ReplBuffer(Interpreter interp, Environment environment)
  {
    super(interp.getName() + " interaction");

    StyleConstants.setBold(inputStyle, true);

    processMark = new Marker(pointMarker);
    processWriter = new BufferWriter(processMark, true);
    in_r = new QueueReader ();
    out = new OutPort(processWriter, 512, true);
    err = new OutPort(processWriter, 512, true);
    // GuiInPort proves better cut/paste support.
    in = new TtyInPort(in_r, "<stdin>", out);

    thread = new Future (new kawa.repl(interp),
			 environment, in, out, err);
    thread.start();
  }

  public void enter()
  {
    insert('\n', 1, null);
    int pos = getDot();
    int markPos = processMark.getOffset();
    Segment segment = new Segment();
    try
      {
        document.getText(markPos, pos - markPos, segment);
      }
    catch (BadLocationException ex)
      {
        throw new RuntimeException("internal error: "+ex);
      }
    in_r.append(segment.array, segment.offset, segment.count);
    processMark.setDot(pos);
  }

  public static ReplBuffer scheme()
  {
   return  new ReplBuffer(Scheme.getInstance(), Environment.getCurrent());
  }
}
