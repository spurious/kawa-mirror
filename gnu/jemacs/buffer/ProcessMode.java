package gnu.jemacs.buffer;
import javax.swing.text.*;
import gnu.mapping.*;
import java.io.*;

public class ProcessMode extends Mode
{
  protected Writer toInferior;

  protected Marker processMark;

  protected boolean lineMode = false;

  static Procedure enterAction = new gnu.expr.PrimProcedure(gnu.bytecode.ClassType.make("gnu.jemacs.buffer.ProcessMode").getDeclaredMethod("enterAction", 0));

  public static Keymap modeMap = BufferKeymap.makeEmptyKeymap("process");
  static
  {
    BufferKeymap.defineKey(modeMap, "\n", enterAction);
    modeMap.setDefaultAction(new ProcessDefaultAction());
  }

  public static void enterAction()
  {
    Buffer buffer = Buffer.getCurrent();
    ProcessMode pmode = getProcessMode(buffer);
    pmode.enter();
  }

  public void enter()
  {
    try
      {
	if (lineMode)
	  {
	    buffer.insert('\n', 1, null);
	    int pos = buffer.getDot();
	    int markPos = processMark.getOffset();
	    Segment segment = new Segment();
	    buffer.document.getText(markPos, pos - markPos, segment);
	    processMark.setDot(pos);
	    System.err.println("sent to inf: (pos:"+pos+" pmark:"+markPos+") "+segment.count+" \""+new String(segment.array, segment.offset, segment.count)+"\"");
	    toInferior.write(segment.array, segment.offset, segment.count);
	  }
	else
	  toInferior.write('\r');
	toInferior.flush();
      }
    catch (Exception ex)
      {
        throw new WrappedException(ex);
      }
  }

  public static ProcessMode getProcessMode(Buffer buffer)
  {
    for (Mode mode = buffer.modes;  ;  mode = mode.next)
      {
	if (mode == null)
	  Signal.error("not in process mode");
	if (mode instanceof ProcessMode)
	  return (ProcessMode) mode;
      }
  }
}



class ProcessDefaultAction extends javax.swing.text.TextAction
{
  public ProcessDefaultAction()
  {
    super(null);
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    insert(event);
  }

  public static void insert(java.awt.event.ActionEvent event)
  {
    int count = 1;  // Get C-u prefix.  FIXME.
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
    String command = event.getActionCommand();
    char ch = command.charAt(0);
    ProcessMode pmode = ProcessMode.getProcessMode(buffer);
    if (! pmode.lineMode)
      {
	try
	  {
	    pmode.toInferior.write(ch);
	    pmode.toInferior.flush();
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
      }
    else
      buffer.insert(ch, count, buffer.inputStyle);
  }
}
