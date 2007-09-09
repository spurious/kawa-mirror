package kawa;

import java.io.Reader;
import gnu.mapping.*;
import gnu.text.Path;

/** A TtyInPort that reads from a ReplPane.
  */

class GuiInPort extends TtyInPort
{
  ReplPane buffer;

  public GuiInPort (Reader in, Path path, OutPort tie, ReplPane buffer)
  {
    super (in, path, tie);
    this.buffer = buffer;
  }

  /** Overrides lineStart.
    * Needed to handle when a multi-line selection is pasted in.
    * We want the output (and prompt) to be "interpolated" in the right
    * places, so we fake an <Enter> when we're ready to read the
    * next line.  This sends the next line to the reader.
    */
  public void lineStart (boolean revisited) throws java.io.IOException
  {
    super.lineStart(revisited);
    if (! revisited && buffer.outputMark < buffer.endMark)
      {
	buffer.enter();
      }
  }

  public void emitPrompt (String prompt) throws java.io.IOException
  {
    buffer.write(prompt, ReplPane.promptStyle);
  }
}
