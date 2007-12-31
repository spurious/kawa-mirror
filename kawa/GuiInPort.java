package kawa;

import java.io.Reader;
import gnu.mapping.*;
import gnu.text.Path;

/** A TtyInPort that reads from a ReplPane.
  */

class GuiInPort extends TtyInPort
{
  ReplDocument document;

  public GuiInPort (Reader in, Path path, OutPort tie, ReplDocument document)
  {
    super (in, path, tie);
    this.document = document;
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
    if (! revisited && document.outputMark < document.endMark)
      {
	((ReplPane) document.pane).enter(); // FIXME
      }
  }

  public void emitPrompt (String prompt) throws java.io.IOException
  {
    document.write(prompt, ReplDocument.promptStyle);
  }
}
