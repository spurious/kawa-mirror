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

  public void emitPrompt (String prompt) throws java.io.IOException
  {
    document.write(prompt, ReplDocument.promptStyle);
  }
}
