// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swing;
import gnu.jemacs.buffer.*;

public class SwingToolkit extends EToolkit
{
  public EFrame newFrame(Buffer buffer)
  {
    return new SwingFrame(buffer);
  }

  public Buffer newBuffer (String name)
  {
    return new SwingBuffer(name);
  }
}
