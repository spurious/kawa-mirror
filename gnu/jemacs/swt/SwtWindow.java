//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import gnu.jemacs.buffer.Buffer;
import gnu.jemacs.buffer.EKeymap;
import gnu.jemacs.buffer.EWindow;

/**
 * @author Christian Surlykke
 *         11-07-2004
 */
public class SwtWindow extends EWindow implements VerifyKeyListener, FocusListener, KeyListener, MouseListener
{
  private StyledText styledText;
  private SwtBuffer swtBuffer; 
  
  public SwtWindow(Buffer buffer) {
    this(buffer, true);
  }

  public SwtWindow(Buffer buffer, boolean wantModeLine) {
    super(buffer);
    this.swtBuffer = (SwtBuffer) buffer;
  }
  
  /**
   * 
   */
  public void getReadyToShow(Composite parent)
  {
    styledText = SwtHelper.newStyledText(parent, SWT.V_SCROLL | SWT.H_SCROLL, swtBuffer.getBufferContent(), this);
  }

  
  /**
   * @see gnu.jemacs.buffer.EWindow#setBuffer(gnu.jemacs.buffer.Buffer)
   */
  public void setBuffer(Buffer buffer)
  {
    super.setBuffer(buffer);
    styledText.setContent(((SwtBuffer) buffer).getBufferContent());
  }

  
  /**
   * The purpose of this method is to emulate the 'toInt' method of SwingWindow
   * so as to transform Swt KeyEvents into the same int's as equivalent awt KeyEvents.
   * 
   * TODO: Elaborate this method so that all KeyEvents work (e.g. enter (!))
   * 
   * I've been thinkin it perhaps would be better to make EKeymap abstract with implementors 
   * for each toolkit, and then lookup commands by Swt events directly when running
   * Swt and Swing events when running swing. Must be considered more... 
   * 
   *  
   * @param swtKeyCode
   * @param stateMask
   * @param additionalFlags
   * @return
   */
  private int transFormKeyKode(int swtKeyCode, int stateMask, int additionalFlags)
  {
    int characterPart = Character.toUpperCase((char) (swtKeyCode & 0xFFFF));
    int modifierPart = (stateMask & swtModifiers) >> 1; // awt modifiers seem to be displaced
                                                        // one bit to the left relative to 
                                                        // swt modifiers.
    return characterPart | modifierPart | (additionalFlags << 16);
  }

  private final static int swtModifiers = SWT.SHIFT | SWT.CTRL | SWT.ALT;

  public void handleKey (int code)
  {
    Object command = lookupKey(code);
    if (command == null )
    {
      return;
    }
    pushPrefix(code);
    pendingLength--;
    handleCommand (command);
  }


  
  public void handleCommand(Object command)
  {
    int oldDot = getBuffer().getDot();
    super.handleCommand(command);
    styledText.redraw();
    if (oldDot != getBuffer().getDot())
    {
      styledText.showSelection();
    }
  }
  
  /**
   * @see gnu.jemacs.buffer.EWindow#setSelected()
   */
  public void setSelected()
  {
    super.setSelected();
    buffer.pointMarker.sequence = null;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#unselect()
   */
  public void unselect()
  {
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getPoint()
   */
  public int getPoint()
  {
    return styledText.getCaretOffset();
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#setDot(int)
   */
  public void setDot(int offset)
  {
    styledText.setCaretOffset(offset);
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#split(gnu.jemacs.buffer.Buffer, int, boolean)
   */
  public EWindow split(Buffer buffer, int lines, boolean horizontal)
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getCharSize()
   */
  protected void getCharSize()
  {
    // TODO
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getWidth()
   */
  public int getWidth()
  {
    return styledText.getSize().x;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getHeight()
   */
  public int getHeight()
  {
    return styledText.getSize().y;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#tooLong(int)
   */
  public Object tooLong(int pendingLength)
  {
    // TODO Something more subtle here
    return null;
  }
  
  // ---------------------------- Listener methods ------------------------------------
  
  // --- VerifyKeyListener
  public void verifyKey(VerifyEvent event)
  {
    handleKey(transFormKeyKode(event.keyCode, event.stateMask, EKeymap.PRESSED));
    if (event.character != 0)
    {
      handleKey(event.character);
    }
    styledText.setCaretOffset(buffer.getDot());
    event.doit = false;
  }

  // --- FocusListener ---
  public void focusGained(FocusEvent e)
  {
    setSelected();
  }
  
  public void focusLost(FocusEvent e)
  {
    unselect();
  }
  
  // --- KeyListener ---
  public void keyPressed(KeyEvent e)
  {
  }
  
  public void keyReleased(KeyEvent e)
  {
    handleKey(transFormKeyKode(e.keyCode, e.stateMask, EKeymap.RELEASED));
  }
  
  // --- MouseListener ---
  public void mouseDoubleClick(MouseEvent e)
  {
  }

  public void mouseDown(MouseEvent e)
  {
    if (EWindow.getSelected() == this)  // Is this nessecary - aren't we always selected when this event arrives?
    {
      buffer.setDot(styledText.getCaretOffset());
      styledText.showSelection();
    }
  }

  public void mouseUp(MouseEvent e)
  {
  }


  
  
  /**
   * @param e
   */
  private void show(KeyEvent e)
  {
    System.out.println("keyCode:   " + Integer.toBinaryString(e.keyCode));
    System.out.println("character: " + Integer.toBinaryString(e.character));
    System.out.println("stateMask: " + Integer.toBinaryString(e.stateMask));
  }

}
