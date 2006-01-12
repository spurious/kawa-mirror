package gnu.kawa.swtviews;
import gnu.kawa.models.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;

public class SwtDisplay extends gnu.kawa.models.Display
{
  org.eclipse.swt.widgets.Display swtDisplay;

  static SwtDisplay instance;

  public SwtDisplay (org.eclipse.swt.widgets.Display swtDisplay)
  {
    this.swtDisplay = swtDisplay;
  }

  public static synchronized gnu.kawa.models.Display getInstance()
  {
    if (instance == null)
      {
        instance = new SwtDisplay(new org.eclipse.swt.widgets.Display());
      }
    return instance;
  }

  public Window makeWindow ()
  {
    SwtWindow window = new SwtWindow(this);
    window.display = this;
    return window;
  }

  public void addBox (Box model, Object where)
  {
    new SwtBox(model, (Composite) where, this);
  }

  public void addButton (gnu.kawa.models.Button model, Object where)
  {
    new SwtButton(model, (Composite) where, this);
  }

  public void addLabel (gnu.kawa.models.Label model, Object where)
  {
    //new SwtLabel(model, (Composite) where, this);
    org.eclipse.swt.widgets.Label swtLabel
      = new org.eclipse.swt.widgets.Label((Composite) where, 0);

    String text = model.getText();
    if (text != null)
      swtLabel.setText(text);

    LabelListener listener = new LabelListener(swtLabel, model);

    model.addListener(listener);
    swtLabel.pack();
  }

  public void addImage (DrawImage model, Object where)
  {
    throw new Error("SwtDisplay.addImage not implemented!");
  }

  public void addView (Object view, Object where)
  {
    if (! ((Control) view).setParent((Composite) where))
      throw new Error("SwtDisplay.addView not implemented!");
  }
}

class SwtButton
  implements // ModelListener,
  org.eclipse.swt.events.SelectionListener
{
  org.eclipse.swt.widgets.Button swtButton;

  gnu.kawa.models.Button model;

  public SwtButton (gnu.kawa.models.Button model,
                    Composite parent, SwtDisplay display)
  {
    this.model = model;

    swtButton = new org.eclipse.swt.widgets.Button(parent, SWT.PUSH);
    swtButton.setText(model.getText());
    swtButton.addSelectionListener(this);
    swtButton.pack();
  }

  public void widgetSelected(SelectionEvent e)
  {
    try
      {
        ((gnu.mapping.Procedure) model.getAction()).apply1(e);
      }
    catch (Throwable ex)
      {
	throw new gnu.mapping.WrappedException(ex);
      }
  }

  public void widgetDefaultSelected (SelectionEvent e)
  {
    System.err.println("(widgetDefaultSelected invoked)");
  }
}

/** A listener to update an SWT Label when the model has changed. */

class LabelListener extends WeakListener
{
  public LabelListener(org.eclipse.swt.widgets.Label swtLabel,
                       gnu.kawa.models.Label model)
  {
    super(swtLabel);
  }

  public void update (Object view, Model model, Object key)
  {
    org.eclipse.swt.widgets.Label swtLabel
      = (org.eclipse.swt.widgets.Label) view;
    gnu.kawa.models.Label label = (gnu.kawa.models.Label) model;
    if (key == "text")
      swtLabel.setText(label.getText());
    swtLabel.pack(true);
  }
}

class SwtBox extends Composite
{
  Box model;

  public SwtBox (Box model, Composite parent, gnu.kawa.models.Display display)
  {
    super(parent, 0);
    this.model = model;

    Viewable cellSpacing = model.getCellSpacing();
    int n = model.getComponentCount();
    for (int i = 0;  i < n;  i++)
      {
        if (i > 0 && cellSpacing != null)
          cellSpacing.makeView(display, this);
        model.getComponent(i).makeView(display, this);
      }

    GridLayout layout = new GridLayout();
    if (model.getAxis() == 0)
      { // Row
        layout.numColumns = n;
      }
    else
      { // Column
        layout.numColumns = 1;
      }
    layout.horizontalSpacing = layout.verticalSpacing = 0;
    layout.marginWidth = layout.marginHeight = 0;
    setLayout(layout);
    pack();
  }
}

class SwtWindow implements gnu.kawa.models.Window
{
  org.eclipse.swt.widgets.Shell shell;

  SwtDisplay display;

  public gnu.kawa.models.Display getDisplay () { return display; }

  public SwtWindow (SwtDisplay display)
  {
    this.display = display;
    shell = new Shell(display.swtDisplay, SWT.CLOSE|SWT.BORDER|SWT.RESIZE);
  }

  public String getTitle ()
  {
    return "(unknown title)";  // FIXME
  }

  public void setTitle (String title)
  {
    // FIXME
  }

  public void setMenuBar (Object menubar)
  {
    // FIXME
  }

  public void setContent (Object content)
  {
    if (content instanceof Viewable)
      ((Viewable) content).makeView(getDisplay(), shell);
    else
      new Error("SwtDisplay.setContent:"+ content).printStackTrace();
  }

  public void open ()
  {
    shell.open();
    while (!shell.isDisposed ())
      {
        if (!display.swtDisplay.readAndDispatch())
          display.swtDisplay.sleep();
      }
    display.swtDisplay.dispose();
  }

  public void finalize ()
  {
    if (shell != null)
      shell.dispose();
  }
}