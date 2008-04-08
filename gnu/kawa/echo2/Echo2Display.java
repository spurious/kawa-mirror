package gnu.kawa.echo2;
import gnu.kawa.models.*;
import gnu.kawa.servlet.*;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.ContentPane;
import java.awt.Dimension;
import java.awt.geom.Dimension2D;
import gnu.text.*;

public class Echo2Display extends Display
{
  static final Echo2Display instance = new Echo2Display();

  public static Display getInstance() { return instance; }

  public Window makeWindow ()
  {
    Echo2Window window = new Echo2Window();
    window.display = this;
    return window;
  }

  public static ContentPane asContentPane (Object value)
  {
    if (value instanceof ContentPane)
      return (ContentPane) value;
    ContentPane pane = new ContentPane();
    Echo2Display.addComponent(pane, value);
    return pane;
  }

  public static void
  addComponent (nextapp.echo2.app.Component container, Object value)
  {
    if (value instanceof Viewable)
      ((Viewable) value).makeView(Echo2Display.getInstance(), container);
    else
      container.add(asComponent(value));
  }

  public static Component asComponent (Object value)
  {
    if (value instanceof gnu.lists.FString || value instanceof String)
      return new nextapp.echo2.app.Label(value.toString());
    return (Component) value;
  }

  public void addButton (Button model, Object where)
  {
    addView(new Echo2Button(model), where);
  }

  public void addLabel (Label model, Object where)
  {
    addView(new Echo2Label(model), where);
  }

  public void addImage (DrawImage model, Object where)
  {
    try
      {
        addView(new nextapp.echo2.app.Label(new ResourceImageReference(model.getSrc())), where);
      }
    catch (Throwable ex)
      {
        throw gnu.mapping.WrappedException.wrapIfNeeded(ex);
      }
  }

  public void addText (Text model, Object where)
  {
    addView(new nextapp.echo2.app.TextField(new Echo2Document(model)), where);
  }

  public void addBox (Box model, Object where)
  {
    Component component;
    if (model.getAxis() == 0)
      component = new Echo2Row(model, this);
    else
      component = new Echo2Column(model, this);
    addView(component, where);
  }

  public void addView (Object view, Object where)
  {
    ((Component) where).add((Component) view);
  }
}

class Echo2Document extends nextapp.echo2.app.text.AbstractDocument
                    implements ModelListener
{
  public final Text model;

  public Echo2Document (Text model)
  {
    this.model = model;
    model.addListener(this);
  }

  public String getText() {
    return model.getText();
  }

  public void setText(String text) {
    model.setText(text);
  }

  public void modelUpdated (Model model, Object key)
  {
    if (key == "text" && model == this.model)
      fireDocumentUpdate(new nextapp.echo2.app.event.DocumentEvent(this));
  }
}

class Echo2Column
  extends nextapp.echo2.app.Column
  implements ModelListener //, nextapp.echo2.app.event.ActionListener
{
  Box model;

  public Echo2Column (Box model, Display display)
  {
    this.model = model;
    model.addListener(this);

    Viewable cellSpacing = model.getCellSpacing();
    Spacer spacer = null;
    if (cellSpacing instanceof Spacer)
      spacer = (Spacer) cellSpacing;

    if (spacer != null && spacer.isRigid())
      {
        Dimension2D dim = spacer.getMaximumSize2D();
        if (dim != null && dim.getWidth() == 0)
          {
            cellSpacing = null;
            setCellSpacing(new Extent((int) (dim.getHeight() + 0.5)));
          }
      }

    int n = model.getComponentCount();
    for (int i = 0;  i < n;  i++)
      {
        if (i > 0 && cellSpacing != null)
          cellSpacing.makeView(display, this);
        model.getComponent(i).makeView(display, this);
      }
  }

  public void modelUpdated (Model model, Object key)
  {
  }
}

class Echo2Row
  extends nextapp.echo2.app.Row
  implements ModelListener //, nextapp.echo2.app.event.ActionListener
{
  Box model;

  public Echo2Row (Box model, Display display)
  {
    this.model = model;

    model.addListener(this);

    int n = model.getComponentCount();
    for (int i = 0;  i < n;  i++)
      {
        model.getComponent(i).makeView(display, this);
      }
  }

  public void modelUpdated (Model model, Object key)
  {
  }
}

class ResourceImageReference extends nextapp.echo2.app.ResourceImageReference
{
  public ResourceImageReference (Path resource)
    throws java.io.IOException
  {
    super(resource.toString());
  }

  public void render(java.io.OutputStream out)
    throws java.io.IOException
  {
    String resource = getResource();
    javax.servlet.ServletContext context = ServletCallContext.getServletContext();
    java.io.InputStream in = null;
    byte[] buffer = new byte[4096];
    int bytesRead = 0;

    try {
      // The base class uses:
      //in = Thread.currentThread().getContextClassLoader().getResourceAsStream(resource);
      // That may be ok.  If so, we may not need this class. FIXME.
      in = new java.net.URL(resource).openStream();
      if (in == null) {
        throw new IllegalArgumentException("Specified resource does not exist: " + resource + ".");
      }
      do {
        bytesRead = in.read(buffer);
        if (bytesRead > 0) {
          out.write(buffer, 0, bytesRead);
        }
      } while (bytesRead > 0);
    } finally {
      if (in != null) { try { in.close(); } catch (java.io.IOException ex) { } }
    }
  }
}

class Echo2Label
  extends nextapp.echo2.app.Label
  implements ModelListener //, nextapp.echo2.app.event.ActionListener
{
  Label model;

  public Echo2Label (Label model)
  {
    this.model = model;
    String text = model.getText();
    if (text != null)
      super.setText(text);
    model.addListener(this);
  }

  public void setText(String text)
  {
    if (model == null)
      super.setText(text);
    else
      model.setText(text);
  }

  public void modelUpdated (Model model, Object key)
  {
    if (key == "text" && model == this.model)
      super.setText(this.model.getText());
    /*
    else if (key == "foreground" && model == this.model)
      {
        java.awt.Color fg1 = this.model.getForeground();
        nextapp.echo2.app.Color fg2 = toEcho2Color(fg1);
      super.setForeground(fg2);
      }
    else if (key == "background" && model == this.model)
      super.setBackground(toEcho2Color(this.model.getBackground()));
    */
  }
}

class Echo2Button
  extends nextapp.echo2.app.Button
  implements ModelListener, nextapp.echo2.app.event.ActionListener
{
  Button model;

  public Echo2Button (Button model)
  {
    super(model.getText());
    this.model = model;
    model.addListener(this);
    addActionListener(this);
    java.awt.Color fg = model.getForeground();
    if (fg != null)
      super.setForeground(toEcho2Color(fg));
    java.awt.Color bg = model.getBackground();
    if (bg != null)
      super.setBackground(toEcho2Color(bg));
  }

  public void setText(String text)
  {
    if (model == null)
      super.setText(text);
    else
      model.setText(text);
  }

  public void setForeground (nextapp.echo2.app.Color fg)
  {
    model.setForeground(toAWTColor(fg));
  }

  public void setBackground (nextapp.echo2.app.Color bg)
  {
    model.setBackground(toAWTColor(bg));
  }

  public void setForeground (java.awt.Color fg)
  {
    model.setForeground(fg);
  }

  public void setBackground (java.awt.Color bg)
  {
    model.setBackground(bg);
  }

  public static nextapp.echo2.app.Color toEcho2Color (java.awt.Color color)
  {
    return new nextapp.echo2.app.Color(color.getRGB() & 0xFFFFFF);
  }

  public static java.awt.Color toAWTColor (nextapp.echo2.app.Color color)
  {
    return new java.awt.Color(color.getRgb());
  }

  public void modelUpdated (Model model, Object key)
  {
    if (key == "text" && model == this.model)
      super.setText(this.model.getText());
    else if (key == "foreground" && model == this.model)
      {
        java.awt.Color fg1 = this.model.getForeground();
        nextapp.echo2.app.Color fg2 = toEcho2Color(fg1);
        super.setForeground(fg2);
      }
    else if (key == "background" && model == this.model)
      super.setBackground(toEcho2Color(this.model.getBackground()));
  }

  public void actionPerformed (nextapp.echo2.app.event.ActionEvent e)
  {
    try
      {
        Object action = model.getAction();
        if (action != null)
          ((gnu.mapping.Procedure) action).apply1(e);
      }
    catch (Throwable ex)
      {
	throw new gnu.mapping.WrappedException(ex);
      }
  }
}
