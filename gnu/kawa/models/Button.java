package gnu.kawa.models;

/** A model (data) for a clickable button. */

public class Button
implements Viewable
{
  boolean disabled;
  String label;
  Object action;

  public Object makeView (ViewContainer container)
  {
    return container.addButton(this);
  }

  public boolean isDisabled () { return disabled; }
  public void setDisabled (boolean disabled) { this.disabled = disabled; }

  public String getLabel () { return label; }
  public void setLabel (String label) { this.label = label; }

  public Object getAction () { return action; }
  public void setAction (Object action) { this.action = action; }
}
