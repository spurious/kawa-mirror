// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class UnboundLocation extends NamedLocation
{
  static final UnboundLocation instance = new UnboundLocation (null, null);
  public static UnboundLocation getInstance () { return instance; }

  Environment env;

  /* BEGIN JAVA2 */
  java.lang.ref.WeakReference ref;
  /* END JAVA2 */
  /* BEGIN JAVA1 */
  // NamedLocation loc;
  /* END JAVA1 */

  public UnboundLocation (Environment env, Symbol symbol, Object property)
  {
    super(symbol, property);
    this.env = env;
  }

  public UnboundLocation (Symbol symbol, Object property)
  {
    super(symbol, property);
  }

  void setLocation (NamedLocation loc, SimpleEnvironment env)
  {
    /* BEGIN JAVA2 */
    ref = new WeakLocation (loc, this, env.queue);
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // this.loc = loc;
    /* END JAVA1 */
  }

  protected NamedLocation getLocation ()
  {
    /* BEGIN JAVA2 */
    return (NamedLocation) ref.get();
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // return loc;
    /* END JAVA1 */
  }

  public boolean isBound ()
  {
    return false;
  }

  public final Object get (Object defaultValue)
  {
    return defaultValue;
  }

  public void set (Object newValue)
  {
    Environment env = getEnvironment();
    if ((env.flags & Environment.CAN_IMPLICITLY_DEFINE) == 0)
      throw new UnboundLocationException(name);
    env.define(name, property, newValue);
  }
}

/* BEGIN JAVA2 */
class WeakLocation extends java.lang.ref.WeakReference
{
  UnboundLocation uloc;

  public WeakLocation (NamedLocation loc, UnboundLocation uloc,
		       java.lang.ref.ReferenceQueue queue)
  {
    super(loc, queue);
    this.uloc = uloc;
  }
}
/* END JAVA2 */
