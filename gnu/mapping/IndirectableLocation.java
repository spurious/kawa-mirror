// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public abstract class IndirectableLocation extends NamedLocation
{
  /** If <code>value==DIRECT_ON_SET</code>, break indirection on a <code>set</code>. */
  protected static final Object DIRECT_ON_SET = new String("(direct-on-set)");

  /** If non-null, operations are forwarded to the base location. */
  protected Location base;

  /** If <code>base</code> is null, the current value stored in
   * this <code>Location</code>.
   * If <code>base</code> is non-null, then <code>value</code> is generally
   * ignored.  However, the special value <code>DIRECT_ON_SET</code> means that
   * writes change change <code>value</code> directly, instead of setting
   * the value of <code>base</code>.
   */
  protected Object value;

  public IndirectableLocation (Symbol symbol, Object property)
  {
    super(symbol, property);
  }

  public boolean isConstant ()
  {
    return base != null && base.isConstant();
  }

  public Location getBase ()
  {
    return base == null ? this : base.getBase();
  }

  public void setBase (Location base)
  { 
    Environment env = getEnvironment();
    if (env != null && next == null && this.base instanceof UnboundLocation)
      env.addLocation(name, property, this);
    this.base = base;
    this.value = null;
  }

  public void undefine ()
  {
    base = UnboundLocation.getInstance();
    value = UNBOUND;
  }

  public Environment getEnvironment ()
  {
    return (! entered() && base instanceof NamedLocation
	    ? ((NamedLocation) base).getEnvironment()
	    : super.getEnvironment());
  }

  public String toString() { return getClass().getName()+"[#:"+id
      +" name:"+name+(property==null?"":(" prop:"+property))
      +(base==null?"":(" base:"+base.id)) +"]"; }
}
