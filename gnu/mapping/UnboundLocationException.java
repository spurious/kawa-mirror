package gnu.mapping;

/** An undefined symbol was evaluated. */

public class UnboundLocationException extends RuntimeException
{
  public Object symbol;
  Location location;

  public UnboundLocationException ()
  {
  }

  public UnboundLocationException (Object symbol)
  {
    super ("Unbound symbol " + symbol);
    this.symbol = symbol;
  }

  public UnboundLocationException (Location loc)
  {
    this.location = loc;
  }

  public UnboundLocationException (Object symbol, String message)
  {
    super (message);
    this.symbol = symbol;
  }
  
  public String getMessage()
  {
    StringBuffer sbuf = new StringBuffer();
    if (location instanceof NamedLocation)
      {
	NamedLocation nloc = (NamedLocation) location;
	sbuf.append("unbound location ");
	sbuf.append(nloc.name);
	if (nloc.name != null)
	  sbuf.append("@"+Integer.toHexString(System.identityHashCode(nloc.name))+" env:"+nloc.getEnvironment()+" loc:"+nloc);
	if (nloc.property != null)
	  {
	    sbuf.append(" (property ");
	    sbuf.append(nloc.property);
	    sbuf.append(')');
	  }
      }
    else if (symbol != null)
      {
	sbuf.append("unbound location ");
	sbuf.append(symbol);
      }
    else
      sbuf.append("unbound location");
    return sbuf.toString();
  }
}
