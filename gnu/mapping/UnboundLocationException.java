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
    String msg = super.getMessage();
    if (msg != null)
      return msg;
    StringBuffer sbuf = new StringBuffer();
    Symbol name = location == null ? null : location.getKeySymbol();
    if (name != null)
      {
	sbuf.append("unbound location ");
	sbuf.append(name);
	Object property = location.getKeyProperty();
	if (property != null)
	  {
	    sbuf.append(" (property ");
	    sbuf.append(property);
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
