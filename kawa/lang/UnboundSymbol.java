package kawa.lang;

// An undefined symbol was evaled.
public class UnboundSymbol extends Exception
{
  public String symbol;

  public UnboundSymbol(String symbol)
  {
    super ("Unbound symbol " + symbol);
    this.symbol = symbol;
  }
}
