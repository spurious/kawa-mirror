package kawa.lang;

// An undefined symbol was evaled.
public class UnboundSymbol extends Exception
{
  public Symbol symbol;

  public UnboundSymbol(Symbol symbol)
  {
    this.symbol = symbol;
  }
}
