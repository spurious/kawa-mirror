package kawa.lang;

// An undefined symbol was evaled.
public class UnboundSymbol extends Exception {
   public java.lang.String symbol;

   public UnboundSymbol(java.lang.String s) {
      symbol = new java.lang.String(s);
   }
}
