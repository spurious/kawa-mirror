package kawa.standard;

import kawa.lang.Syntax2;

public class define extends kawa.lang.Syntax2 {
   public kawa.standard.define() {
      super("define");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongType,
            kawa.lang.WrongArguments,
            kawa.lang.GenericError,
            kawa.lang.UnboundSymbol
   {
      kawa.lang.symbol sym = (kawa.lang.symbol)arg1;
      i.define(sym.name,i.eval(arg2,frames));
      return i.undefinedObject;

   }

}
