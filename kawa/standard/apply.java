package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class apply extends kawa.lang.Procedure2 {
   public kawa.standard.apply() {
      super("apply");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType,
            kawa.lang.GenericError,
            kawa.lang.UnboundSymbol
   {
      return i.apply(arg1,arg2,frames);
   }

   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.standard.apply>");
   }
}
