package kawa.standard;

import kawa.lang.Syntax3;

public class ifp extends kawa.lang.Syntax3 {
   public kawa.standard.ifp() {
      super("if");
   }

   public Object execute3(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2,
      Object arg3
   ) throws kawa.lang.WrongType,
            kawa.lang.WrongArguments,
            kawa.lang.GenericError,
            kawa.lang.UnboundSymbol
   {
      java.lang.Boolean cond = (java.lang.Boolean)i.eval(arg1,frames);

      if (cond.booleanValue()) {
         return i.eval(arg2,frames);
      } else {
         return i.eval(arg3,frames);
      }
   }

}
