package kawa.standard;

import kawa.lang.Syntaxable;

public class when extends kawa.lang.Named implements kawa.lang.Syntaxable {
   public kawa.standard.when() {
      super("when");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object formo)
      throws kawa.lang.WrongType,
             kawa.lang.WrongArguments,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (formo instanceof kawa.lang.pair) {
         kawa.lang.pair pair = (kawa.lang.pair)formo;
         java.lang.Boolean cond = (java.lang.Boolean)i.eval(pair.car,frames);

         Object result = i.undefinedObject;
         if (cond.booleanValue()) {
            while (pair.cdr instanceof kawa.lang.pair) {
               pair = (kawa.lang.pair)pair.cdr;
               result = i.eval(pair.car,frames);
            }
         } 
         return result;
      } else {
         throw new kawa.lang.GenericError("Malformed when statement.");
      }

   }

}
