package kawa.standard;

import kawa.lang.Syntaxable;

public class begin extends kawa.lang.Named implements Syntaxable {
   public kawa.standard.begin() {
      super("begin");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object formo)
      throws kawa.lang.WrongType,
             kawa.lang.WrongArguments,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      Object result = kawa.lang.Interpreter.undefinedObject;
      if (formo instanceof kawa.lang.pair) {
         while (formo instanceof kawa.lang.pair) {
            kawa.lang.pair pair = (kawa.lang.pair)formo;
            result = i.eval(pair.car,frames);
            formo = pair.cdr;
         }
      }

      return result;

   }

}
