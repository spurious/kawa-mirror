package kawa.standard;
import kawa.lang.*;

import kawa.lang.Syntaxable;

public class unless extends kawa.lang.Named implements kawa.lang.Syntaxable {
   public kawa.standard.unless() {
      super("unless");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object formo)
      throws kawa.lang.WrongType,
             kawa.lang.WrongArguments,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (formo instanceof Pair) {
         Pair pair = (Pair)formo;
         java.lang.Boolean cond = (java.lang.Boolean)i.eval(pair.car,frames);

         Object result = kawa.lang.Interpreter.undefinedObject;
         if (!cond.booleanValue()) {
            while (pair.cdr instanceof Pair) {
               pair = (Pair)pair.cdr;
               result = i.eval(pair.car,frames);
            }
         } 
         return result;
      } else {
         throw new kawa.lang.GenericError("Malformed when statement.");
      }

   }

}
