package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Syntaxable;
import java.util.Hashtable;

public class letrec extends kawa.lang.Named implements kawa.lang.Syntaxable {
   public kawa.standard.letrec() {
      super("letrec");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object formo)
      throws kawa.lang.WrongType,
             kawa.lang.WrongArguments,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (formo instanceof kawa.lang.pair) {
         kawa.lang.pair form = (kawa.lang.pair)formo;
         Object obj1 = form.car;
         Object obj2 = form.cdr;
         if (obj1 instanceof kawa.lang.pair) {
            java.util.Hashtable frame = new java.util.Hashtable();

            //-- Bind dummy values
            Object bobj = obj1;
            while (bobj instanceof kawa.lang.pair) {
               kawa.lang.pair blist = (kawa.lang.pair)bobj;
               if (blist.car instanceof kawa.lang.pair) {
                  kawa.lang.pair binding = (kawa.lang.pair)blist.car;
                  if (binding.car instanceof kawa.lang.symbol && 
                      binding.cdr instanceof kawa.lang.pair) {
                     kawa.lang.pair vpair = (kawa.lang.pair)binding.cdr;
                     if (vpair.cdr instanceof kawa.lang.snull) {
                        frame.put(((kawa.lang.symbol)binding.car).toString(),kawa.lang.Interpreter.falseObject);
                     } else {
                        throw new kawa.lang.GenericError("Malformed let binding.");
                     }
                  } else {
                     throw new kawa.lang.GenericError("Malformed let binding.");
                  }
               } else {
                  throw new kawa.lang.GenericError("Malformed let binding.");
               }
               bobj = blist.cdr;
            } 

            //-- Bind values
            bobj = obj1;
            while (bobj instanceof kawa.lang.pair) {
               kawa.lang.pair blist = (kawa.lang.pair)bobj;
               kawa.lang.pair binding = (kawa.lang.pair)blist.car;
               kawa.lang.pair vpair = (kawa.lang.pair)binding.cdr;
               Object value = i.eval(vpair.car,frames);
               frame.put(((kawa.lang.symbol)binding.car).toString(),value);
               bobj = blist.cdr;
            } 

            //-- Add frame to execution frames
            frames.addElement(frame);

            Object result = kawa.lang.Interpreter.undefinedObject;
            while (obj2 instanceof kawa.lang.pair) {
               kawa.lang.pair pair = (kawa.lang.pair)obj2;
               result = i.eval(pair.car,frames);
               obj2 = pair.cdr;
            }

            //-- Remove frame from execution frames
            frames.removeElement(frame);

            return result;

         } else {
            throw new kawa.lang.WrongArguments(this.name,2,"(let ((n obj) ...) obj)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(let ((n obj) ...) obj)");
      }

   }

}
