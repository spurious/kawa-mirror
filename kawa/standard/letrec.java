package kawa.standard;
import kawa.lang.*;

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
      if (formo instanceof Pair) {
         Pair form = (Pair)formo;
         Object obj1 = form.car;
         Object obj2 = form.cdr;
         if (obj1 instanceof Pair) {
            java.util.Hashtable frame = new java.util.Hashtable();

            //-- Bind dummy values
            Object bobj = obj1;
            while (bobj instanceof Pair) {
               Pair blist = (Pair)bobj;
               if (blist.car instanceof Pair) {
                  Pair binding = (Pair)blist.car;
                  if (binding.car instanceof kawa.lang.Symbol && 
                      binding.cdr instanceof Pair) {
                     Pair vpair = (Pair)binding.cdr;
                     if (vpair.cdr == List.Empty) {
                        frame.put(((Symbol)binding.car).toString(),kawa.lang.Interpreter.falseObject);
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
            while (bobj instanceof Pair) {
               Pair blist = (Pair)bobj;
               Pair binding = (Pair)blist.car;
               Pair vpair = (Pair)binding.cdr;
               Object value = i.eval(vpair.car,frames);
               frame.put(((Symbol)binding.car).toString(),value);
               bobj = blist.cdr;
            } 

            //-- Add frame to execution frames
            frames.addElement(frame);

            Object result = kawa.lang.Interpreter.undefinedObject;
            while (obj2 instanceof Pair) {
               Pair pair = (Pair)obj2;
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
