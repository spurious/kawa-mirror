package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Syntaxable;
import java.util.Hashtable;

public class let extends kawa.lang.Named implements kawa.lang.Syntaxable {
   public kawa.standard.let() {
      super("let");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object formo)
      throws kawa.lang.WrongType,
             kawa.lang.WrongArguments,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (formo instanceof kawa.lang.pair) {
         //-- regular let
         kawa.lang.pair form = (kawa.lang.pair)formo;
         Object obj1 = form.car;
         Object obj2 = form.cdr;
         if (obj1 instanceof kawa.lang.pair) {
            java.util.Hashtable frame = new java.util.Hashtable();

            //-- Bind values
            Object bobj = obj1;
            while (bobj instanceof kawa.lang.pair) {
               kawa.lang.pair blist = (kawa.lang.pair)bobj;
               if (blist.car instanceof kawa.lang.pair) {
                  kawa.lang.pair binding = (kawa.lang.pair)blist.car;
                  if (binding.car instanceof kawa.lang.symbol && 
                      binding.cdr instanceof kawa.lang.pair) {
                     kawa.lang.pair vpair = (kawa.lang.pair)binding.cdr;
                     if (vpair.cdr instanceof kawa.lang.snull) {
                        Object value = i.eval(vpair.car,frames);
                        frame.put(((kawa.lang.symbol)binding.car).name,value);
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

            //-- Add frame to execution frames
            frames.addElement(frame);

            Object result = i.undefinedObject;
            while (obj2 instanceof kawa.lang.pair) {
               kawa.lang.pair pair = (kawa.lang.pair)obj2;
               result = i.eval(pair.car,frames);
               obj2 = pair.cdr;
            }

            //-- Remove frame from execution frames
            frames.removeElement(frame);

            return result;

         } else if (obj1 instanceof kawa.lang.symbol && obj2 instanceof kawa.lang.pair) {
            //-- Named let

            kawa.lang.pair p = (kawa.lang.pair)obj2;

            kawa.lang.pair arguments = null;
            kawa.lang.pair larg = null;
            Object values = i.nullObject;
            kawa.lang.pair lval = null;

            if (p.car instanceof kawa.lang.pair && p.cdr instanceof kawa.lang.pair) {
               Object bobj = p.car;
               while (bobj instanceof kawa.lang.pair) {
                  kawa.lang.pair blist = (kawa.lang.pair)bobj;
                  if (blist.car instanceof kawa.lang.pair) {
                     kawa.lang.pair binding = (kawa.lang.pair)blist.car;
                     if (binding.car instanceof kawa.lang.symbol && 
                         binding.cdr instanceof kawa.lang.pair) {
                        kawa.lang.pair vpair = (kawa.lang.pair)binding.cdr;
                        if (vpair.cdr instanceof kawa.lang.snull) {
                           if (larg==null) {
                              arguments = new kawa.lang.pair(binding.car,i.nullObject);
                              larg = arguments;
                           } else {
                              larg.cdr = new kawa.lang.pair(binding.car,i.nullObject);
                              larg = (kawa.lang.pair)larg.cdr;
                           }
                           if (lval==null) {
                              lval = new kawa.lang.pair(vpair.car,i.nullObject);
                              values = lval;
                           } else {
                              lval.cdr = new kawa.lang.pair(vpair.car,i.nullObject);
                              lval = (kawa.lang.pair)lval.cdr;
                           }
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

               java.util.Hashtable frame = new java.util.Hashtable();

               kawa.lang.Executable proc = new kawa.lang.LambdaProcedure(
                  arguments,
                  (kawa.lang.pair)p.cdr,
                  frames
               );

               frame.put(
                  ((kawa.lang.symbol)obj1).name,
                  proc
               );

               kawa.lang.pair exec = new kawa.lang.pair(proc,values);

               //-- Add frame to execution frames
               frames.addElement(frame);

               Object result = i.eval(exec);

               //-- Remove frame from execution frames
               frames.removeElement(frame);

               return result;

            } else {
               throw new kawa.lang.WrongArguments(this.name,2,"(let sym ((n obj) ...) obj)");
            }
         } else {
            throw new kawa.lang.WrongArguments(this.name,2,"(let ((n obj) ...) obj)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(let ((n obj) ...) obj)");
      }

   }

}
