package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public class LambdaProcedure extends Named implements Executable {
   kawa.lang.pair arguments;
   kawa.lang.pair expressions;
   java.util.Vector frames;
   public LambdaProcedure(
      kawa.lang.pair args,
      kawa.lang.pair exp,
      java.util.Vector f
   ) {
      super("lambda-form");
      arguments = args;
      expressions = exp;
      frames = f;
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector exeframes,Object arglist)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      java.util.Hashtable frame = null;

      if (arguments!=null) {
         if (arglist instanceof kawa.lang.pair) {

            frame = new java.util.Hashtable();
            kawa.lang.pair binding = arguments;
            kawa.lang.pair parameter = (kawa.lang.pair)arglist;
            while (binding!=null && parameter!=null) {
               frame.put(((kawa.lang.symbol)binding.car).name,parameter.car);
               if (binding.cdr instanceof kawa.lang.snull) {
                  binding = null;
               } else {
                  binding = (kawa.lang.pair)binding.cdr;
               }
               if (parameter.cdr instanceof kawa.lang.snull) {
                  parameter = null;
               } else {
                  parameter = (kawa.lang.pair)parameter.cdr;
               }
            }

            if (binding!=null || parameter!=null) {
               throw new kawa.lang.GenericError("An invalid number of arguments was specified for the lambda expression.");
            }
          } else {
             throw new kawa.lang.GenericError("No arguments specified for lambda expression that requires arguments.");
          }
      } else if (arglist instanceof kawa.lang.pair) {
         throw new kawa.lang.GenericError("Arguments specified for lambda expression that requires no arguments.");
      }


      kawa.lang.pair expression = expressions;

      if (frame!=null) {
         frames.addElement(frame);
      }

      Object result = i.undefinedObject;

      while (expression!=null) {
         try {
            result = i.eval(expression.car,frames);
         } catch (kawa.lang.WrongArguments e) {
            if (frame!=null) {
                frames.removeElement(frame);
            }
            throw e;
         } catch (kawa.lang.WrongType e) {
            if (frame!=null) {
                frames.removeElement(frame);
            }
            throw e;
         } catch (kawa.lang.GenericError e) {
            if (frame!=null) {
                frames.removeElement(frame);
            }
            throw e;
         } catch (kawa.lang.UnboundSymbol e) {
            if (frame!=null) {
                frames.removeElement(frame);
            }
            throw e;
         }
         if (expression.cdr instanceof kawa.lang.pair) {
            expression = (kawa.lang.pair)expression.cdr;
         } else {
            expression = null;
         }
      }

      if (frame!=null) {
         frames.removeElement(frame);
      }

      return result;

   }

   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.lang.LambdaProcedure>");
   }
}
