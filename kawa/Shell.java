package kawa;

import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

public class Shell {
   protected java.io.PrintStream pout;
   protected java.io.PrintStream perr;
   kawa.lang.Interpreter interpreter;
   boolean prompt;
   boolean display;

   public Shell(
      kawa.lang.Interpreter interp,
      boolean pflag,
      boolean dflag
   ) {
      if (interp.out.raw() instanceof java.io.PrintStream) {
         pout = (java.io.PrintStream)interp.out.raw();
      } else {
         pout = new java.io.PrintStream(interp.out.raw());
      }
      if (interp.err.raw() instanceof java.io.PrintStream) {
         perr = (java.io.PrintStream)interp.err.raw();
      } else {
         perr = new java.io.PrintStream(interp.err.raw());
      }
      interpreter = interp;
      prompt = pflag;
      display = dflag;
   }

   public int run() {

      Object obj = null;
      boolean noFatalExceptions = true;

      do {

         try {
            if (prompt) {
               pout.print("kawa>");
               pout.flush();
            }
            try {
               obj = interpreter.read();
               if (obj!=null) {
                  obj = interpreter.eval(obj);
                  if (obj!=null) {
                     if (obj instanceof kawa.lang.Exit) {
                        obj = null;
                     } else if (display) {
                        if (obj instanceof kawa.lang.Printable) {
                           ((kawa.lang.Printable)obj).print(pout);
                        } else if (obj instanceof java.lang.StringBuffer) {
                           pout.print(((java.lang.StringBuffer)obj).toString());
                        } else if (obj instanceof java.lang.Integer) {
                           pout.print(((java.lang.Integer)obj).toString());
                        } else if (obj instanceof java.lang.Double) {
                           pout.print(((java.lang.Double)obj).toString());
                        } else if (obj instanceof java.lang.Boolean) {
                           if (((java.lang.Boolean)obj).booleanValue()) {
                              pout.print("#t");
                           } else {
                              pout.print("#f");
                           }
                        } else if (obj instanceof java.lang.Character) {
                           pout.print(((java.lang.Character)obj).charValue());
                        } else {
                           pout.print(obj.getClass().getName());
                        }
                        pout.println();
                        pout.flush();
                     }
                  }
               }
            } catch (kawa.lang.WrongArguments e) {
               perr.println();
               perr.println("Wrong arguments to procedure "+e.procname+", expected "+e.number+".");
               perr.println("usage: "+e.usage);
            } catch (kawa.lang.WrongType e) {
               perr.println();
               perr.println("Argument "+e.number+" to "+e.procname+" must be of type "+e.typeExpected);
            } catch (kawa.lang.GenericError e) {
               perr.println();
               perr.println(e.message);
            } catch (java.lang.ClassCastException e) {
               perr.println();
               perr.println("Invalid parameter, should be: "+ e.getMessage());
               e.printStackTrace(perr);
            }
         } catch (kawa.lang.EOFInComment e) {
            perr.println();
            perr.println("An <EOF> occurred in a #| comment.");
            noFatalExceptions = false;
         } catch (kawa.lang.UnexpectedCloseParen e) {
            perr.println();
            perr.println("An unexpected close paren was read.");
            /*try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            } */
         } catch (kawa.lang.InvalidPoundConstruct e) {
            perr.println();
            perr.println("An invalid pound construct was read.");
            /*try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            } */
         } catch (kawa.lang.EOFInString e) {
            perr.println();
            perr.println("An <EOF> occurred in a string.");
            noFatalExceptions = false;
         } catch (kawa.lang.NumberTooLong e) {
            perr.println();
            perr.println("The number was too long for the interpreter to read.");
            /*try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            } */
         } catch (kawa.lang.InvalidCharacterName e) {
            perr.println();
            perr.println("Invalid character name.");
            /*try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            } */
         } catch (kawa.lang.MalformedList e) {
            perr.println();
            perr.println("Malformed list.");
            /*try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            }  */
         } catch (kawa.lang.UnboundSymbol e) {
            perr.println();
            perr.println("Unbound symbol "+e.symbol+" in execution.");
            /* try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            }  */
         } catch (kawa.lang.NotImplemented e) {
            perr.println();
            perr.println("Not Implemented.");
            /*
            try {
               System.in.skip(System.in.available());
            } catch (java.io.IOException e2) {
               perr.println("A fatal IO exception occurred on a read.");
               noFatalExceptions = false;
            } */
         } catch (java.io.IOException e) {
            perr.println();
            perr.println("A fatal IO exception occurred on a read.");
            noFatalExceptions = false;
         } 
      } while (obj!=null && noFatalExceptions);
      return 0;
   }
}
