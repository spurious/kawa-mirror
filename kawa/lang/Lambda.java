package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public class Lambda extends Named implements Syntaxable {

   public Lambda() {
      super("lambda");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (arglist instanceof kawa.lang.pair) {
         kawa.lang.pair pair = (kawa.lang.pair)arglist;
         if ((pair.car instanceof kawa.lang.snull || pair.car instanceof kawa.lang.pair)
             && pair.cdr instanceof kawa.lang.pair) {

             java.util.Vector f = null;
             if (frames!=null) {
                f = (java.util.Vector)frames.clone();
             }
             if (pair.car instanceof kawa.lang.snull) {
                return new LambdaProcedure(null,(kawa.lang.pair)pair.cdr,f);
             } else {
                return new LambdaProcedure((kawa.lang.pair)pair.car,(kawa.lang.pair)pair.cdr,f);
             }

         } else {
            throw new kawa.lang.GenericError("Malformed lambda expression.");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(lambda idspec e1 e2 ...)");
      }
   }

   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.lang.Lambda>");
   }
}
