package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public abstract class Procedure1 extends Named implements Executable {

   public Procedure1(java.lang.String n) {
      super(n);
   }

   public abstract Object apply1 (Object arg1)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol;


   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object list
   ) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (list instanceof kawa.lang.pair) {
         kawa.lang.pair p = (kawa.lang.pair)list;
         if (p.cdr instanceof kawa.lang.snull) {
            return apply1(p.car);
         }
      } 
      throw new kawa.lang.WrongArguments(this.name,1,"(?)");
   }
}
