package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public abstract class Syntax1 extends Named implements Syntaxable {

   public Syntax1(java.lang.String n) {
      super(n);
   }

   public abstract Object execute1(kawa.lang.Interpreter i,java.util.Vector frames,Object arg1)
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
            return execute1(i,frames,p.car);
         }
      } 
      throw new kawa.lang.WrongArguments(this.name,1,"(?)");
   }
}
