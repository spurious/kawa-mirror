package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public abstract class Procedure0 extends Named implements Executable {

   public Procedure0(java.lang.String n) {
      super(n);
   }

   public abstract Object apply0 ()
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
      if (list instanceof kawa.lang.snull) {
         return apply0 ();
      } 
      throw new kawa.lang.WrongArguments(this.name,0,"(?)");
   }
}
