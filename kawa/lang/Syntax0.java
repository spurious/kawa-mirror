package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public abstract class Syntax0 extends Named implements Syntaxable {

   public Syntax0(java.lang.String n) {
      super(n);
   }

   public abstract Object execute0(kawa.lang.Interpreter i,java.util.Vector frames)
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
         return execute0(i,frames);
      } 
      throw new kawa.lang.WrongArguments(this.name,0,"(?)");
   }
}
