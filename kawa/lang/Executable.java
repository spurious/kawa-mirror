package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public interface Executable {
   public java.lang.String name();
   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object list)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
      ;
}
