package kawa.standard;

import kawa.lang.Executable;

public class list extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.list() {
      super("list");
   }

   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arglist
   ) 
   {
      return arglist;
   }

}
