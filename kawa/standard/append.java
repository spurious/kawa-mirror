package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Executable;

public class append extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.append() {
      super("append");
   }

   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arglist
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arglist instanceof kawa.lang.pair) {
         kawa.lang.pair arg = (kawa.lang.pair)arglist;

         if (arg.car instanceof kawa.lang.pair) {
            int argcount = 1;
            kawa.lang.pair pair    = (kawa.lang.pair)arg.car;
            kawa.lang.pair newlist = i.copy(pair);
            kawa.lang.pair last    = i.lastpair(pair);

            while (arg.cdr instanceof kawa.lang.pair) {
               argcount++;
               arg = (kawa.lang.pair)arg.cdr;
               if (arg.car instanceof kawa.lang.pair) {
                  pair = (kawa.lang.pair)arg.car;
                  kawa.lang.pair list = i.copy(pair);
                  last.cdr = list;
                  last = i.lastpair(list);
               } else {
                  throw new kawa.lang.WrongType(this.name,argcount,"list");
               }
            }                             

            if (arg.cdr instanceof kawa.lang.snull) {
               return newlist;
            } else {
               throw new kawa.lang.WrongType(this.name,argcount,"list");
            }
         } else {
            throw new kawa.lang.WrongArguments(this.name,1,"(append list ...)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,1,"(append list ...)");
      }
   }

}
