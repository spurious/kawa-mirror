package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Syntax2;
import java.io.PrintStream;

public class set_b extends kawa.lang.Syntax2 {
   public kawa.standard.set_b() {
      super("set!");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType,
            kawa.lang.GenericError,
            kawa.lang.UnboundSymbol
   {
      if (arg1 instanceof kawa.lang.symbol) {
         kawa.lang.symbol s = (kawa.lang.symbol)arg1;
         Object o = i.eval(arg2,frames);
         if (frames!=null) {
            int length = frames.size();
            for (int f=length-1; f>=0; f--) {
               java.util.Hashtable frame = (java.util.Hashtable)frames.elementAt(f);
               Object p = frame.get(s.name);
               if (p!=null) {
                  frame.put(s.name,o);
                  return i.undefinedObject;
               }
            } 
         }
         if (i.lookup(s.name)!=null) {
            i.define(s.name,o);
            return i.undefinedObject;
         } else {
            throw new kawa.lang.UnboundSymbol(s.name);
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(set! symbol obj)");
      }
   }
}
