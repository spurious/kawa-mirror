package kawa.standard;

public class vector extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.vector() {
      super("vector");
   }

   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arglist
   ) 
   {
      int count = 0;
      Object o = arglist;
      java.util.Vector v = new java.util.Vector();
      while (o instanceof kawa.lang.pair) {
         count++;
         v.addElement(((kawa.lang.pair)o).car);
         o = ((kawa.lang.pair)o).cdr;
      }

      return new kawa.lang.vector(v);

   }

}
