package kawa.lang;

public class vector extends Object implements Printable {

    public java.util.Vector value;
    public int size;

    public kawa.lang.vector(int num,Object o) {
       value = new java.util.Vector(num);
       size = num;
       for (int t=0; t<size; t++) {
          value.addElement(o);
       }
    }

    public kawa.lang.vector(java.util.Vector v) {
       value = (java.util.Vector)v.clone();
       size = v.size();
    }

    public void print(java.io.PrintStream ps) {
       ps.print("#(");
       for (int t=0; t<size; t++) {
          if (t!=0)
             ps.print(" ");
	  kawa.lang.print.print (value.elementAt(t), ps);
       }
       ps.print(")");
    }

}
