package kawa.lang;

import java.io.BufferedOutputStream;

public class oport extends BufferedOutputStream {

   private int bufferSize;
   private int peekBuffer[];
   private int peekindex = -1;
   private int peekcursor = 0;

   public oport(java.io.OutputStream i) {
      super(i);
      peekBuffer = new int[8];
   }
   
   public oport(java.io.OutputStream i,int size) {
      super(i,size);
      peekBuffer = new int[8];
   }

   public java.io.OutputStream raw() {
      return out;
   }
}
