package kawa.lang;

import java.io.BufferedInputStream;

public class iport extends BufferedInputStream {

   private int bufferSize;
   private int peekBuffer[];
   private int peekindex = -1;
   private int peekcursor = 0;

   public iport(java.io.InputStream i) {
      super(i);
      peekBuffer = new int[8];
   }
   
   public iport(java.io.InputStream i,int size) {
      super(i,size);
      peekBuffer = new int[8];
   }

   public java.io.InputStream raw() {
      return in;
   }

   public int peek() 
      throws java.io.IOException
   {
      if (peekindex<peekcursor) {
         peekBuffer[++peekindex] = in.read();
         peekcursor = peekindex;
      }
      //System.out.println("peek():");
      //System.out.println("peekindex = "+peekindex);
      //System.out.println("peekcursor = "+peekcursor);
      //dumpPeekBuffer();
      return peekBuffer[peekcursor++];
   }

   public void putback(int c) {
      for (int t=peekindex+1; t>0; t--) {
         peekBuffer[t] = peekBuffer[t-1];
      }
      peekBuffer[0] = c;
      peekindex++;
   }

   public void unpeek()
   {
      //System.out.println("unpeek():");
      if (peekcursor>0) {
         //System.out.println("peekindex = "+peekindex);
         //System.out.println("peekcursor = "+peekcursor);
         //dumpPeekBuffer();
         peekcursor--;
         //System.out.println("peekcursor = "+peekcursor);
      }
   }


   public void flushLastPeek() {
      if (peekcursor>0) {
         //System.out.println("Flushing last peek...");
         peekcursor--;
         peekindex--;
      }
   }

   public void flushPeekBuffer() {
      peekindex = -1;
      peekcursor = 0;
   }

   public int flushPeek() {
      //System.out.println("flushPeek():");
      if (peekindex>=0) {
         //System.out.println("Flushing peek...");
         int retval = peekBuffer[0];
         for (int t=0; t<peekindex; t++) {
            peekBuffer[t] = peekBuffer[t+1];
         }
         peekindex--;
         peekcursor = 0;
         //dumpPeekBuffer();
         return retval;
      } else {
         return 0;
      }
   }

   public int read() 
      throws java.io.IOException
   {
      //System.out.println("read():");
      if (peekindex<0) {
         //System.out.println("Reading character...");
         return in.read();
      } else {
         //System.out.println("Returning peek value...");
         //System.out.println("peekindex = "+peekindex);
         //dumpPeekBuffer();
         peekcursor--;
         int retval = peekBuffer[0];
         for (int t=0; t<peekindex; t++) {
            peekBuffer[t] = peekBuffer[t+1];
         }
         peekindex--;
         //dumpPeekBuffer();
         return retval;
      }
   }

   protected void dumpPeekBuffer() {
      System.out.print("peekBuffer='");
      for (int t=0; t<=peekindex; t++) {
         System.out.print((char)peekBuffer[t]);
      }
      System.out.println("'");
   }

}
