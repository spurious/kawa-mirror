package kawa.lang;

// An undefined symbol was evaled.
public class GenericError extends Exception {
   public java.lang.String message;

   public GenericError(java.lang.String msg) {
      message = new java.lang.String(msg);
   }
}
