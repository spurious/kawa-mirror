package kawa.lang;

// An undefined symbol was evaled.
public class WrongType extends Exception {
   //-- number of the argument
   public int number;
   //-- type of the argument
   public java.lang.String typeExpected;
   //-- Procedure name that threw the exception
   public java.lang.String procname;

   public WrongType(java.lang.String name,int n,java.lang.String u) {
      procname = new java.lang.String(name);
      number = n;
      typeExpected = new java.lang.String(u);
   }

  public WrongType (Symbol name, int n, java.lang.String u)
  {
    this (name.toString (), n, u);
  }
}
