package kawa.lang;

public class WrongArguments extends Exception {
   //-- negative indicates that the right number of arguments was used
   public int number;
   //-- usage description for procedure
   public java.lang.String usage;
   //-- Procedure name that threw the exception
   public java.lang.String procname;

   public WrongArguments(java.lang.String name,int n,java.lang.String u) {
      procname = new java.lang.String(name);
      number = n;
      usage = new java.lang.String(u);
   }

  public WrongArguments(Symbol name, int n, String u)
  {
    this (name.toString (), n, u);
  }
}
