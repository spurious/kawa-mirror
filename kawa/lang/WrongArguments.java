package kawa.lang;

public class WrongArguments extends Exception {
   //-- negative indicates that the right number of arguments was used
   public int number;
   //-- usage description for procedure
   public String usage;
   //-- Procedure name that threw the exception
   public String procname;

   public WrongArguments(java.lang.String name,int n,java.lang.String u) {
      procname = name;
      number = n;
      usage = u;
   }
}
