package kawa.lang;

// An undefined symbol was evaled.
public class WrongType extends Exception {
   //-- number of the argument
   public int number;
   //-- type of the argument
   public String typeExpected;
   //-- Procedure name that threw the exception
   public String procname;

   public WrongType(String name, int n, String u) {
      procname = name;
      number = n;
      typeExpected = u;
   }
}
