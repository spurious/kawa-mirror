package kawa.lang;

public class WrongArguments extends IllegalArgumentException {
  //-- negative indicates that the right number of arguments was used
  public int number;
  //-- usage description for procedure
  public String usage;
  //-- Procedure name that threw the exception
  public String procname;

  Procedure proc;

  public String getMessage()
  {
    if (proc != null)
      {
	int x = proc.minArgs();
	if (number < x)
	  return "too few arguments ("+number+ 
	    ") to "+proc.name()+" (requires "+x+")";
	x = proc.maxArgs();
	if (x >= 0 && number > x)
	  return "too many argments ("+number+ 
	    ") to "+proc.name()+" (at most "+x+")";
      }
    return super.getMessage();
  }

  public WrongArguments(Procedure proc, int argCount)
  {
    this.proc = proc;
    number = argCount;
  }

   public WrongArguments(java.lang.String name,int n,java.lang.String u) {
      procname = name;
      number = n;
      usage = u;
   }
}
