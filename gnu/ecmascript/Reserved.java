package gnu.ecmascript;
import kawa.lang.*;

public class Reserved
{
  String name;
  int prio;
  Procedure proc;

  public static final int PLUS_OP = 1;
  public static final int MINUS_OP = 2;
  public static final int TIMES_OP = 3;
  public static final int LSHIFT_OP = 4;
  public static final int LESS_OP = 5;

  public Reserved (String name, int prio, Procedure proc)
  {
    this.name = name;
    this.prio = prio;
    this.proc = proc;
  }

  public Reserved (String name, int prio, int op)
  {
    this.name = name;
    this.prio = prio;
    this.proc = new BinaryOp(name, op);
  }

  final static Reserved lessOp = new Reserved("<", 4, LESS_OP);
  final static Reserved lshiftOp = new Reserved("<<", 6, LSHIFT_OP);
  final static Reserved plusOp = new Reserved("+", 8, PLUS_OP);
  final static Reserved minusOp = new Reserved("-", 8, MINUS_OP);
  final static Reserved timesOp = new Reserved("*", 10, TIMES_OP);

  public String toString() { return "[Reserved \""+name+"\" prio:"+prio+"]"; }

  public boolean isAssignmentOp() { return false; }
}
