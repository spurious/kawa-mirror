package codegen;
import java.io.*;
import codegen.*;

public class Variable {
  Variable next;
  int offset;
  int start_pc;
  int end_pc;
  public Type type;
  public byte[] name;
  final boolean dead () { return end_pc > 0; }

  public byte[] utfName ()
  {
    return name;
  }

  public String strName ()
  {
    // FFIXME - only works for ASCII names!
    return name == null ? null : new String (name, 0);
  }
}
