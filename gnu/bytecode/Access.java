package codegen;
import java.io.*;

/** Access flags. */

public class Access {
  static public final short PUBLIC      = 0x0001;
  static public final short PRIVATE     = 0x0002;
  static public final short PROTECTED   = 0x0004;
  static public final short STATIC      = 0x0008;
  static public final short FINAL       = 0x0010;
  static public final short SYNCHRONIZED= 0x0020;
  static public final short VOLATILE    = 0x0040;
  static public final short TRANSIENT   = 0x0080;
  static public final short NATIVE      = 0x0100;
  static public final short INTERFACE   = 0x0200;
  static public final short ABSTRACT    = 0x0400;
}

