package codegen;
import java.util.Hashtable;
import java.io.*;

/** Information about one member of a ZipArchive. */

public class ZipMember
{
  ZipMember next; 
  int compressed_size;
  int uncompressed_size;
  //  short filename_length;
  int relative_offset_local_header;  // start of local directory

  byte[] name;
  private String str_name;

  /* Where the actual data bytes start, withing the achive. */
  int fileStart ()
  {
    return relative_offset_local_header + ZipArchive.LREC_SIZE + 4 + name.length;
  }

  public void print (PrintStream ps)
  {
    System.out.write (name, 0, name.length);
    System.out.println (" size: "+compressed_size+" position: "+fileStart ());
  }

  public String strName ()
  {
    if (str_name == null)
      str_name = new String (name, 0);
    return str_name;
  }

  boolean matches (String match_name)
  {
    if (name.length != match_name.length ())
      return false;
    return match_name.equals (strName ());
  }

};
