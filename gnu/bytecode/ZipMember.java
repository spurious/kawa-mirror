// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.Hashtable;
import java.io.*;

/** Information about one member of a ZipArchive. */

public class ZipMember extends java.util.zip.ZipEntry
{
  ZipMember next; 
  long compressed_size;
  //  short filename_length;
  long relative_offset_local_header;  // start of local directory

  public ZipMember (String zname)
  {
    super(zname);
  }

  /* Where the actual data bytes start, withing the achive. */
  long fileStart ()
  {
    return relative_offset_local_header + ZipArchive.LREC_SIZE + 4
      + getName().length();
  }

  public void print (PrintStream ps)
  {
    ps.print (getName());
    ps.println (" size: "+compressed_size+" position: "+fileStart ());
  }

  boolean matches (String match_name)
  {
    return match_name.equals (getName ());
  }

  public byte[] getData (ZipArchive archive) throws IOException
  {
    archive.file.seek (fileStart());
    byte[] result = new byte[(int) compressed_size];
    archive.file.readFully (result);
    return result;
  }
};
