// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.Hashtable;
import java.io.*;
import java.util.zip.*;

/** A class to manipulate a .zip archive.
 * Does not handle compression/uncompression, though that could be added.
 * When used an an application. provides a simplified tar-like interface.
 * @author	Per Bothner <bothner@cygnus.com>
 */

public class ZipArchive
{
  //Hashtable directory;

  static final int LREC_SIZE = 26;  // lengths of local file headers, central
  static final int CREC_SIZE = 42;  // directory headers, and the end-of-
  static final int ECREC_SIZE= 18;  // central-dir record, respectively 

  /* Field offsets of end-of-central directory. */
  static final int TOTAL_ENTRIES_CENTRAL_DIR = 10;
  static final int SIZE_CENTRAL_DIRECTORY = 12;

  /* Field offsets of central directory. */
  static final int C_COMPRESSED_SIZE = 16;
  static final int C_UNCOMPRESSED_SIZE = 20;
  static final int C_FILENAME_LENGTH = 24;
  static final int C_RELATIVE_OFFSET_LOCAL_HEADER = 38;

  RandomAccessFile file;
  String archive_name;

  public ZipArchive (String name, String mode) throws IOException
  {
    archive_name = name;
    file = new RandomAccessFile (name, mode);
    readDirectory ();
  }

  public ZipArchive (File file, String mode) throws IOException
  {
    archive_name = file.getName ();
    this.file = new RandomAccessFile (file, mode);
    readDirectory ();
  }

  /** Return a directory entry with the given name, or null if not found. */
  public ZipMember find (String name)
  {
    for (ZipMember zipd = firstEntry;  zipd != null;  zipd = zipd.next)
      {
	if (zipd.matches (name))
	  return zipd;
      }
    return null;
  }

  long size;
  int count_in;  // Number of entries read
  int count_out;

  public int size ()
  {
    return count_in + count_out;
  }

  ZipMember firstEntry, lastEntry;

  byte[] buffer = new byte[100];

  void write2 (int val, byte[] buffer, int pos)
  {
    buffer[pos] = (byte) val;
    buffer[pos+1] = (byte) (val >> 8);
  }

  void write4 (int val, byte[] buffer, int pos)
  {
    buffer[pos] = (byte) (val);
    buffer[pos+1] = (byte) (val >> 8);
    buffer[pos+2] = (byte) (val >> 16);
    buffer[pos+3] = (byte) (val >> 24);
  }

  void writeLocalHeader (ZipMember zmember) throws IOException
  {
    for (int i = 4+LREC_SIZE;  --i >= 0; )
      buffer[i] = 0;
    buffer[0] = (byte) 'P';
    buffer[1] = (byte) 'K';
    buffer[2] = (byte) '\003';
    buffer[3] = (byte) '\004';
    buffer[4] = (byte) '\n';          // 4+L_VERSION_NEEDED_TO_EXTRACT_0
    write2 (zmember.getName().length(), buffer, 4+22);
    write4 ((int) zmember.compressed_size, buffer, 4+14);
    write4 ((int) zmember.getSize(), buffer, 4+18);
    file.write (buffer, 0, 4+LREC_SIZE);
  }

  void writeCentralHeader (ZipMember zmember) throws IOException
  {
    for (int i = 4+CREC_SIZE;  --i >= 0; )
      buffer[i] = 0;
    buffer[0] = (byte) 'P';
    buffer[1] = (byte) 'K';
    buffer[2] = (byte) '\001';
    buffer[3] = (byte) '\002';
    buffer[4] = (byte) '\024';             // 4+C_VERSION_MADE_BY_0
    buffer[5] = (byte) '\003';             // 4+C_VERSION_MADE_BY_1
    buffer[6] = (byte) '\n';               // 4+C_VERSION_NEEDED_TO_EXTRACT_0
    write4 ((int) zmember.compressed_size, buffer, 4+16);
    write4 ((int) zmember.getSize(), buffer, 4+20);
    write2 (zmember.getName().length(), buffer, 4+24);
    write4 ((int) zmember.relative_offset_local_header, buffer, 4+38);
    file.write (buffer, 0, 4+CREC_SIZE);
  }

  public void close () throws IOException
  {
    if (count_out > 0)
      writeEndHeaders ();
    file.close ();
  }

  void writeEndHeaders () throws IOException
  {
    int count = 0;
    long dir_start = lastEntry == null ? 0
      : lastEntry.fileStart () + lastEntry.compressed_size;
    file.seek (dir_start);
    for (ZipMember zmemb = firstEntry;  zmemb != null;  zmemb = zmemb.next)
      {
	writeCentralHeader (zmemb);
	String name = zmemb.getName();
	int len = name.length ();
	byte[] bname = new byte[len];
	name.getBytes (0, len, bname, 0);
	file.write (bname);
	count++;
      }
    if (count != count_in + count_out)
      throw new Error ("internal error writeEndHeaders");
    long dir_size = file.getFilePointer () - dir_start;
    for (int i = 4+ECREC_SIZE;  --i >= 0; )
      buffer[i] = 0;
    buffer[0] = (byte) 'P';
    buffer[1] = (byte) 'K';
    buffer[2] = (byte) '\005';
    buffer[3] = (byte) '\006';
    write2 (count, buffer, 8);       // NUM_ENTRIES_CENTRL_DIR_THS_DISK
    write2 (count, buffer, 10);      // TOTAL_ENTRIES_CENTRAL_DIR
    write4 ((int) dir_size, buffer, 12);   // SIZE_CENTRAL_DIRECTORY
    write4 ((int) dir_start, buffer, 16);  // OFFSET_START_CENTRAL_DIRECTORY
    file.write (buffer, 0, 4+ECREC_SIZE);
  }

  ZipMember addMember (String name)
  {
    ZipMember zmemb = new ZipMember(name);
    if (firstEntry == null)
      firstEntry = zmemb;
    else
      lastEntry.next = zmemb;
    lastEntry = zmemb;
    return zmemb;
  }

  public ZipMember append (byte[] name, byte[] contents) throws IOException
  {
    return append(new String(name, 0), contents);
  }

  public ZipMember append (String name, byte[] contents) throws IOException
  {
    int len = name.length ();
    byte[] bname = new byte[len];
    name.getBytes (0, len, bname, 0);
    ZipMember prev = lastEntry;
    ZipMember zmemb = addMember(name);
    count_out++;
    zmemb.compressed_size = contents.length;
    zmemb.setSize(contents.length);
    long start = prev == null ? 0 : prev.fileStart () + prev.compressed_size;
    zmemb.relative_offset_local_header = start;
    file.seek (start);
    writeLocalHeader(zmemb);
    file.write(bname);
    file.write(contents);
    return zmemb;
  }

  private int readu2 () throws IOException
  {
    int byte0 = file.read () & 0xFF;
    int byte1 = file.read () & 0xFF;
    return (byte1 << 8) | byte0;
  }

  private int read4 () throws IOException
  {
    int byte0 = file.read () & 0xFF;
    int byte1 = file.read () & 0xFF;
    int byte2 = file.read () & 0xFF;
    int byte3 = file.read () & 0xFF;
    return (byte3 << 24) + (byte2 << 16) + (byte1 << 8) + byte0;
  }

  void readDirectory () throws IOException
  {
    size = file.length ();
    if (size == 0)
      {
	count_in = 0;
	return;
      }
    if (size < ECREC_SIZE+4) throw new IOException ("zipfile too short");
    file.seek (size - (ECREC_SIZE+4));
    if (file.read () != 'P'
	|| file.read () != 'K'
	|| file.read () != '\005'
	|| file.read () != '\006')
      throw new IOException ("not a valid zipfile");
    file.skipBytes (TOTAL_ENTRIES_CENTRAL_DIR - 4);
    count_in = readu2 ();     // Get TOTAL_ENTRIES_CENTRAL_DIR
    int dir_size = read4 ();  // Get SIZE_CENTRAL_DIRECTORY
    file.seek (size - (dir_size + ECREC_SIZE+4));

    for (int i = 0;  i < count_in;  i++)
      {
	file.skipBytes (4+C_COMPRESSED_SIZE);
	long compressed_size = read4();  // Get C_COMPRESSED_SIZE
	long uncompressed_size = read4(); // Get C_UNCOMPRESSED_SIZE
	int filename_length = readu2();  // Get C_FILENAME_LENGTH
	file.skipBytes (C_RELATIVE_OFFSET_LOCAL_HEADER-(C_FILENAME_LENGTH+2));
	long relative_offset_local_header = read4();
	byte[] name = new byte[filename_length];
	file.readFully(name);
	ZipMember zipd = addMember(new String(name, 0));
	zipd.compressed_size = compressed_size;
	zipd.setSize(uncompressed_size);
	zipd.relative_offset_local_header = relative_offset_local_header;
      }
  }

  private static void usage ()
  {
    System.err.println ("zipfile [ptxq] archive [file ...]");
    System.exit (-1);
  }

  public static long copy(InputStream in, OutputStream out, byte[] buffer)
    throws IOException
  {
    long total = 0;
    for (;;)
      {
	int count = in.read(buffer);
	if (count <= 0)
	  return total;
	out.write(buffer, 0, count); 
	total += count;
      }
  }

  public static void copy(InputStream in, String name, byte[] buffer)
    throws IOException
    {
    File f = new File(name);
    String dir_name = f.getParent();
    if (dir_name != null)
      {
	File dir = new File(dir_name);
	if (! dir.exists())
	  System.err.println("mkdirs:"+dir.mkdirs());
      }
    if (name.charAt (name.length () - 1) != '/')
      {
	OutputStream out = new BufferedOutputStream(new FileOutputStream(f));
	copy(in, out, buffer);
	out.close ();
      }
  }

  /**
   * Manipulate a .zip archive using a tar-like interface.
   * <p>
   * Usage:  <code>ZipArchive</code> <var>command archive</var> [<var>file</var> ...]
   * <dl>
   * <dt><code>ZipArchive t</code> <var>archive file</var> ...<dd>
   *   List information about the named members of the archive.
   * <dt><code>ZipArchive x</code> <var>archive file</var> ...<dd>
   *   Extract the named members from the archive.
   * <dt><code>ZipArchive p</code> <var>archive file</var> ...<dd>
   *   Print the named members from the archive on standard output.
   *   Prints just the raw contents, with no headers or conversion.
   * <dt><code>ZipArchive</code> [<code>ptx</code>] <var>archive</var><dd>
   *   With no arguments, does each command for every member in the archive.
   * <dt><code>ZipArchive q</code> <var>archive file</var> ...<dd>
   *   Add the named files to the end of archive.
   *   Does not check for duplicates.
   * </dl>
   */

  public static void main (String args[]) throws IOException
  {
    if (args.length < 2)
      usage ();
    String command = args[0];
    String archive_name = args[1];

    try
      {
	if (command.equals ("t")
	    || command.equals ("p")
	    || command.equals ("x"))
	  {
	    PrintStream out = System.out;
	    byte[] buf = new byte[1024];
	    if (args.length == 2)
	      {
		BufferedInputStream in
		  = new BufferedInputStream(new FileInputStream(archive_name));
		ZipInputStream zin = new ZipInputStream (in);
		ZipEntry zent;
		while ((zent = zin.getNextEntry()) != null)
		  {
		    String name = zent.getName();
		    if (command.equals("t"))
		      {
			out.print(name);
			out.print(" size: ");
			out.println(zent.getSize());
		      }
		    else if (command.equals("p"))
		      {
			copy(zin, out, buf);
		      }
		    else // commend.equals("x")
		      {
			copy(zin, name, buf);
		      }
		  }
	      }
	    else
	      {
		ZipFile zar = new ZipFile(archive_name);
		for (int i = 2;  i < args.length; i++)
		  {
		    String name = args[i];
		    ZipEntry zent = zar.getEntry(name);
		    if (zent == null)
		      {
			System.err.println ("zipfile " + archive_name + ":" +
					    args[i] + " - not found");
			System.exit (-1);
		      }
		    else if (command.equals("t"))
		      {
			out.print(name);
			out.print(" size: ");
			out.println(zent.getSize());
		      }
		    else if (command.equals("p"))
		      {
			copy(zar.getInputStream(zent), out, buf);
		      }
		    else // commend.equals("x")
		      {
			copy(zar.getInputStream(zent), name, buf);
		      }
		  }
	      }
	  }
	else if (command.equals ("q"))
	  {
	    ZipArchive zar = new ZipArchive (archive_name, "rw");
	    for  (int i = 2;  i < args.length; i++)
	      {
		File in = new File (args[i]);
		if (!in.exists ())
		  throw new IOException (args[i] + " - not found");
		if (!in.canRead ())
		  throw new IOException (args[i] + " - not readable");
		int size = (int) in.length ();
		FileInputStream fin = new FileInputStream (in);
		byte[] contents = new byte[size];
		if (fin.read (contents) != size)
		  throw new IOException (args[i] + " - read error");
		fin.close ();
		zar.append (args[i], contents);
	      }
	    zar.close ();
	  }
	else
	  usage ();
      }
    catch (IOException ex)
      {
	System.err.println ("I/O Exception:  " + ex);
      }
  }
}
