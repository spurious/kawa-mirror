// Copyright (c) 2001, 2004, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.io.*;
import gnu.mapping.ThreadLocation;
import gnu.lists.LList;

/** A pretty printer.
 *
 * This code is transcribed from pprint.lisp in Steel Bank Common Lisp,
 * which is again based on the code in CMU Common Lisp.
 */

public class PrettyWriter extends java.io.Writer
{
  protected Writer out;

  public PrettyWriter(java.io.Writer out)
  {
    this.out = out;
    prettyPrintingMode = 1;
  }

  public PrettyWriter(java.io.Writer out, int lineLength)
  {
    this.out = out;
    this.lineLength = lineLength;
    prettyPrintingMode = lineLength > 1 ? 1 : 0;
  }

  public PrettyWriter(java.io.Writer out, boolean prettyPrintingMode)
  {
    this.out = out;
    this.prettyPrintingMode = prettyPrintingMode ? 1 : 0;
  }

  /** Line length we should format to. */
  int lineLength = 80;
  int miserWidth = 40;

  public static ThreadLocation lineLengthLoc
    = new ThreadLocation("line-length");
  public static ThreadLocation miserWidthLoc
    = new ThreadLocation("miser-width");
  public static ThreadLocation indentLoc
    = new ThreadLocation("indent");

  /** The current pretty-printing mode.
   * See setPrettyPrintingMode for valid values. */
  int prettyPrintingMode;

  /** Control pretty-printing mode.
   * @param mode the value 0 disables pretty-printing;
   *   the value 1 enables ecplicit pretty-printing;
   *   the value 2 enables pretty-printing with auto-fill, which means that
   *   spaces are treated like enqueing NEWLINE_SPACE (essentiall a 'fill').
   */
  public void setPrettyPrintingMode (int mode)
  { prettyPrintingMode = mode; }

  /** Return pretty-printing mode.
   * @return 0, 1, 2, as described for {@link #setPrettyPrintingMode(int)}.
   */
  public int getPrettyPrintingMode () { return prettyPrintingMode; }

  /** Is pretty printing enabled? */
  public boolean isPrettyPrinting () { return prettyPrintingMode > 0; }

  /** Turn pretty printing on or off.
   * Equivalent to {@code setPrettyPrintingMode(mode?1:0)}.
   */
  public void setPrettyPrinting (boolean mode)
  {
    prettyPrintingMode = mode ? 0 : 1;
  }

  public static int initialBufferSize = 126;

  /** Holds all the text that has been output but not yet printed. */
  public /* FIXME */ char[] buffer = new char[initialBufferSize];

  /** The index into BUFFER where more text should be put. */
  public /* FIXME */ int bufferFillPointer;

  /** Total amount of stuff that has been shifted out of the buffer.
   * Whenever we output stuff from the buffer, we shift the remaining noise
   * over. This makes it difficult to keep references to locations in
   * the buffer. */
  int bufferOffset;

  /** The column the first character in the buffer will appear in.
   * Normally zero, but if we end up with a very long line with no breaks in it
   * we might have to output part of it. Then this will no longer be zero.
   * Ditto after emitting a prompt. */
  int bufferStartColumn;
  
  /** The line number we are currently on. Used for *print-lines* abrevs and
   * to tell when sections have been split across multiple lines. */
  int lineNumber;

  // There are three different units for measuring character positions:
  //   COLUMN - offset (in characters) from the start of the current line.
  //   INDEX - index into the output buffer.
  //   POSN - some position in the stream of characters cycling through
  //          the output buffer.

  private int indexPosn (int index)
  {
    return index + bufferOffset;
  }

  private int posnIndex (int posn)
  {
    return posn - bufferOffset;
  }

  private int posnColumn (int posn)
  {
    return indexColumn(posnIndex(posn));
  }

  /** Stack of logical blocks in effect at the buffer start.
   * I.e. blocks for which {@code reallyStartLogicalBlock} has been called.
   * Each block uses {@code LOGICAL_BLOCK_LENGTH} {@code int} in this array. */
  int[] blocks = new int[10 * LOGICAL_BLOCK_LENGTH];
  /** Number of {@code int}s used by each block in the {@code blocks} array. */
  static final private int LOGICAL_BLOCK_LENGTH = 6;
  static final private int BLOCK_START_COLUMN = -1;
  static final private int BLOCK_SECTION_COLUMN = -2;
  static final private int BLOCK_PER_LINE_PREFIX_END = -3;
  static final private int BLOCK_PREFIX_LENGTH = -4;
  static final private int BLOCK_SUFFIX_LENGTH = -5;
  static final private int BLOCK_SECTION_START_LINE = -6;
  /** The "stack pointer" in the {@code blocks} array. */
  int blockDepth = LOGICAL_BLOCK_LENGTH;

  /** Buffer holding the per-line prefix active at the buffer start.
   * Indentation is included in this. The length of this is stored
   * in the logical block stack. */
  char[] prefix = new char[initialBufferSize];

  /** Buffer holding the total remaining suffix active at the buffer start.
   * The characters are right-justified in the buffer to make it easier
   * to output the buffer. The length is stored in the logical block stack. */
  char[] suffix = new char[initialBufferSize];

  static final int QUEUE_INIT_ALLOC_SIZE = 300; // FIXME

  /** A queue of pending operations.
   * This is primarily stored in the circular buffer queueInts.  There
   * are different kinds of operation types, and each operation can
   * require a variable number of elements in the buffer, depending on
   * the operation type.  Given an operation at 'index', the type
   * operation type code is 'getQueueType(index)' (one of the
   * QITEM_XXX_TYPE macros below), and the number of elements in the
   * buffer is 'getQueueSize(index)' (one of the QITEM_XXX_SIZE values
   * below).  You can think of the various QITEM_XXX_TYPEs as
   * "sub-classes" of queued operations, but instead of creating
   * actual Java objects, we allocate the objects' fields in the
   * queueInts and QueueStrings arrays, to avoid expensive object
   * allocation.  The special QITEM_NOP_TYPE is a used as a marker for
   * when there isn't enough space in the rest of buffer, so we have
   * to wrap around to the start.  The other QITEM_XXX macros are the
   * offsets of the various "fields" relative to the start index. */
  int[] queueInts = new int[QUEUE_INIT_ALLOC_SIZE];

  /** For simplicity, queueStrings is the same size as queueInts. */
  String[] queueStrings = new String[QUEUE_INIT_ALLOC_SIZE];
  /** Index in queueInts and queueStrings of oldest enqueued operation. */
  int queueTail;
  /** Number of elements (in queueInts and queueStrings) in use. */
  int queueSize;
  /** If >= 0, index (into queueInts) of current unclosed begin-block node.
   * This is a head of a linked linked of queued BLOCK_START for which
   * we haven't seen the matching BLOCK_END  */
  int currentBlock = -1;
  /** Number of startLogicalBlock - number of endLogicalBlock. */
  public int pendingBlocksCount;

  /** The first it QITEM contains it type code and size.
   * The type code is one of the QITEM_XXX_TYPE values below.
   * The size is the corresponding QITEM_XXX_SIZE value below,
   * except for the case of QITEM_NOP_TYPE (which is used as a filler). */
  static final int QITEM_TYPE_AND_SIZE = 0;
  private int getQueueType(int index) { return queueInts[index] & 0xFF; }
  private int getQueueSize(int index) { return queueInts[index] >> 16; }
  /** Relative offset of POSN field of a QITEM> */
  static final int QITEM_POSN = 1;
  /** Size of "base part" of a QITEM. */
  static final int QITEM_BASE_SIZE = 2;

  /** A dummy queue item used at the high end of the queue buffer
   * when there isn't enough space for the needed queue item. */
  static final int QITEM_NOP_TYPE = 0;

  /** "Abstract" type for beginning of section.
   * A section is from a block-start to a newline, from a newline to
   * the next newline (in the same block?), or from a newline to
   * the block end (?). */
  /*static final int QITEM_SECTION_START_TYPE = 1;*/
  static final int QITEM_SECTION_START_SIZE = QITEM_BASE_SIZE + 2;
  static final int QITEM_SECTION_START_DEPTH = QITEM_BASE_SIZE;
  static final int QITEM_SECTION_START_SECTION_END = QITEM_BASE_SIZE + 1;

  /** A newline queue item. */
  static final int QITEM_NEWLINE_TYPE = 2;
  static final int QITEM_NEWLINE_SIZE = QITEM_SECTION_START_SIZE + 1;
  static final int QITEM_NEWLINE_KIND = QITEM_SECTION_START_SIZE;
  public static final int NEWLINE_LINEAR = 'N';
  public static final int NEWLINE_LITERAL = 'L';
  public static final int NEWLINE_FILL = 'F';
  /** A non-nested ' ' gets an implicit NEWLINE_SPACE.
   * This is treated similarly to NEWLINE_FILL, but not quite. */
  public static final int NEWLINE_SPACE = 'S';
  public static final int NEWLINE_MISER = 'M';
  public static final int NEWLINE_MANDATORY = 'R';  // "required"

  static final int QITEM_INDENTATION_TYPE = 3;
  static final int QITEM_INDENTATION_SIZE = QITEM_BASE_SIZE + 2;
  static final int QITEM_INDENTATION_KIND = QITEM_BASE_SIZE;
  static final char QITEM_INDENTATION_BLOCK = 'B';
  static final char QITEM_INDENTATION_CURRENT = 'C';
  static final int QITEM_INDENTATION_AMOUNT = QITEM_BASE_SIZE + 1;

  /** A "block-start" queue item. */
  static final int QITEM_BLOCK_START_TYPE = 4;
  static final int QITEM_BLOCK_START_SIZE = QITEM_SECTION_START_SIZE + 3;
  /** If the QITEM_BLOCK_START_BLOCK_END < 0, it points to
   * the previous (outer) un-closed block-start.
   * If QITEM_BLOCK_START_BLOCK_END > 0, it points to the
   * corresponding block-end node.
   * In both cases the pointers are relative to the current BLOCK_START. */
  static final int QITEM_BLOCK_START_BLOCK_END = QITEM_SECTION_START_SIZE;
  static final int QITEM_BLOCK_START_PREFIX = QITEM_SECTION_START_SIZE + 1;
  static final int QITEM_BLOCK_START_SUFFIX = QITEM_SECTION_START_SIZE + 2;

  static final int QITEM_BLOCK_END_TYPE = 5;
  static final int QITEM_BLOCK_END_SIZE = QITEM_BASE_SIZE;

  static final int QITEM_TAB_TYPE = 6;
  static final int QITEM_TAB_SIZE = QITEM_BASE_SIZE + 3;
  static final int QITEM_TAB_FLAGS = QITEM_BASE_SIZE;
  static final int QITEM_TAB_IS_SECTION = 1;
  static final int QITEM_TAB_IS_RELATIVE = 2;
  static final int QITEM_TAB_COLNUM = QITEM_BASE_SIZE + 1;
  static final int QITEM_TAB_COLINC = QITEM_BASE_SIZE + 2;

  private int getSectionColumn()
  {
    return blocks[blockDepth+BLOCK_SECTION_COLUMN];
  }

  private int getStartColumn()
  {
    return blocks[blockDepth+BLOCK_START_COLUMN];
  }

  private int getPerLinePrefixEnd()
  {
    return blocks[blockDepth+BLOCK_PER_LINE_PREFIX_END];
  }

  private int getPrefixLength()
  {
    return blocks[blockDepth+BLOCK_PREFIX_LENGTH];
  }

  private int getSuffixLength()
  {
    return blocks[blockDepth+BLOCK_SUFFIX_LENGTH];
  }

  private int getSectionStartLine()
  {
    return blocks[blockDepth+BLOCK_SECTION_START_LINE];
  }

  boolean wordEndSeen;

  /** Note the end of a "word".  See {@link #writeWordStart}. */
  public void writeWordEnd ()
  {
    wordEndSeen = true;
  }

  /** Maybe write a word-separating space.
   * Specifically, write a space if the previous output
   * was {@link #writeWordEnd}.  Otherwise, do nothing.
   */
  public void writeWordStart ()
  {
    if (wordEndSeen)
      write(' ');
    wordEndSeen = false;
  }

  public void clearWordEnd ()
  {
    wordEndSeen = false;
  }

  public void write (int ch)
  {
    wordEndSeen = false;
    //log("{WRITE-ch: "+((char)ch)+"}");
    if (ch == '\n' && prettyPrintingMode > 0)
      enqueueNewline(NEWLINE_LITERAL);
    else
      {
	ensureSpaceInBuffer(1);
	int fillPointer = bufferFillPointer;
	buffer[fillPointer] = (char) ch;
	bufferFillPointer = 1 + fillPointer;
	if (ch == ' ' && prettyPrintingMode > 1 && currentBlock < 0)
	  enqueueNewline(NEWLINE_SPACE);
      }
  }

  public void write (String str)
  {
    write(str, 0, str.length());
  }

  public void write (String str, int start, int count)
  {
    wordEndSeen = false;
    //log("{WRITE-str: "+str.substring(start, start+count)+"}");
    while (count > 0)
      {
	int cnt = count;
	// May allocate for space than we need (if the buffer gets fluhed).  FIXME
	int available = ensureSpaceInBuffer(count);
	if (cnt > available)
	  cnt = available;
	int fillPointer = bufferFillPointer;
	count -= cnt;
	while (--cnt >= 0)
	  {
	    char ch = str.charAt(start++);
	    if (ch == '\n' && prettyPrintingMode > 0)
	      {
		bufferFillPointer = fillPointer;
		enqueueNewline(NEWLINE_LITERAL);
		fillPointer = bufferFillPointer;
	      }
	    else
	      {
		buffer[fillPointer++] = (char) ch;
		if (ch == ' ' && prettyPrintingMode > 1 && currentBlock < 0)
		  {
		    bufferFillPointer = fillPointer;
		    enqueueNewline(NEWLINE_SPACE);
		    fillPointer = bufferFillPointer;
		  }
	      }
	  }
	bufferFillPointer = fillPointer;
      }
  }

  public void write (char[] str)
  {
    write(str, 0, str.length);
  }

  public void write (char[] str, int start, int count)
  {
    wordEndSeen = false;
    //log("{WRITE: "+new String(str, start, count)+"}");
    int end = start + count;
  retry:
    while (count > 0)
      {
	// Look for newline.  Should be merged with following loop.  FIXME.
	for (int i = start;  i < end;  i++)
	  {
	    char c;
	    if (prettyPrintingMode > 0
		&& ((c = str[i]) == '\n'
		    || (c == ' ' && currentBlock < 0)))
	      {
		write(str, start, i - start); // Recurse
		write(c);
		start = i + 1;
		count = end - start;
		continue retry;
	      }
	  }

	for (;;)
	  {
	    int available = ensureSpaceInBuffer(count);
	    int cnt = available < count ? available : count;
	    int fillPointer = bufferFillPointer;
	    int newFillPtr = fillPointer + cnt;
	    for (int i = fillPointer;  i < newFillPtr;  i++)
	      buffer[i] = str[start++];
	    bufferFillPointer = newFillPtr;
	    count -= cnt;
	    if (count == 0)
	      break;
	  }
      }
  }

  private void pushLogicalBlock(int column,
				int perLineEnd,
				int prefixLength, int suffixLength,
				int sectionStartLine)
  {
    int newLength = blockDepth + LOGICAL_BLOCK_LENGTH;
    if (newLength >= blocks.length)
      {
	int[] newBlocks = new int[2 * blocks.length];
	System.arraycopy(blocks, 0, newBlocks, 0, blockDepth);
	blocks = newBlocks;
      }
    blockDepth = newLength;
    blocks[blockDepth + BLOCK_START_COLUMN] = column;
    blocks[blockDepth + BLOCK_SECTION_COLUMN] = column;
    blocks[blockDepth + BLOCK_PER_LINE_PREFIX_END] = perLineEnd;
    blocks[blockDepth + BLOCK_PREFIX_LENGTH] = prefixLength;
    blocks[blockDepth + BLOCK_SUFFIX_LENGTH] = suffixLength;
    blocks[blockDepth + BLOCK_SECTION_START_LINE] = sectionStartLine;
  }
  
  void reallyStartLogicalBlock(int column, String prefix, String suffix)
  {
    int perLineEnd = getPerLinePrefixEnd();
    int prefixLength = getPrefixLength();
    int suffixLength = getSuffixLength();
    pushLogicalBlock(column, perLineEnd, prefixLength, suffixLength,
		     lineNumber);
    setIndentation(column);
    if (prefix != null)
      {
	blocks[blockDepth + BLOCK_PER_LINE_PREFIX_END] = column;
	int plen = prefix.length();
	prefix.getChars(0, plen, this.suffix, column - plen);
      }
    if (suffix != null)
      {
	// Prepend the new suffix in front of the old suffix in this.suffix.
	// The suffix is stored at the "right" (high-index) end of
	// this.suffix to make it easier to prepend new suffixes.
	char[] totalSuffix = this.suffix;
	int totalSuffixLen = totalSuffix.length;
	int additional = suffix.length();
	int newSuffixLen = suffixLength + additional;
	if (newSuffixLen > totalSuffixLen)
	  {
	    int newTotalSuffixLen = enoughSpace(totalSuffixLen, additional);
	    this.suffix = new char[newTotalSuffixLen];
	    System.arraycopy(totalSuffix, totalSuffixLen - suffixLength,
			     this.suffix, newTotalSuffixLen - suffixLength,
			     suffixLength);
	    totalSuffixLen = newTotalSuffixLen;
	  }
	suffix.getChars(0, additional,
			totalSuffix, totalSuffixLen - newSuffixLen);
	blocks[blockDepth + BLOCK_SUFFIX_LENGTH] = newSuffixLen;
      }

  }

  int enqueueTab (int flags, int colnum, int colinc) // DONE
  {
    int addr = enqueue(QITEM_TAB_TYPE, QITEM_TAB_SIZE);
    queueInts[addr + QITEM_TAB_FLAGS] = flags;
    queueInts[addr + QITEM_TAB_COLNUM] = colnum;
    queueInts[addr + QITEM_TAB_COLINC] = colinc;
    return addr;
  }

  /** Calculate how much space to allocate for a buffer.
   * @param current the current size of the buffer
   * @param want how much more space is needed
   */
  private static int enoughSpace(int current, int want)
  {
    int doubled = 2 * current;
    int enough = current + ((5 * want) >> 2);
    return doubled > enough ? doubled : enough;
  }

  public void setIndentation (int column)
  {
    char[] prefix = this.prefix;
    int prefixLen = prefix.length;
    int current = getPrefixLength();
    int minimum = getPerLinePrefixEnd();
    if (minimum > column)
      column = minimum;
    if (column > prefixLen)
      {
	prefix = new char[enoughSpace(prefixLen, column - prefixLen)];
	System.arraycopy(this.prefix, 0, prefix, 0, current);
	this.prefix = prefix;
      }
    if (column > current)
      {
	for (int i = current;  i < column;  i++)
	  prefix[i] = ' ';
      }
    blocks[blockDepth + BLOCK_PREFIX_LENGTH] = column;
  }

  void reallyEndLogicalBlock ()
  {
    int oldIndent = getPrefixLength();
    blockDepth -= LOGICAL_BLOCK_LENGTH;  // Pop
    int newIndent = getPrefixLength();
    if (newIndent > oldIndent)
      {
	for (int i = oldIndent;  i < newIndent;  i++)
	  prefix[i] = ' ';
      }
  }

  public int enqueue (int kind, int size)
  {
    int oldLength = queueInts.length;
    int endAvail = oldLength - queueTail - queueSize;
    if (endAvail > 0 && size > endAvail)
      enqueue(QITEM_NOP_TYPE, endAvail);
    if (queueSize + size > oldLength)
      {
	int newLength = enoughSpace(oldLength, size);
	int[] newInts = new int[newLength];
	String[] newStrings = new String[newLength];
	int queueHead = queueTail + queueSize - oldLength;
	if (queueHead > 0)
	  { // Wraps around.
	    System.arraycopy(queueInts, 0, newInts, 0, queueHead);
	    System.arraycopy(queueStrings, 0, newStrings, 0, queueHead);
	  }
	int part1Len = oldLength - queueTail;
	int deltaLength = newLength - oldLength;
	System.arraycopy(queueInts, queueTail,
			 newInts, queueTail + deltaLength,
			 part1Len);
	System.arraycopy(queueStrings, queueTail,
			 newStrings, queueTail + deltaLength,
			 part1Len);
	queueInts = newInts;
	queueStrings = newStrings;
	if (currentBlock >= queueTail)
	  currentBlock += deltaLength;
	queueTail += deltaLength;
      }
    int addr = queueTail + queueSize;
    if (addr >= queueInts.length)
      addr -= queueInts.length;
    queueInts[addr + QITEM_TYPE_AND_SIZE] = kind | (size << 16);
    if (size > 1)
      queueInts[addr + QITEM_POSN] = indexPosn(bufferFillPointer);
    //log("enqueue "+itemKindString(kind)+" size:"+size+" at:"+queueSize+enqueueExtraLog); enqueueExtraLog = "";
    queueSize += size;
    return addr;
  }

  public void enqueueNewline (int kind)
  {
    wordEndSeen = false;
    int depth = pendingBlocksCount;
    //enqueueExtraLog = " kind:"+(char) kind;
    int newline = enqueue(QITEM_NEWLINE_TYPE, QITEM_NEWLINE_SIZE);
    queueInts[newline + QITEM_NEWLINE_KIND] = kind;
    queueInts[newline + QITEM_SECTION_START_DEPTH] = pendingBlocksCount;
    queueInts[newline + QITEM_SECTION_START_SECTION_END] = 0;
    int entry = queueTail;
    int todo = queueSize;
    while (todo > 0)
      {
	if (entry == queueInts.length)
	  entry = 0;
	if (entry == newline)
	  break;
	int type = getQueueType(entry);
	if ((type == QITEM_NEWLINE_TYPE
	     || type == QITEM_BLOCK_START_TYPE)
	    && queueInts[entry + QITEM_SECTION_START_SECTION_END] == 0
	    && depth <= queueInts[entry + QITEM_SECTION_START_DEPTH])
	  {
	    int delta = newline - entry;
	    if (delta < 0)
	      delta += queueInts.length;
	    queueInts[entry + QITEM_SECTION_START_SECTION_END] = delta;
	  }
	int size = getQueueSize(entry);
	todo -= size;
	entry += size;
      }
    maybeOutput (kind == NEWLINE_LITERAL || kind == NEWLINE_MANDATORY, false);
  }

  public final void writeBreak(int kind)
  {
    if (prettyPrintingMode > 0)
      enqueueNewline(kind);
  }

  public int enqueueIndent (char kind, int amount)
  {
    //enqueueExtraLog = " kind:"+kind+" amount:"+amount;
    int result = enqueue(QITEM_INDENTATION_TYPE, QITEM_INDENTATION_SIZE);
    queueInts[result + QITEM_INDENTATION_KIND] = kind;
    queueInts[result + QITEM_INDENTATION_AMOUNT] = amount;
    return result;
  }

  public void addIndentation(int amount, boolean current)
  {
    if (prettyPrintingMode > 0)
      enqueueIndent((current ? QITEM_INDENTATION_CURRENT
		     : QITEM_INDENTATION_BLOCK),
		    amount);
  }

  public void startLogicalBlock (String prefix, boolean perLine, String suffix)
  {
    // If the queue is empty, it is a good time to check if line-length etc
    // have been changed.
    if (queueSize == 0 && bufferFillPointer == 0)
      {
        Object llen = lineLengthLoc.get(null);
        if (llen == null)
          lineLength = 80;
        else
          lineLength = Integer.parseInt(llen.toString());

        Object mwidth = miserWidthLoc.get(null);
        if (mwidth == null || mwidth == Boolean.FALSE
            // For Common Lisp nil.  Should we use Language.isTrue() FIXME.
            || mwidth == LList.Empty)
          miserWidth = -1;
        else
          miserWidth = Integer.parseInt(mwidth.toString());

        Object indent = indentLoc.get(null);
        // if (indent == null || indent ...
      }
    if (prefix != null)
      write(prefix);
    if (prettyPrintingMode == 0)
      return;
    int start = enqueue (QITEM_BLOCK_START_TYPE,
			 QITEM_BLOCK_START_SIZE);
    queueInts[start + QITEM_SECTION_START_DEPTH] = pendingBlocksCount;
    queueStrings[start + QITEM_BLOCK_START_PREFIX]
      = perLine ? prefix : null;
    queueStrings[start + QITEM_BLOCK_START_SUFFIX] = suffix;
    pendingBlocksCount++;
    int outerBlock = currentBlock;
    if (outerBlock < 0)
      outerBlock = 0;
    else
      {
	outerBlock -= start;
	if (outerBlock > 0)
	  outerBlock -= queueInts.length;
      }
    queueInts[start + QITEM_BLOCK_START_BLOCK_END] = outerBlock;
    queueInts[start + QITEM_SECTION_START_SECTION_END] = 0;
    currentBlock = start;
  }

  public void endLogicalBlock ()
  {
    int end = enqueue (QITEM_BLOCK_END_TYPE, QITEM_BLOCK_END_SIZE);
    pendingBlocksCount--;
    if (currentBlock < 0)
      {
	// reallyStartLogicalBlock has been called for the matching
	// BEGIN_BLOCK, so it is no longer in the queue.  Instead it is in
	// the 'blocks' stack.
	int suffixLength = blocks[blockDepth+BLOCK_SUFFIX_LENGTH];
	int suffixPreviousLength
	  = blocks[blockDepth - LOGICAL_BLOCK_LENGTH + BLOCK_SUFFIX_LENGTH];
	if (suffixLength > suffixPreviousLength)
	  write(this.suffix,
		this.suffix.length - suffixLength,
		suffixLength - suffixPreviousLength);
	currentBlock = -1;
	return;
      }
    int start = currentBlock;
    int outerBlock = queueInts[start + QITEM_BLOCK_START_BLOCK_END];
    if (outerBlock == 0)
      currentBlock = -1;
    else
      {
	int qtailFromStart = queueTail - start;
	if (qtailFromStart > 0)
	  qtailFromStart -= queueInts.length;
	if (outerBlock < qtailFromStart)
	  {
	    // reallyStartLogicalBlock has been called for the outer block,
	    // so there is no currentBlock.
	    currentBlock = -1;
	  }
	else
	  {
	    // Make currentBlock absolute instead of relative.
	    outerBlock += start;
	    if (outerBlock < 0)
	      outerBlock += queueInts.length;
	    currentBlock = outerBlock;
	  }
      }
    String suffix = queueStrings[start + QITEM_BLOCK_START_SUFFIX];
    if (suffix != null)
      write(suffix);
    int endFromStart = end - start;
    if (endFromStart < 0) // wrap-around.
      endFromStart += queueInts.length;
    queueInts[start + QITEM_BLOCK_START_BLOCK_END] = endFromStart;
    //log("endLogicalBlock end:"+end+" start:"+start+" rel:"+endFromStart);
  }

  public void endLogicalBlock (String suffix)
  {
    if (prettyPrintingMode > 0)
      endLogicalBlock();
    else if (suffix != null)
      write(suffix);
  }

  // Tab support

  int computeTabSize (int tab, int sectionStart, int column) // DONE
  {
    int flags = queueInts[tab + QITEM_TAB_FLAGS];
    boolean isSection = (flags & QITEM_TAB_IS_SECTION) != 0;
    boolean isRelative = (flags & QITEM_TAB_IS_RELATIVE) != 0;
    int origin = isSection ? sectionStart : 0;
    int colnum = queueInts[tab + QITEM_TAB_COLNUM];
    int colinc = queueInts[tab + QITEM_TAB_COLINC];
    if (isRelative)
      {
	if (colinc > 1)
	  {
	    int newposn = column + colnum;
	    int rem = newposn % colinc;
	    if (rem != 0)
	      colnum += colinc = rem;
	  }
	return colnum;
      }
    else if (column <= colnum + origin)
      return column + origin - column;
    else
      return colinc - (column - origin) % colinc;
  }

  int indexColumn(int index)
  {
    int column = bufferStartColumn;
    int sectionStart = getSectionColumn();
    int endPosn = indexPosn(index);
    int op = queueTail;
    int todo = queueSize;
    while (todo > 0)
      {
	// If at end of queueInts, skip.
	if (op >= queueInts.length)
	  op = 0;
	int type = getQueueType(op);
	if (type != QITEM_NOP_TYPE)
	  {
	    int posn = queueInts[op + QITEM_POSN];
	    if (posn >= endPosn)
	      break;
	    if (type == QITEM_TAB_TYPE)
	      column += computeTabSize(op, sectionStart,
				       column + posnIndex (posn));
	    else if (type == QITEM_NEWLINE_TYPE
		     || type == QITEM_BLOCK_START_TYPE)
	      sectionStart
		= column + posnIndex(queueInts[op + QITEM_POSN]);
	  }
	int size = getQueueSize(op);
	todo -= size;
	op += size;
      }
    return column + index;
  }

  void expandTabs (int through)
  {
    int numInsertions = 0;
    int additional = 0;
    int column = bufferStartColumn;
    int sectionStart = getSectionColumn();
    int op = queueTail;
    int todo = queueSize;
    int blocksUsed = LOGICAL_BLOCK_LENGTH * pendingBlocksCount;
    while (todo > 0)
      {
	if (op == queueInts.length)
	  op = 0;
	if (op == through)
	  break;
	int type = getQueueType(op);
	if (type == QITEM_TAB_TYPE)
	  {
	    int index = posnIndex(queueInts[op + QITEM_POSN]);
	    int tabsize = computeTabSize (op, sectionStart, column + index);
	    if (tabsize != 0)
	      {
		// We use the blocks array for a temporary tab buffer.
		if (blocksUsed + 2 * numInsertions + 1 >= blocks.length)
		  {
		    int[] newBlocks = new int[2 * blocks.length];
		    System.arraycopy(blocks, 0, newBlocks, 0, blocks.length);
		    blocks = newBlocks;
		  }
		blocks[blocksUsed + 2 * numInsertions] = index;
		blocks[blocksUsed + 2 * numInsertions + 1] = tabsize;
		numInsertions++;
		additional += tabsize;
		column += tabsize;
	      }
	  }
	else if (op == QITEM_NEWLINE_TYPE || op == QITEM_BLOCK_START_TYPE)
	  {
	    sectionStart = column + posnIndex(queueInts[op + QITEM_POSN]);
	  }
	int size = getQueueSize(op);
	todo -= size;
	op += size;
      }
    if (numInsertions > 0)
      {
	int fillPtr = bufferFillPointer;
	int newFillPtr = fillPtr + additional;
	char[] buffer = this.buffer;
	char[] newBuffer = buffer;
	int length = buffer.length;
	int end = fillPtr;
	if (newFillPtr > length)
	  {
	    int newLength = enoughSpace (fillPtr, additional);
	    newBuffer = new char[newLength];
	    this.buffer = newBuffer;
	  }
	bufferFillPointer = newFillPtr;
	bufferOffset -= additional;
	for (int i = numInsertions;  --i >= 0; )
	  {
	    int srcpos = blocks[blocksUsed + 2 * i];
	    int amount = blocks[blocksUsed + 2 * i + 1];
	    int dstpos = srcpos + additional;
	    System.arraycopy(buffer, srcpos, newBuffer, dstpos, end - srcpos);
	    for (int j = dstpos - amount;  j < dstpos;  j++)
	      newBuffer[j] = ' ';
	    additional -= amount;
	    end = srcpos;
	  }
	if (newBuffer != buffer)
	  System.arraycopy(buffer, 0, newBuffer, 0, end);
      }
  }

  // stuff to do the actual outputting

  int ensureSpaceInBuffer (int want)
  {
    char[] buffer = this.buffer;
    int length = buffer.length;
    int fillPtr = bufferFillPointer;
    int available = length - fillPtr;
    if (available > 0)
      return available;
    else if (prettyPrintingMode > 0 && fillPtr > lineLength)
      {
	if (! maybeOutput(false, false))
	  outputPartialLine();
	return ensureSpaceInBuffer(want);
      }
    else
      {
	int newLength = enoughSpace(length, want);
	char[] newBuffer = new char[newLength];
	this.buffer = newBuffer;
	for (int i = fillPtr;  --i >= 0; )
	  newBuffer[i] = buffer[i];
	return newLength - fillPtr;
      }
  }

  boolean maybeOutput(boolean forceNewlines, boolean flushing)
  {
    boolean outputAnything = false;
    //log("maybeOutput("+forceNewlines+"):");  dumpQueue();
  loop:
    while (queueSize > 0)
      {
	if (queueTail >= queueInts.length)
	  queueTail = 0;
	int next = queueTail;
	int type = getQueueType(next);
	switch (type)
	  {
	  case QITEM_NEWLINE_TYPE:
	    boolean cond;
            int fits = -1;
	    switch (queueInts[next+QITEM_NEWLINE_KIND])
	      {
	      default: // LINEAR, LITERAL, or MANDATORY:
		cond = true;
		break;
	      case NEWLINE_MISER:
		cond = isMisering();
		break;
	      case NEWLINE_FILL:
		if (isMisering()
		    || (lineNumber > getSectionStartLine()))
		  {
		    cond = true;
		    break;
		  }
		/// ... fall through to ...
	      case NEWLINE_SPACE:
		int end = queueInts[next+QITEM_SECTION_START_SECTION_END];
		if (end == 0)
		  end = -1;
		else
		  { // convert relative->absolute.
		    end = next + end;
		    if (end >= queueInts.length)
		      end -= queueInts.length;
		  }
		fits = fitsOnLine(end, forceNewlines);
		if (fits > 0)
		  cond = false;
		else if (fits < 0 || flushing)
		  cond = true;
		else
		  break loop;
		break;
	      }
	    if (cond)
	      {
		outputAnything = true;
		try
		  {
                    if (flushing && fits == 0)
                      outputPartialLine();
                    else
                      outputLine(next);
		  }
		catch (IOException ex)
		  {
		    throw new RuntimeException(ex);
		  }
	      }
	    break;
	  case QITEM_INDENTATION_TYPE:
	    if (! isMisering())
	      {
		int kind = queueInts[next+QITEM_INDENTATION_KIND];
		int indent = queueInts[next+QITEM_INDENTATION_AMOUNT];
		if (kind == QITEM_INDENTATION_BLOCK)
		  indent += getStartColumn();
		else
		  indent += posnColumn(queueInts[next+QITEM_POSN]);
		//log("setIndent from "+next+": "+queueInts[next+QITEM_INDENTATION_AMOUNT]+" column:"+indent);
		setIndentation(indent);
	      }
	    break;
	  case QITEM_BLOCK_START_TYPE:
	    int start = next;
	    int end = queueInts[next + QITEM_SECTION_START_SECTION_END];
	    // Convert relative offset to absolute index:
	    end = end > 0 ? (end + next) % queueInts.length : -1;
	    fits = fitsOnLine (end, forceNewlines);
	    //log("block-start @"+next+" end:"+end+" force:"+forceNewlines+" fits:"+fits);
	    if (fits > 0)
	      {
		// Just nuke the whole logical block and make it look
		// like one nice long literal.
		int endr = queueInts[next + QITEM_BLOCK_START_BLOCK_END];
		// make absolute:
		next = (endr + next) % queueInts.length;
		expandTabs(next);
		queueTail = next;
		queueSize -= endr;
		//log("remove block -> next:"+next+" endr:"+endr+" qSize:"+queueSize);
	      }
	    else if (fits < 0 || flushing)
	      {
		String prefix = queueStrings[next + QITEM_BLOCK_START_PREFIX];
		String suffix = queueStrings[next + QITEM_BLOCK_START_SUFFIX];
		//log("reallyStartLogicalBlock: "+blockDepth+" at:"+next);
		reallyStartLogicalBlock (posnColumn(queueInts[next + QITEM_POSN]),
					 prefix, suffix);
	      }
	    else // Don't know.
	      break loop;
	    if (currentBlock == start)
	      currentBlock = -1;
	    break;
	  case QITEM_BLOCK_END_TYPE:
	    //log("reallyEndLogicalBlock: "+blockDepth+" at:"+next);
	    reallyEndLogicalBlock();
	    break;
	  case QITEM_TAB_TYPE:
	    expandTabs(next);
	    break;
	  }
	int size = getQueueSize(queueTail);
	queueSize -= size;
	//log("maybeWrite size: "+size+" ->"+queueSize);
	queueTail = next + size;
      }
    return outputAnything;
  }

  protected int getMiserWidth () // DONE
  {
    // CommonLisp:  Use *print-miser-width*.
    return miserWidth;
  }

  boolean isMisering() // DONE
  {
    int mwidth = getMiserWidth ();
    return (mwidth > 0
	    && lineLength - getStartColumn() <= mwidth);
  }

  int getMaxLines ()
  {
    // Should be value of CommonLisp *print-lines*.
    return -1;
  }

  boolean printReadably()
  {
    // Should be value of CommonLisp *print-readably*.
    return true;
  }

  /** Return 1 if true;  -1 if false; 0 if don't know. */
  int fitsOnLine (int sectionEnd, boolean forceNewlines) // DONE
  {
    int available = lineLength;
    if (! printReadably() && getMaxLines() == lineNumber)
      {
	available -= 3;  // For the " ..".
	available -= getSuffixLength();
      }
    if (sectionEnd >= 0)
      return posnColumn(queueInts[sectionEnd + QITEM_POSN]) <= available ? 1 : -1;
    if (forceNewlines)
      return -1;
    if (indexColumn(bufferFillPointer) > available)
      return -1;
    return 0; // don't know.
  }

  public void lineAbbreviationHappened()
  {
    // Hook.
  }

  /** Output a new line.
   * @param newline index of a newline queue item
   */
  void outputLine (int newline)  throws IOException
  {
    char[] buffer = this.buffer;
    int kind = queueInts[newline + QITEM_NEWLINE_KIND];
    boolean isLiteral = kind == NEWLINE_LITERAL;
    int amountToConsume = posnIndex(queueInts[newline + QITEM_POSN]);
    int amountToPrint;
    if (isLiteral)
      amountToPrint = amountToConsume;
    else
      {
	// Skip trailing spaces.
	for (int i = amountToConsume; ; )
	  {
	    if (--i < 0)
	      {
		amountToPrint = 0;
		break;
	      }
	    if (buffer[i] != ' ')
	      {
		amountToPrint = i + 1;
		break;
	      }
	  }
      }
    out.write(buffer, 0, amountToPrint);
    int lineNumber = this.lineNumber;
    //log("outputLine#"+lineNumber+": \""+new String(buffer, 0, amountToPrint)+"\" curBlock:"+currentBlock);
    lineNumber++;
    if (! printReadably())
      {
	int maxLines = getMaxLines();
	if (maxLines > 0 && lineNumber >= maxLines)
	  {
	    out.write(" ..");
	    int suffixLength = getSuffixLength();
	    if (suffixLength != 0)
	      {
		char[] suffix = this.suffix;
		int len = suffix.length;
		out.write(suffix, len - suffixLength, suffixLength);
	      }
	    // (throw 'line-limit-abbreviation-happened t))
	    lineAbbreviationHappened();
	  }
      }
    this.lineNumber = lineNumber;
    out.write('\n');
    bufferStartColumn = 0;
    int fillPtr = bufferFillPointer;
    int prefixLen = isLiteral ? getPerLinePrefixEnd() : getPrefixLength();
    int shift = amountToConsume - prefixLen;
    int newFillPtr = fillPtr - shift;
    char[] newBuffer = buffer;
    int bufferLength = buffer.length;
    if (newFillPtr > bufferLength)
      {
	newBuffer = new char[enoughSpace(bufferLength,
					 newFillPtr - bufferLength)];
	this.buffer = newBuffer;
      }
    System.arraycopy(buffer, amountToConsume, newBuffer, prefixLen,
		     fillPtr - amountToConsume);
    System.arraycopy(prefix, 0, newBuffer, 0, prefixLen);
    bufferFillPointer = newFillPtr;
    bufferOffset += shift;
    if (! isLiteral)
      {
	blocks[blockDepth+BLOCK_SECTION_COLUMN] = prefixLen;
	blocks[blockDepth+BLOCK_SECTION_START_LINE] = lineNumber;
      }
  }

  void outputPartialLine ()
  {
    //log("outputPartialLine");
    int tail = queueTail;
    while (queueSize > 0 && getQueueType(tail) == QITEM_NOP_TYPE)
      {
	int size = getQueueSize(tail);
	queueSize -= size;
	tail += size;
	if (tail == queueInts.length)
	  tail = 0;
	queueTail = tail;
      }
    int fillPtr = bufferFillPointer;
    int count = queueSize > 0 ? posnIndex (queueInts[tail + QITEM_POSN])
      : fillPtr;
    int newFillPtr = fillPtr - count;
    if (count <= 0)
      throw new Error("outputPartialLine called when nothing can be output.");
    try
      {
	out.write(buffer, 0, count);
	//log("outputPartial: \""+new String(buffer, 0, count)+'\"');
      }
    catch (IOException ex)
      {
	throw new RuntimeException(ex);
      }
    bufferFillPointer = count; // For the sake of the following:
    bufferStartColumn = getColumnNumber();
    System.arraycopy(buffer, count, buffer, 0, newFillPtr);
    bufferFillPointer = newFillPtr;
    bufferOffset += count;
  }

  public void forcePrettyOutput () throws IOException
  {
    maybeOutput(false, true);
    if (bufferFillPointer > 0)
      outputPartialLine();
    expandTabs(-1);
    bufferStartColumn = getColumnNumber();
    out.write(buffer, 0, bufferFillPointer);
    bufferFillPointer = 0;
    queueSize = 0;
  }

  public void flush()
  {
    if (out == null)
      return;
    try
      {
	forcePrettyOutput();
	out.flush();
      }
    catch (IOException ex)
      {
	throw new RuntimeException(ex.toString());
      }
  }

  public void close()  throws IOException
  {
    if (out != null)
      { 
	forcePrettyOutput();
        out.close();
        out = null;
      }
    buffer = null;
  }

  /** Flush and close this local Writer, but not underlying Writers. */
  public void closeThis()  throws IOException
  {
    if (out != null)
      { 
	forcePrettyOutput();
        out = null;
      }
    buffer = null;
  }

  /** Not meaningful if {@code prettyPrintingMode > 0}. */
  public int getColumnNumber ()
  {
    int i = bufferFillPointer;
    for (;;)
      {
	if (--i < 0)
	  return bufferStartColumn + bufferFillPointer;
	char ch = buffer[i];
	if (ch == '\n' || ch == '\r')
	  return bufferFillPointer - (i+1);
      }
  }

  public void setColumnNumber (int column)
  {
    bufferStartColumn += column - getColumnNumber ();
  }

  public void clearBuffer ()
  {
    bufferStartColumn = 0;
    bufferFillPointer = 0;
    lineNumber = 0;
    bufferOffset = 0;
    blockDepth = LOGICAL_BLOCK_LENGTH;
    queueTail = 0;
    queueSize = 0;
    pendingBlocksCount = 0;
  }

  /*
  public static PrintWriter log;
  static {
    try { log = new PrintWriter(new FileOutputStream("/tmp/pplog")); }
    catch (Throwable ex) { ex.printStackTrace(); }
  }
  void log(String str)
  {
    log.println(str);
    log.flush();
  }
  void dumpQueue()
  {
    log.println("Queue tail:"+queueTail+" size:"+queueSize
		+" length:"+queueInts.length+" startCol:"+bufferStartColumn);
    dumpQueue(queueTail, queueSize, log);
  }

  void dumpQueue(int start, int todo, PrintWriter out)
  {
    int bufIndex = 0;
    while (todo > 0)
      {
	if (start == queueInts.length)
	  start = 0;
	if (start < 0 || start >= queueInts.length)
	  {
	    out.print('@');	out.print(start);  out.print(": ");
	    out.print("out of bounds - queueInts length is ");
	    out.println(queueInts.length);
	    break;
	  }
	int type = getQueueType(start);
	int size = getQueueSize(start);
	if (type != QITEM_NOP_TYPE)
	  {
	    int newIndex = posnIndex(queueInts[start+QITEM_POSN]);
	    int count = newIndex - bufIndex;
	    if (count > 0)
	      {
		out.print(count); out.print(" chars: \"");
		out.write(buffer, bufIndex, count);
		out.println('\"');
		bufIndex = newIndex;
	      }
	  }
	out.print('@');	out.print(start);  out.print(": ");
	out.print("type:");  out.print(type);
	switch (type)
	  {
	  case QITEM_NEWLINE_TYPE:
	    out.print("(newline)");  break;
	  case QITEM_INDENTATION_TYPE:
	    out.print("(indentation)");  break;
	  case QITEM_BLOCK_START_TYPE:
	    out.print("(block-start)");  break;
	  case QITEM_BLOCK_END_TYPE:
	    out.print("(block-end)");  break;
	  case QITEM_TAB_TYPE:  out.print("(tab)");
	    break;
	  case QITEM_NOP_TYPE:
	    out.print("(nop)");  break;
	  }
	out.print(" size:");  out.print(size);
	out.print(";  @");  out.print(start+QITEM_POSN);
	if (type != QITEM_NOP_TYPE)
	  {
	    out.print(": posn:");
	    int posn = queueInts[start+QITEM_POSN];
	    out.print(posn);
	    out.print(" index:");
	    out.println(posnIndex(posn));
	  }
	if (type == QITEM_NEWLINE_TYPE
	    || type == QITEM_BLOCK_START_TYPE)
	    
	  {
	    out.print('@');  out.print(start+QITEM_SECTION_START_DEPTH);
	    out.print(": - depth:");
	    out.print(queueInts[start+QITEM_SECTION_START_DEPTH]);
	    out.print(";  @");
	    out.print(start+QITEM_SECTION_START_SECTION_END);
	    out.print(": section-end:");
	    out.println(queueInts[start+QITEM_SECTION_START_SECTION_END]);
	  }
	switch (type)
	  {
	  case QITEM_BLOCK_START_TYPE:
	    printQueueWord(start, QITEM_BLOCK_START_BLOCK_END, "block-end", out);
	    printQueueStringWord(start, QITEM_BLOCK_START_PREFIX, "prefix", out);
	    printQueueStringWord(start, QITEM_BLOCK_START_SUFFIX, "suffix", out);
	    break;
	  case QITEM_NEWLINE_TYPE:
	    out.print('@');
	    out.print(start+QITEM_NEWLINE_KIND);
	    out.print(": - kind: ");
	    int kind = queueInts[start+QITEM_NEWLINE_KIND];
	    String skind = "???";
	    switch (kind)
	      {
	      case NEWLINE_LINEAR:    skind = "linear";    break;
	      case NEWLINE_LITERAL:   skind = "literal";   break;
	      case NEWLINE_FILL:      skind = "fill";      break;
	      case NEWLINE_SPACE:     skind = "space";      break;
	      case NEWLINE_MISER:     skind = "miser";     break;
	      case NEWLINE_MANDATORY: skind = "mandatory"; break;
	      }
	    out.print(kind);
	    out.print('(');
	    out.print(skind);
	    out.println(')');
	    break;
          case QITEM_INDENTATION_TYPE:
	    printQueueWord(start, QITEM_INDENTATION_KIND, "kind", out);
	    printQueueWord(start, QITEM_INDENTATION_AMOUNT, "amount", out);
	    break;
	  default:
	    for (int i = 2;  i < size;  i++)
	      printQueueWord(start, i, "word#"+i, out);
	  }
	todo -= size;
	start += size;
      }
    int count = bufferFillPointer - bufIndex;
    if (count > 0)
      {
	out.print(count); out.print(" chars: \"");
	out.write(buffer, bufIndex, count);
	out.println('\"');
      }
  }

  private void printQueueWord(int start, int offset,
			      String fname, PrintWriter out)
  {
    out.print('@');
    out.print(start+offset);
    out.print(": - ");
    out.print(fname);
    out.print(": ");
    out.println(queueInts[start+offset]);
  }

  private void printQueueStringWord(int start, int offset,
				    String fname, PrintWriter out)
  {
    out.print('@');
    out.print(start+offset);
    out.print(": - ");
    out.print(fname);
    out.print(": ");
    String str = queueStrings[start+offset];
    if (str == null)
      out.println("null");
    else
      {
	out.print('\"');
	out.print(str);
	out.print('\"');
	out.print(" length: ");
	out.println(str.length());
      }
  }

  void check (String where)
  {
    String msg = null;
    if (currentBlock != -1
	&& ! (currentBlock < queueInts.length
	      && currentBlock >= queueTail
	      ? currentBlock < queueTail + queueSize
	      : currentBlock < queueTail + queueSize - queueInts.length))
      msg = ("currentBlock ("+currentBlock
	     +") >= queue length ("+queueInts.length+")");
    if (msg != null)
      {
	if (where != null)
	  msg = "in "+where+": "+msg;
	log.println(msg);
	dumpQueue(); 
	log.flush();
	throw new Error(msg);
      }
  }

  String itemKindString (int kind)
  {
    switch (kind)
      {
      case QITEM_NOP_TYPE:  return "nop";
      case QITEM_NEWLINE_TYPE:  return "newline";
      case QITEM_INDENTATION_TYPE:  return "indentation";
      case QITEM_BLOCK_START_TYPE:  return "block-start";
      case QITEM_BLOCK_END_TYPE:  return "block-end";
      case QITEM_TAB_TYPE:  return "tab";
      default: return "("+kind+" - unknown)";
      }
  }
  String enqueueExtraLog = "";
  */
}
