package gnu.lists;

/**
 * A Sequence of values referenced indirectly as tree positions.
 * I.e. each element in this Sequence is a pointer to a position
 * in some other sequence or tree.
 * Conceptually similar to org.www.dom.NodeList.
 */

public class TreePositionList extends AbstractSequence
implements Sequence, PositionConsumer, PositionContainer
{
  /** Offsets into sposes, iposes, xposes.
   * Element i of this uses elements offsets[i] ... offsets[i+1]-1
   * in sposes, iposes, xposes. */
  int[] offsets;
  int size;

  AbstractSequence[] sposes;
  int[] iposes;
  Object[] xposes;
  int used;

  public TreePositionList()
  {
    sposes = new AbstractSequence[20];
    iposes = new int[20];
    xposes = new Object[20];
    offsets = new int[10];
  }

  public int size()
  {
    return size;
  }

  protected void
  makePosition(int index, boolean isAfter,
	       PositionContainer posSet, int posNumber)
  {
    posSet.setPosition(posNumber, (index << 1) | (isAfter ? 1 : 0), null);
    posSet.setSequence(posNumber, this);
  }

  /*
  public Object get (int index)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException();
    int last = offsets[index+1] - 1;
    AbstractSequence seq = sposes[last];
    Object xpos = xposes[last];
    if (seq == null)
      return xpos;
    return seq.getNext(iposes[last], xpos);
  }
  */

  public Object get(int index)
  {
    TreePosition pos = new TreePosition();
    get(index, pos);
    return pos;
  }

  public void get(int index, TreePosition pos)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    pos.clear();
    int offset0 = offsets[index];
    int offset1 = offsets[index+1];
    int depth = offset1 - offset0;
    if (depth == 1 && sposes[offset0] == null)
      {
	pos.xpos = xposes[offset0];
      }
    else
      {
	for (int i = offset0;  i < offset1;  i++)
	  pos.push(sposes[i], iposes[i], xposes[i]);
      }
  }

  /**
   * Append a new position to the sequence.
   */
  public boolean consume(TreePosition position)
  {
    int depth = position.getDepth();
    if (used + depth >= xposes.length)
      {
	int s = 2 * xposes.length;
	if (used + depth > s)
	  s = used + depth;
	AbstractSequence[] stmp = new AbstractSequence[s];
	int[] itmp = new int[s];
	Object[] xtmp = new Object[s];
	System.arraycopy(sposes, 0, stmp, 0, used);
	System.arraycopy(iposes, 0, itmp, 0, used);
	System.arraycopy(xposes, 0, xtmp, 0, used);
	sposes = stmp;
	iposes = itmp;
	xposes = xtmp;
      }
    AbstractSequence seq;
    for (int i = 0;  i < depth-1;  i++)
      {
	seq = position.sstack[i];
	seq.copyPosition(position.istack[i], position.xstack[i], this, used);
	used++;
      }
    seq = position.sequence;
    if (depth == 0)
      xposes[used] = position.xpos;
    else
      seq.copyPosition(position.ipos, position.xpos, this, used);
    used++;

    size++;
    if (size >= offsets.length)
      {
	int[] otmp = new int[2 * size];
	System.arraycopy(offsets, 0, otmp, 0, size);
	offsets = otmp;
      }
    offsets[size] = used;
    return true;
  }

  /** See java.util.List. */
  public boolean add (Object o)
  {
    consume((TreePosition) o);
    return true;
  }

  public void getPosition(int index, boolean isAfter,
			  PositionContainer posSet, int posNumber)
  {
    posSet.setPosition(posNumber, index, null);
  }

  protected int nextIndex(int ipos, Object xpos)
  {
    return ipos;
  }

  public int getPositionInt(int positionNumber)
  { return iposes[positionNumber]; }

  /** Get the Object part of a specified position pair. */
  public Object getPositionPtr(int positionNumber)
  { return xposes[positionNumber]; }

  public void setPosition(int positionNumber, int ipos, Object xpos)
  {
    iposes[positionNumber] = ipos;
    xposes[positionNumber] = xpos;
  }

  public void setSequence(int positionNumber, AbstractSequence seq)
  { sposes[positionNumber] = seq; }

  /** Get the number of positions pairs stored in this PositionContainer. */
  public int countPositions() { return used; }

  public void finalize()
  {
    for (int i = used;  --i >= 0; )
      {
	AbstractSequence seq = sposes[i];
	if (seq != null)
	  seq.releasePosition(iposes[i], xposes[i]);
      }
    sposes = null;
    iposes = null;
    xposes = null;
    offsets = null;
    used = 0;
    size = 0;
  }
}
