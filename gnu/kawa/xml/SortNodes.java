// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Sort argument nodes in document order.
 * Implements the standard XQuery function distinct-nodes.
 * Uses the SortedNodes class to do the actual work. */

public class SortNodes extends Procedure1 implements Inlineable
{
  public static final SortNodes sortNodes = new SortNodes();

  public int numArgs() { return 0x1001; }

  public Object apply1 (Object values)
  {
    SortedNodes nodes = new SortedNodes();
    Values.writeValues(values, nodes);
    return nodes;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    ConsumerTarget.compileUsingConsumer(exp, comp, target,
					makeSortedNodesMethod, null);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final ClassType typeSortedNodes
    = ClassType.make("gnu.kawa.xml.SortedNodes");
  static final Method makeSortedNodesMethod
    = typeSortedNodes.getDeclaredMethod("<init>", 0);

}
