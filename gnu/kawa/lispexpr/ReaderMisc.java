// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;

public class ReaderMisc extends ReadTableEntry
{
  int kind;

  public ReaderMisc (int kind) { this.kind = kind; }

  public int getKind() { return kind; }
}
