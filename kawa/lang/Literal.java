package kawa.lang;

/** A Literal contains compile-time information about a constant. */

public class Literal
{
  Object value;
  int index;
  Literal next;

  public Literal (Object value, Compilation comp)
  {
    this.value = value;
    this.index = comp.literalsCount++;
    comp.literalTable.put (value, this);
    next = comp.literalsChain;
    comp.literalsChain = this;
  }

  void compile (Compilation comp)
  {
    comp.method.compile_getstatic (comp.literalsField);
    comp.method.compile_push_int (index);
    comp.method.compile_array_load (comp.scmObjectType);
  }
}

