package gnu.expr;
import gnu.bytecode.*;

/** A piece of code that needs to be added to <clinit>. */

public abstract class Initializer
{
  Initializer next;

  /** If non-null:  The Field that is being initialized. */
  public Field field;

  public abstract void emit(Compilation comp);
}
