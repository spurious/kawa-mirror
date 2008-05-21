package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.jemacs.buffer.*;
import gnu.kawa.reflect.Invoke;

public class SaveExcursion extends Syntax
{
  boolean bufferOnly;

  public static ClassType typeSaveExcursion
    = ClassType.make("gnu.jemacs.lang.SaveExcursion");
  public static ClassType typeBuffer
    = ClassType.make("gnu.jemacs.buffer.Buffer");

  public SaveExcursion(boolean bufferOnly)
  {
    this.bufferOnly = bufferOnly;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Expression[] inits1 = new Expression[1];
    inits1[0] = Invoke.makeInvokeStatic(typeBuffer, "getCurrent",
					Expression.noExpressions);
    LetExp let1 = new LetExp(inits1);
    Declaration savedBuffer = let1.addDeclaration(null, typeBuffer);
    savedBuffer.noteValue(inits1[0]);
    Declaration savedPointMark;
    LetExp let2;
    tr.push(let1);
    if (bufferOnly)
      {
	savedPointMark = null;
	let2 = let1;
      }
    else
      {
	Expression[] inits2 = new Expression[1];
	let2 = new LetExp(inits2);
	savedPointMark = let2.addDeclaration(null, Type.longType);
	Expression[] args = new Expression[1];
	args[0] = new ReferenceExp(savedBuffer);
	inits2[0] = Invoke.makeInvokeStatic(typeSaveExcursion,
					   "savePointMark", args);
	savedBuffer.noteValue(inits2[0]);
	tr.push(let2);
      }
    Expression body = tr.rewrite_body(obj);
    Expression finalizer;
    if (bufferOnly)
      {
	Expression[] args = new Expression[1];
	args[0] = new ReferenceExp(savedBuffer);
	finalizer = Invoke.makeInvokeStatic(typeBuffer, "setBuffer", args);
      }
    else
      {
	tr.pop(let2);
	let1.body = let2;
	Expression[] args = new Expression[2];
	args[0] = new ReferenceExp(savedBuffer);
	args[1] = new ReferenceExp(savedPointMark);
	finalizer = Invoke.makeInvokeStatic(typeSaveExcursion,
					   "restoreBufferPointMark", args);
      }
    tr.pop(let1);
    let2.body = new TryExp(body, finalizer);
    return let1;
  }

  /** Save point and (in the future) mark of a buffer.
   * Returns a pair (packed in a long) of buffer posistions. */
  public static long savePointMark(Buffer buffer)
  {
    return buffer.savePointMark();
  }

  public static void restoreBufferPointMark(Buffer buffer, long pointMark)
  {
    Buffer.setCurrent(buffer);
    buffer.restorePointMark(pointMark);
  }
}
