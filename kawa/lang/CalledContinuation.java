package kawa.lang;

public class CalledContinuation extends RuntimeException
{
  public Object[] values;
  public Continuation continuation;

  CalledContinuation (Object[] values, Continuation continuation)
  {
    super ("call/cc called");
    this.values = values;
    this.continuation = continuation;
  }
}
