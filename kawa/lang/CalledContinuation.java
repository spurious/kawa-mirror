package kawa.lang;

public class CalledContinuation extends RuntimeException
{
  public Object value;
  public Continuation continuation;

  CalledContinuation (Object value, Continuation continuation)
  {
    super ("call/cc called");
    this.value = value;
    this.continuation = continuation;
  }
}
