package kawa.lang;

public class CalledContinuation extends RuntimeException
{
  Object value;
  Continuation continuation;

  CalledContinuation (Object value, Continuation continuation)
  {
    super ("call/cc called");
    this.value = value;
    this.continuation = continuation;
  }
}
