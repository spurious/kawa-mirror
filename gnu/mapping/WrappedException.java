// Copyright (c) 1999, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** Encapsulate some Exception inside a RuntimeException.
 * Inspired by org.xml.sax.SAXException written by David Megginson.
 */

public class WrappedException extends RuntimeException
{
  /**
   * Create a new WrappedException.
   */
  public WrappedException ()
  {
  }

  /**
   * Create a new WrappedException.
   *
   * @param message The error or warning message.
   */
  public WrappedException (String message)
  {
    this.message = message;
  }

  /**
   * Create a new WrappedException wrapping an existing exception.
   *
   * <p>The existing exception will be embedded in the new
   * one, and its message will become the default message for
   * the WrappedException.</p>
   *
   * @param e The exception to be wrapped in a WrappedException.
   */
  public WrappedException (Throwable e)
  {
    super();
    this.message = null;
    initCause(e);
  }

  /**
   * Create a new WrappedException from an existing exception.
   *
   * <p>The existing exception will be embedded in the new
   * one, but the new exception will have its own message.</p>
   *
   * @param message The detail message.
   * @param e The exception to be wrapped in a WrappedException.
   */
  public WrappedException (String message, Throwable e)
  {
    super();
    this.message = message;
    initCause(e);
  }

  /**
   * Return a detail message for this exception.
   *
   * <p>If there is a embedded exception, and if the WrappedException
   * has no detail message of its own, this method will return
   * the detail message from the embedded exception.</p>
   *
   * @return The error or warning message.
   */
  public String getMessage ()
  {
    Throwable cause;
    if (message == null && (cause = getCause()) != null)
      return cause.getMessage();
    else
      return this.message;
  }

  /**
   * Return the embedded exception, if any.
   *
   * @return The embedded exception, or null if there is none.
   */
  public Throwable getException ()
  {
    return getCause();
  }

  /**
   * Convert this exception to a string.
   *
   * @return A string version of this exception.
   */
  public String toString ()
  {
    return getMessage();
  }

  /* The initCause/getCause functionality was added in JDK 1.4.
     It is available in gcj 3.3, so we could perhaps put it in a JAVA1 block,
     but I'm not quite ready for that yet.
     Future: BEGIN a JAVA1 section */
  public Throwable initCause(Throwable cause)
  {
    exception = cause;
    return this;
  }

  public Throwable getCause()
  {
    return exception;
  }

  private Throwable exception;
  /* Future: END a JAVA1 section. */
  private String message;
}
