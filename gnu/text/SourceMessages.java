// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;

/** A collection of (zero or more) SourceErrors. */

public class SourceMessages
{
  // Number of errors (not counting warnings).  A value of 1000 is "fatal".
  int errorCount;

  SourceError firstError;
  SourceError lastError;

  public SourceError getErrors() { return firstError; }

  /** Return true iff errors (not warnings) have been seen. */
  public boolean seenErrors() { return errorCount > 0; }

  /** Get the number of errors (not counting warnings). */
  public int getErrorCount() { return errorCount; }

  public void clearErrors() { errorCount = 0; }

  // The last SourceError with a *differnt* filename than prev has.
  SourceError lastPrevFilename = null;

  public void error(SourceError error)
  {
    if (error.severity == 'f')
      errorCount = 1000;
    else if (error.severity != 'w')
      errorCount++;

    // Insert the next error so that line numbers are increasing.
    if (lastError != null && lastError.filename != null
	&& ! lastError.filename.equals(error.filename))
      lastPrevFilename = lastError;
    SourceError prev = lastPrevFilename;
    for (;;)
      {
	SourceError next;
	if (prev == null)
	  next = firstError;
	else
	  next = prev.next;
	if (next == null)
	  break;
	if (error.line != 0 && next.line != 0)
	  {
	    if (error.line < next.line)
	      break;
	    if (error.line == next.line
		&& error.column != 0 && next.column != 0)
	      {
		if (error.column < next .column)
		  break;
	      }
	  }
	prev = next;
      }
    if (prev == null)
      {
	error.next = firstError;
	firstError = error;
      }
    else
      {
	error.next = prev.next;
	prev.next = error;
      }
    if (prev == lastError) 
      lastError = error;
  }

  public void error(char severity, String filename, int line, int column,
		    String message)
  {
    error(new SourceError(severity, filename, line, column, message));
  }

  void printAll(java.io.PrintStream out, int max)
  {
    for (SourceError err = firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	out.println(err);
      }
  }

  void printAll(java.io.PrintWriter out, int max)
  {
    for (SourceError err = firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	out.println(err);
      }
  }

  public String toString(int max)
  {
    if (firstError == null)
      return null;
    StringBuffer buffer = new StringBuffer ();
    for (SourceError err = firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	buffer.append(err);
	buffer.append('\n');
      }
    return buffer.toString();
  }

  /** Returns true if an error was seen.  Prints and clears the messages.
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0) */
  public boolean checkErrors(java.io.PrintWriter out, int max)
  {
    if (firstError != null)
      {
	printAll(out, max);
	firstError = lastError = null;
        int saveCount = errorCount;
	errorCount = 0;
	return saveCount > 0;
      }
    return false;
  }

  /** Returns true if an error was seen.  Prints and clears the messages
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0) */
  public boolean checkErrors(java.io.PrintStream out, int max)
  {
    if (firstError != null)
      {
	printAll(out, max);
	firstError = lastError = null;
        int saveCount = errorCount;
	errorCount = 0;
	return saveCount > 0;
      }
    return false;
  }

}
