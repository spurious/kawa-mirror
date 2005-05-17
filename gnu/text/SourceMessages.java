// Copyright (c) 1999, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;

/** A collection of (zero or more) SourceErrors.
 * Has a "current line number" which clients can use as the default line
 * number, or clients can explicitly provide a line number.
 * Does not handle localization of messages.
 */

public class SourceMessages
{
  // Number of errors (not counting warnings).  A value of 1000 is "fatal".
  private int errorCount = 0;

  /** The first error or warning in a linked list. */
  SourceError firstError;
  /** The last error or warning in a linked list. */
  SourceError lastError;

  public SourceError getErrors() { return firstError; }

  String current_filename;
  int current_line;
  int current_column;

  /** Return true iff errors (not warnings) have been seen. */
  public boolean seenErrors() { return errorCount > 0; }

  /** Get the number of errors (not counting warnings). */
  public int getErrorCount() { return errorCount; }

  /** Clear the error count (only). */
  public void clearErrors() { errorCount = 0; }

  /** Clear the contained errors and warnings. */
  public void clear()
  {
    firstError = lastError = null;
    errorCount = 0;
  }

  // The last SourceError with a *differnt* filename than prev has.
  SourceError lastPrevFilename = null;

  /** True if we should sort messages by line number. */
  public boolean sortMessages;

  /** Link in an error. */
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
    if (! sortMessages || error.severity == 'f')
      prev = lastError;
    else
      {
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

  /** Record a new error.
   * @param severity is the seriousness of the error
   *  - one of 'w' (for warning), 'e' (for error), or 'f' (for fatal error)
   * @param filename the name or URL of the file containing the error
   * @param line the (1-origin) line number or 0 if unknown
   * @param column the (1-origin) column number or 0 if unknown
   * @param message the error message
   */
  public void error(char severity, String filename, int line, int column,
		    String message)
  {
    error(new SourceError(severity, filename, line, column, message));
  }

  /** Record a new error at the current default source file location.
   * @param severity is the seriousness of the error
   *  - one of 'w' (for warning), 'e' (for error), or 'f' (for fatal error)
   * @param message the error message
   */
  public void error(char severity, String message)
  {
    error(new SourceError(severity, current_filename,
			  current_line, current_column, message));
  }

  /** Print all the error messages to a PrintStream. */
  public void printAll(java.io.PrintStream out, int max)
  {
    for (SourceError err = firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	err.println(out);
      }
  }

  /** Print all the error messages to a PrintWriter. */
  public void printAll(java.io.PrintWriter out, int max)
  {
    for (SourceError err = firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	err.println(out);
      }
  }

  /** Convert this to a String containing the recorded errors.
   * @param max the maximum number of error error to list
   * @return a String with one '\n'-terminated line per recorded error
   */
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

  /** Checks if an error was seen; if so, prints and clears the messages.
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0)
   */
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

  /** Checks if an error was seen; if so, prints and clears the messages.
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

  /** The default filename to use for a new error. */
  public final String getFile() { return current_filename; }
  /** The default line number to use for a new error. */
  public final int getLine() { return current_line; }
  /** The default column number to use for a new error. */
  public final int getColumn() { return current_column; }

  /** Set the default filename to use for a new error. */
  public void setFile(String filename) { current_filename = filename; }
  /** Set the default line number to use for a new error. */
  public void setLine(int line) { current_line = line; }
  /** Set the default column number to use for a new error. */
  public void setColumn(int column) { current_column = column; }

  /** Set the default filename, line and column to use for a new error. */
  public void setLine(String filename, int line, int column)
  {
    current_filename = filename;
    current_line = line;
    current_column = column;
  }

}
