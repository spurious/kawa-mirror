// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.util.Hashtable;

/** Mananges a table of named options,
 * Can inherit from another table of "default" options. */

public class Options
{
  /** Bit indicating option value is a boolean. */
  public static final int BOOLEAN_OPTION = 1;

  public static final int STRING_OPTION = 2;

  /** The option table contain defaults, that we "inherit" from. */
  Options previous;

  OptionInfo first;
  OptionInfo last;

  public Options ()
  {
  }

  public Options (Options previous)
  {
    this.previous = previous;
  }

  /** Maps property keys to options values. */
  Hashtable valueTable = new Hashtable();

  /** Maps property keys to OptionInfo. */
  Hashtable infoTable = new Hashtable();

  /** Create a new option and enters it in this table.
   * A duplicate option throws a RuntimeException.
   * @param key the options name (key).
   * @param kind type and other flag bits of the option.
   * @param documentation a String describing what the option does. */
  public void add(String key, int kind, String documentation)
  {
    Object old = (OptionInfo) infoTable.get(key);
    if (old != null)
      throw new RuntimeException("duplicate option key: "+key);
    OptionInfo info = new OptionInfo();
    info.key = key;
    info.kind = kind;
    info.documentation = documentation;
    if (first == null)
      first = info;
    else
      last.next = info;
    last = info;
    infoTable.put(key, info);
  }

  static Object valueOf (OptionInfo info, String argument)
  {
    if ((info.kind & BOOLEAN_OPTION) != 0)
      {
    	if (argument == null
	    || argument.equals("1")
	    || argument.equals("yes")
	    || argument.equals("true"))
	  return Boolean.TRUE;
	if (argument.equals("0")
	    || argument.equals("no")
	    || argument.equals("false"))
	  return Boolean.FALSE;
	return null;
      }
    return argument;
  }

  private void error(String message, SourceMessages messages)
  {
    if (messages == null)
      throw new RuntimeException(message);
    else
      messages.error('e', message);
  }

  /** Set the value of a named option. */
  public void set (String key, Object value)
  {
    set(key, value, null);
  }

  /** Set the value of a named option. */
  public void set (String key, Object value, SourceMessages messages)
  {
    OptionInfo info = getInfo(key);
    if (info == null)
      {
	error("invalid option key: "+key, messages);
	return;
      }
    if ((info.kind & BOOLEAN_OPTION) != 0)
      {
	if (value instanceof String)
	  value = valueOf(info, (String) value);
	if (value == null)
	  {
	    error("value for option "+key
		  +" must be boolean or yes/no/true/false/1/0",
		  messages);
	    return;
	  }
      }
    else if (value == null)
      value = "";
    valueTable.put(key, value);
  }

  public static final String UNKNOWN = "unknown option name";

  /** Set the value of the key to the argument, appropriate parsed.
   * return null on success or a String error message.
   * If the option key is invalid, return UNKNOWN. */
  public String set (String key, String argument)
  {
    OptionInfo info = getInfo(key);
    if (info == null)
      return UNKNOWN;
    Object value = valueOf(info, argument);
    if (value == null)
      {
	if ((info.kind & BOOLEAN_OPTION) != 0)
	  return "value of option "+key+" must be yes/no/true/false/1/0";
      }
    valueTable.put(key, value);
    return null;
  }

  public OptionInfo getInfo (String key)
  {
    Object info = infoTable.get(key);
    if (info == null && previous != null)
      info = previous.getInfo(key);
    return (OptionInfo) info;
  }

  /** Get the value for the option.
   * Throws an except if there is no option by that name,
   * Returns defaultValue if there is such an option, but it
   * hasn't been set. */
  public Object get (String key, Object defaultValue)
  {
    Object val = valueTable.get(key);
    if (val != null)
      return val;
    if (previous != null)
      return previous.get(key, defaultValue);
    OptionInfo info = getInfo(key);
    if (info == null)
      throw new RuntimeException("invalid option key: "+key);
    return defaultValue;
  }

  public boolean getBoolean (String key)
  {
    return ((Boolean) get (key, Boolean.FALSE)).booleanValue();
  }

}
final class OptionInfo
{
  OptionInfo next;
  String key;
  int kind;
  String documentation;
}
