package kawa.lang;

/**
 * A Pattern is used to match against objects.
 * E.g. it can be used to match against macro arguments.
 * @author	Per Bothner
 */

abstract public class Pattern
{
  /**
   * Math this Pattern against an object.
   * @param obj object to match against this pattern
   * @return null on failure, or an array of bound pattern variables.
   */
  abstract public Object[] match (Object obj);
}
