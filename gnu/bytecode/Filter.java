package gnu.bytecode;

public interface Filter<T>
{
  /** Returns true if parameter is selected by this filter. */
  public boolean select(T value);
}
