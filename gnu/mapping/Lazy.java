package gnu.mapping;

public interface Lazy<T>
{
    public T getValue() throws Throwable;
}
