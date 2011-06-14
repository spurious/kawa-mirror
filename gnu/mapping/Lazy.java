package gnu.mapping;

public interface Lazy<T>
{
    public T force() throws Throwable;
}
