public class SimpleB extends SimpleA
{
  public int c = a + b;

  public int f (int y) { return 1000 + super.f(y); }
}
