package gnu.jemacs.buffer;

public class Emacs
{
  public static void checkQuit()
  {
    if (Thread.currentThread().interrupted())
      throw new gnu.mapping.WrappedException(new InterruptedException());
  }
}

