package kawa.lang;

/* A Binding is a Location in an Environment object. */

public class Binding extends Location
{
  /** The (interned) name of the binding. */
  String name;

  /** Used to chain multiple Bindings in the same hash bucket.
   * Note that there can be multiple Bindings with the same name;
   * in that case, the newest comes first. */
  Binding chain;

  /** The "time" the binding was created.
   * If the binding is newer than the current thread, it does not count. */
  int time_stamp;
}
