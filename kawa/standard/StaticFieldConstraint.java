package kawa.standard;
import gnu.kawa.reflect.ClassMemberConstraint;
import gnu.bytecode.ClassType;
import gnu.mapping.*;

public class StaticFieldConstraint extends ClassMemberConstraint
{
  public StaticFieldConstraint(String cname, String fname)
  {
    super(ClassType.make(cname), fname);
  }


  public static void define(Environment environ, String name,
                            String cname, String fname)
  {
    Binding binding = environ.getBinding(name);
    setConstraint(binding, new StaticFieldConstraint(cname, fname));
    //    binding.value = null;
  }
}
