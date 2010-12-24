package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.lang.reflect.Proxy;
import java.util.List;
import java.util.ArrayList;
import java.lang.reflect.Array;
import gnu.expr.*;
import gnu.text.SourceMessages;

/* A procedure that evaluates to an Annotation value.
 * The parameters are using (keyword,value)-pairs,
 * though (as in Java) a single argument implies a "value" keyword.
 * Usually this will be constant-folded so the annotation can be
 * written out to a class file.
 */

public class MakeAnnotation extends ProcedureN
{
  public static final MakeAnnotation instance = new MakeAnnotation(null);

  /** If null, get annotationType from first argument. */
  ClassType annotationType;

  public MakeAnnotation (ClassType annotationType)
  {
    this.annotationType = annotationType;
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.reflect.MakeAnnotation:validate");
  }

  public static MakeAnnotation make (Object annotationType)
  {
    ClassType annotationCType;
    if (annotationType instanceof ClassType)
      annotationCType = (ClassType) annotationType;
    else if (annotationType instanceof Class)
      annotationCType = (ClassType) Type.make((Class) annotationType);
    else
      annotationCType = ClassType.make(annotationType.toString());
    return new MakeAnnotation(annotationCType);
  }

  static final gnu.bytecode.Method makeMethod =
    ClassType.make("gnu.kawa.reflect.MakeAnnotation")
    .getDeclaredMethod("make", 1);
  public static final Procedure makeMethodProc = new PrimProcedure(makeMethod);
  public static final QuoteExp makeMethodExp =
    QuoteExp.getInstance(makeMethodProc);

  public static ApplyExp makeAnnotationMaker (Expression classRef)
  {
    ApplyExp aexp = new ApplyExp(MakeAnnotation.makeMethodExp, new Expression[] { classRef });
    aexp.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    return aexp;
  }

  public String getName ()
  {
    return annotationType == null ? "make-annotation"
      : "@" + annotationType.getName();
  }


  static final ClassType classAnnotation =
    ClassType.make("java.lang.annotation.Annotation");

  public static Expression validate
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    Compilation comp = visitor.getCompilation();
    Language language = visitor.getLanguage();
    SourceMessages messages = visitor.getMessages();
    boolean mustBeConstant = visitor.processingAnnotations();
    MakeAnnotation aproc = (MakeAnnotation) proc;
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    int i = 0;

    ClassType annotationType = aproc.annotationType;

    if (annotationType == null)
      {
        args[0] = visitor.visit(args[0], null);
        i++;
        Type t = visitor.getLanguage().getTypeFor(args[0], true);
        if (t instanceof ClassType)
          annotationType = (ClassType) t;
      }
    int istart = i;

    AnnotationEntry aentry = null;
    if (annotationType != null)
      {
        if (annotationType.implementsInterface(classAnnotation))
          aentry = new AnnotationEntry(annotationType);
        else
          messages.error('e', annotationType.getName()+" is not an annotation type");
      }
    else if (mustBeConstant)
      messages.error('e', "annotation type is not a known class");
    boolean warnedMissingName = false;
    for (; i < nargs;  i++)
      {
        String name;
        if (i == istart && nargs == istart+1)
          name = "value";
        else
          {
            args[i] = visitor.visit(args[i], null);
            Object keyword = args[i].valueIfConstant();
            if (i == nargs || ! (keyword instanceof Keyword))
              {
                if (! warnedMissingName)
                  messages.error(mustBeConstant ? 'e' : 'w',
                                 "missing keyword in annotation arguments");
                warnedMissingName = false;
                aentry = null;
                name = null;
              }
            else
              {
                i++;
                name = ((Keyword) keyword).getName();
              }
          }
        Type eltype = null;
        if (annotationType != null && name != null)
          {
            Method method = annotationType.getDeclaredMethod(name, Type.typeArray0);
            if (method == null)
              {
                messages.error('e', "no annotation element named '"+name+'\'');
                aentry = null;
              }
            else
              eltype = method.getReturnType();
          }

        int ecount = messages.getErrorCount();
        args[i] = visitor.visit(args[i], eltype);
        Object arg = args[i].valueIfConstant();
        if (messages.getErrorCount() > ecount)
          {
            eltype = null;
            aentry = null;
          }
        else if (arg == null && mustBeConstant)
          {
            comp.error('e', "annotation value must be constant", args[i]);
            eltype = null;
            aentry = null;
          }
        if (aentry != null)
          {
            try
              {
                AnnotationEntry.Value val = asAnnotationValue(arg, eltype);
                aentry.addMember(name, val);
              }
            catch (Throwable ex)
              {
                aentry = null;
                comp.error(mustBeConstant ? 'e' : 'w',
                           "bad annotation value",
                           args[i]);
              }
          }
      }
    if (aentry != null)
      {
        Class aclass = annotationType.getReflectClass();
        return new QuoteExp(Proxy.newProxyInstance(aclass.getClassLoader(),
                                                   new Class[] { aclass }, aentry),
                            annotationType);
      }
    else
      return exp;
  }

  public static AnnotationEntry.Value asAnnotationValue (Object val, Type type)
  {
    String sig = type.getSignature();
    char kind = sig.charAt(0);
    switch (kind)
      {
      case 'B': val= ((Number) val).byteValue();  break;
      case 'S': val= ((Number) val).shortValue();  break;
      case 'I': val= ((Number) val).intValue();  break;
      case 'J': val = ((Number) val).longValue();  break;
      case 'L':
        if (sig.equals("Ljava/lang/String;"))
          {
            kind = 's';
            val = (String) val;
          }
        else if (sig.equals("Ljava/lang/Class;"))
          {
            kind = 'c';
            if (val instanceof Type)
              val = (Type) val;
            else
              val = Type.make((Class) val);
          }
        else if (val instanceof Enum)
          {
            kind = 'e';
          }
        break;
      case '[':
        Type eltype = ((ArrayType) type).getComponentType();
        List<AnnotationEntry.Value> alist = new ArrayList<AnnotationEntry.Value>();
        if (val instanceof List<?>)
          {
            List<?> lst = (List<?>) val;
            int len = lst.size();
            for (int i = 0;  i < len; i++)
              alist.add(asAnnotationValue(lst.get(i), eltype));
          }
        else
          {
            int len = Array.getLength(val);
            for (int i = 0;  i < len; i++)
              alist.add(asAnnotationValue(Array.get(val, i), eltype));
          }
        val = alist;
        break;
      }
    return new AnnotationEntry.Value(kind, type, val);
  }

  public Object applyN (Object[] args)
  {
    return applyN(args, null);
  }

  public Object applyN (Object[] args, SourceMessages messages)
  {
    int nargs = args.length;
    int i = 0;

    ClassType annotationType = this.annotationType;
    Class aclass;

    if (annotationType == null)
      {
        aclass = (Class) args[i++];
        annotationType = (ClassType) Type.make(aclass);
      }
    else
      aclass = annotationType.getReflectClass();
    int istart = i;

    AnnotationEntry aentry = new AnnotationEntry(annotationType);
    for (; i < nargs;  i++)
      {
        String name;
        if (i == istart && nargs == istart+1)
          name = "value";
        else
          {
            Object keyword = args[i];
            i++;
            if (i == nargs || ! (keyword instanceof Keyword))
              throw new IllegalArgumentException("missing keyword in annotation arguments");
            name = ((Keyword) keyword).getName();
          }
        Object arg = args[i];
        Method method = annotationType.getDeclaredMethod(name, Type.typeArray0);
        if (method == null)
          throw new IllegalArgumentException("no annotation element named '"+name+'\'');
        Type eltype = method.getReturnType();
        aentry.addMember(name, asAnnotationValue(arg, eltype));
      }
    return Proxy.newProxyInstance(aclass.getClassLoader(),
                                  new Class[] { aclass }, aentry);
  }
}
