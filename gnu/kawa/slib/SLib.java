package gnu.kawa.slib;

public class SLib
{
  static java.util.Hashtable map = new java.util.Hashtable();

  static void map(String featureName, String className)
  {
    map.put(featureName, className);
  }

  private static final String SLIB_PREFIX = "gnu.kawa.slib.";

  static
  {
    map("generic-write", SLIB_PREFIX + "genwrite");
    map("pretty-print", SLIB_PREFIX + "pp");
    map("pprint-file", SLIB_PREFIX + "ppfile");
    map("printf", SLIB_PREFIX + "printf");
    map("xml", SLIB_PREFIX + "XML");
  }

  public static String mapFeature(String featureName)
  {
    return (String) map.get(featureName);
  }
}
