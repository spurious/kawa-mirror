#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>

#ifndef GCJ_COMPILED
char *kawalib = KAWALIB;

char *
get_classpath()
{
  char *path, *classpath;
  int i;
  path = getenv("KAWALIB");
  if (path == NULL)
    path = kawalib;
  i = strlen (path);
  if (i > 4
      && (strcmp (path+i-4, ".zip") == 0
	  || strcmp (path+i-4, ".jar") == 0))
    {
      if (access (path, R_OK) < 0)
	{
	  perror ("KAWALIB does not specify a readable .zip/.jar file");
	  exit(0);
	}
    }
  else
    {
      char *buf = malloc (i + 20);
      sprintf (buf, "%s/kawa/repl.class", path);
      if (access (buf, R_OK) < 0)
	{
	  perror ("KAWALIB does not contain kawa/repl.class");
	  exit(0);
	}
    }
  
  classpath = getenv ("CLASSPATH");
  if (classpath == NULL)
    {
      char *buf = malloc (strlen (path) + 20);
      sprintf (buf, "CLASSPATH=%s", path);
      classpath = buf;
    }
  else
    {
      char *buf = malloc (strlen (path) + strlen (classpath) + 20);
      sprintf (buf, "CLASSPATH=%s:%s", classpath, path);
      classpath = buf;
    }
  return classpath;
}
#endif
