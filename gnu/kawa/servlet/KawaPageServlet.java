// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;

import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * The Kawa servlet interpreter
 *
 * This servlet is responsible for reading and interpeting Kawa language files
 * using the QEXO GNU library.
 *
 * The implementation borrows ideas from Apache Jakarta Tomcat Jasper.
 *
 * @author Ivelin Ivanov
 * @author Tom Reilly
 * @author Per Bothner
 */
public class KawaPageServlet extends KawaServlet
{
  private ServletContext context;

  public void init(ServletConfig config)
      throws ServletException
  {
    super.init(config);
    context = config.getServletContext();
  }

  public void run(CallContext ccontext) throws Throwable
  {
    ServletCallContext ctx = (ServletCallContext) ccontext;
    HttpServletRequest request = ctx.request;
    HttpServletResponse response = ctx.response;

    boolean saveClass = request.getParameter("qexo-save-class") != null;
    String path = request.getServletPath();
    Object mod = getModule(ctx, path, saveClass, response);

    if (mod instanceof ModuleBody)
      ((ModuleBody) mod).run(ctx);
  }

  private static final String MODULE_MAP_ATTRIBUTE = "gnu.kawa.module-map";

  private Object getModule(ServletCallContext ctx, String path, boolean saveClass, HttpServletResponse response)
    throws Exception
  {
    Hashtable mmap
      = (Hashtable) context.getAttribute(MODULE_MAP_ATTRIBUTE);
    if (mmap == null)
      {
        mmap = new Hashtable();
        context.setAttribute(MODULE_MAP_ATTRIBUTE, mmap);
      }
    ModuleContext mcontext
      = (ModuleContext) context.getAttribute("gnu.kawa.module-context");
    if (mcontext == null)
      mcontext = ModuleContext.getContext();
    ModuleInfo minfo = (ModuleInfo) mmap.get(path);
    long now = System.currentTimeMillis();
    ModuleManager mmanager = mcontext.getManager();

    // avoid hitting the disk too much
    if (minfo != null
        && now - minfo.lastCheckedTime < mmanager.lastModifiedCacheTime)
      return mcontext.findInstance(minfo);

    URL url = context.getResource(path);
    String upath = path;
    if (url == null)
      {
        String xpath = path;
        for (;;)
          {
            int sl = xpath.lastIndexOf('/');
            if (sl < 0)
              {
                ctx.response.reset();
                ctx.response.sendError(HttpServletResponse.SC_NOT_FOUND, path);
                return null;
              }
            xpath = xpath.substring(0, sl);
            upath = xpath + "/+default+";
            url = context.getResource(upath);
            if (url != null)
              break;
          }
      }

    if (url == null)
      {
	ctx.response.reset();
	ctx.response.sendError(HttpServletResponse.SC_NOT_FOUND, path);
	return null;
      }

    URLConnection connection = url.openConnection();
    String urlString = url.toExternalForm();
    long lastModified = connection.getLastModified();
    if (minfo != null
        && minfo.lastModifiedTime == lastModified
        && urlString.equals(minfo.sourceAbsPath))
      {
        minfo.lastCheckedTime = now;
        return mcontext.findInstance(minfo);
      }

    minfo = mmanager.findWithURL(url);
    minfo.lastModifiedTime = lastModified;
    minfo.lastCheckedTime = now;
    mmap.put(path, minfo);

    InputStream resourceStream = connection.getInputStream();
            
    Language language
      = Language.getInstanceFromFilenameExtension(path);
    if (language == null)
      language = Language.detect(resourceStream);
    if (language == null)
      {
        if (path != upath)
          {
            ctx.response.reset();
            ctx.response.sendError(HttpServletResponse.SC_NOT_FOUND, path);
            return null;
          }
        String contentType = context.getMimeType(path);
        response.setContentType(contentType);
        ServletOutputStream out = response.getOutputStream();
        byte[] buffer = new byte[4*1024];
        for (;;)
          {
            int n = resourceStream.read(buffer);
            if (n < 0)
              break;
            out.write(buffer, 0, n);
          }
        resourceStream.close();
        return null;
      }
    InPort port = new InPort(resourceStream,
                             URIPath.valueOf(path.substring(path.lastIndexOf('/')+1)));
    Language.setDefaultLanguage(language);
    SourceMessages messages = new SourceMessages();
    Compilation comp;
    try
      {
        comp = language.parse(port, messages, Language.PARSE_IMMEDIATE);
        int dot = path.indexOf('.');
        if (dot < 0)
          dot = path.length();
        String name = path.substring(path.lastIndexOf('/')+1, dot);
        comp.getModule().setName(name);
        language.resolve(comp);
      }
    catch (SyntaxException ex)
      {
        if (ex.getMessages() != messages)
          throw ex;
        // Otherwise handled below ...
        comp = null; // Needed to avoid spurious compilation error.
      }

    Class cl = null;
    if (! messages.seenErrors())
      {
        ModuleExp mexp = comp.getModule();
        comp.addMainClass(mexp);
        comp.walkModule(mexp);
        comp.setState(Compilation.WALKED);
        cl = ModuleExp.evalToClass(comp, url);
      }

    // FIXME: we could output a nice pretty HTML table of the errors
    // or show the script with the errors highlighted, for bonus
    // points the pretty page could be generated by a precompiled
    // xql script with the errors passed as XML somehow and accessed
    // via input()
    if (messages.seenErrors())
      {
        ctx.response.reset();
        ServletOutputStream out = ctx.response.getOutputStream();
        out.print(messages.toString(20));
        return null;
      }

    minfo.moduleClass = cl;
    minfo.className = cl.getName();

    if (saveClass)
      comp.outputClass(context.getRealPath("WEB-INF/classes")+'/');

    return mcontext.findInstance(minfo);
  }
}

