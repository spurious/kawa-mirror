// JavaScript mostly to set up a table-of-contents sidebar.
// The script has no effect when using an ebook reader (since they typically
// have they own table-of-contents) or generally if browsing index.xhtml.
// To enable this code, you have to set the query string "?sidebar"
// (for an <iframe>), or browse with-frames.html (for a <frameset>).
// TODO:
// - When using frameset, and a #PAGE is specified as part of the URL,
//   load that page.  When navigating, update the location correspondingly.
//   (This is low priority, since we primarily use framesets with JavaFX
//   WebView, which doesn't show the location bar.)

var fromJar = location.protocol == "jar:";
var usingFrameset = name =="main" || name=="slider";
var mainTarget = usingFrameset ? "main" : "_parent";
var mainWindow = window;
function withSidebarQuery(href) {
    var h = href.indexOf('#');
    if (h < 0)
        h = href.length;
    return href.substring(0, h) + "?sidebar" + href.substring(h);
}

function filename(pathname) {
    var fname = pathname;
    var sl = fname.lastIndexOf("/");
    if (sl >= 0)
        fname = fname.substring(sl+1);
    return fname;
}

function onMainLoad(evt) {
    if (usingFrameset) {
        top.mainLoaded = true;
        if (top.sidebarLoaded)
            updateSidebarForFrameset();
   } else {
        var iframe = document.createElement("iframe");
        var mainFilename = filename(location.pathname);
        iframe.setAttribute("src", "bk01-toc.xhtml?main="+mainFilename);
        var body = document.getElementsByTagName("body")[0];
        body.insertBefore(iframe, body.firstChild);
        body.setAttribute("class", "mainbar");
    }
    var links = document.getElementsByTagName("a");
    for (var i = links.length; --i >= 0; ) {
        var link = links[i];
        var href = link.getAttribute("href");
        if (href) {
            if (href.indexOf(':') >= 0)
                link.setAttribute("target", "_blank");
            else if (! usingFrameset && ! fromJar && href.indexOf('?') < 0)
                link.setAttribute("href", withSidebarQuery(href));
        }
    }
}

function clearTocStyles(node) {
    if (node.tagName == "ul")
        node.removeAttribute("toc-detail");
    if (node.tagName == "a")
        node.removeAttribute("toc-current");
    for (var child = node.firstElementChild; child;
         child = child.nextElementSibling) {
        clearTocStyles(child);
    }
}

function updateSidebarForFrameset() {
    var mainWindow = top.frames["main"];
    var sideWindow = top.frames["slider"];
    var mainFilename = sideWindow.filename(mainWindow.location.pathname);
    var sideBody = sideWindow.document.getElementsByTagName("body")[0];
    mainWindow.clearTocStyles(sideBody);
    mainWindow.scanToc1(sideBody, mainFilename);
}

function scanToc1(node, current) {
    if (node.tagName == "a") { // lowercase "A" for xhtml
        var href = node.getAttribute("href");
        if (href == current) {
            node.setAttribute("toc-current", "yes");
            var ul = node.nextElementSibling;
            if (ul && ul.tagName == "ul") {
                // keep children but remove grandchildren
                // (Exception: don't remove anything on the current page;
                // however, that's not a problem in the Kawa manual.)
                for (var li = ul.firstElementChild; li; li = li.nextElementSibling) {
                    if (li.tagName == "li" && li.firstElementChild
                        && li.firstElementChild.tagName == "a") {
                        var achild = li.firstElementChild;
                        var lichild = achild.nextElementSibling;
                        if (lichild && lichild.tagName == "ul"
                            // never remove Overall-Index
                            && achild.getAttribute("href") != "Overall-Index.xhtml") {
                            lichild.setAttribute("toc-detail", "yes");
                        }
                    }
                }
            }
            return 2;
        }
    }
    var ancestor = null;
    for (var child = node.firstElementChild; child;
         child = child.nextElementSibling) {
        if (scanToc1(child, current) > 0) {
            ancestor = child;
            break;
        }
    }
    if (ancestor) {
    if (ancestor.parentNode && ancestor.parentNode.parentNode) {
        var pparent = ancestor.parentNode.parentNode;
        for (var sib = pparent.firstElementChild; sib; sib = sib.nextElementSibling) {
            if (sib != ancestor.parentNode) {
                if (sib.firstElementChild && sib.firstElementChild.nextElementSibling) {
                    sib.firstElementChild.nextElementSibling.setAttribute("toc-detail", "yes");
                }
            }
        }
    }
    }
    return ancestor ? 1 : 0;
}

function onSidebarLoad(evt) {
    if (usingFrameset) {
        top.sidebarLoaded = true;
        if (top.mainLoaded)
            updateSidebarForFrameset();
    } else {
        var search = location.search;
        var mainFilename = search.startsWith("?main=") // FIXME use regex
            ? search.substring(6) : null;
        var body = document.getElementsByTagName("body")[0];
        if (mainFilename)
            scanToc1(body, mainFilename);
    }
    var links = document.getElementsByTagName("a");
    for (var i = links.length; --i >= 0; ) {
        var link = links[i];
        var href = link.getAttribute("href");
        if (href) {
            if (href.indexOf(':') > 0) {
                link.setAttribute("target", "_blank");
            } else if (href.indexOf('?') < 0) {
                if (! fromJar && ! usingFrameset)
                    link.setAttribute("href", withSidebarQuery(href));
                link.setAttribute("target", mainTarget);
            }
        }
    }
}

if (location.href.indexOf("bk01-toc.xhtml") >= 0) {
    window.addEventListener("load", onSidebarLoad, false);
} else if (usingFrameset || location.search == "?sidebar" /* || fromJar */) {
    window.addEventListener("load", onMainLoad, false);
}
