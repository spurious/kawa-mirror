// JavaScript mostly to set up a table-of-contents sidebar.
// There are two options for the sidebar:
// (1) to use an old-fashioned <frameset> browse with-frames.html
// (2) otherwise using an <iframe> is preferred as it enables
// cleaner URLs in the location-bar.
// The <iframe> sidebar can be explicitly enabled if you use the hash
// "#sidebar" or "#sidebar=yes"; or explicitly disabled with "#sidebar=no".
// The default is to enable the sidebar except when using a ebook-reader
// (as detected by the property navigator.epubReadingSystem),
// since ebook-readers generally provide their own table-of-contents.

var usingFrameset = top.frames.length > 1;
var mainTarget = usingFrameset ? "main" : "_parent";
var mainWindow = window;
var sidebarQuery = "";
var tocFilename = "ToC.xhtml";
var xhtmlNamespace = "http://www.w3.org/1999/xhtml";

function withSidebarQuery(href) {
    var h = href.indexOf('#');
    if (h < 0)
        h = href.length;
    return href.substring(0, h) + sidebarQuery + href.substring(h);
}

function filename(loc) {
    var fname = loc.pathname;
    var sl = fname.lastIndexOf("/");
    if (sl >= 0)
        fname = fname.substring(sl+1);
    var hash = loc.hash;
    if (hash)
        fname = fname + hash.replace('#', '*');
    return fname;
}

function onMainLoad(evt) {
    if (usingFrameset) {
        top.mainLoaded = true;
        if (top.sidebarLoaded)
            updateSidebarForFrameset();
        top.setHash(location.pathname);
    } else {
        if (useSidebar(location.hash)) {
            var iframe = document.createElement("iframe");
            var mainFilename = filename(location);
            iframe.setAttribute("name", "slider");
            iframe.setAttribute("src", tocFilename+"#main="+mainFilename);
            var body = document.getElementsByTagName("body")[0];
            body.insertBefore(iframe, body.firstChild);
            body.setAttribute("class", "mainbar");
        }
        sidebarQuery = location.hash;
    }
    var links = document.getElementsByTagName("a");
    for (var i = links.length; --i >= 0; ) {
        var link = links[i];
        var href = link.getAttribute("href");
        if (href) {
            if (href.indexOf(':') >= 0)
                link.setAttribute("target", "_blank");
            else if (! usingFrameset && href.indexOf('?') < 0 && href.indexOf('#') < 0)
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
    var mainFilename = sideWindow.filename(mainWindow.location);
    var sideBody = sideWindow.document.getElementsByTagName("body")[0];
    mainWindow.clearTocStyles(sideBody);
    mainWindow.scanToc1(sideBody, mainFilename.replace('*', '#'));
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
    var body = document.getElementsByTagName("body")[0];
    body.setAttribute("class", "toc-sidebar");
    if (usingFrameset) {
        top.sidebarLoaded = true;
        if (top.mainLoaded)
            updateSidebarForFrameset();
    } else {
        var search = location.hash;
        var mainFilename = search.startsWith("#main=") // FIXME use regex
            ? search.substring(6) : null;
        if (mainFilename)
            scanToc1(body,
                     mainFilename == "ToC.xhtml" ? "index.xhtml"
                     : mainFilename.replace('*', '#'));
    }
    var links = document.getElementsByTagName("a");
    for (var i = links.length; --i >= 0; ) {
        var link = links[i];
        var href = link.getAttribute("href");
        if (href) {
            if (href.indexOf(':') > 0) {
                link.setAttribute("target", "_blank");
            } else {
                if (! usingFrameset)
                    link.setAttribute("href", withSidebarQuery(href));
                link.setAttribute("target", mainTarget);
            }
        }
    }
    if (links.length > 0) {
        var tocA = document.createElementNS(xhtmlNamespace, "a");
        tocA.setAttribute("href", "ToC.xhtml");
        tocA.setAttribute("target", mainTarget);
        tocA.appendChild(document.createTextNode("Table of Contents"));
        var tocLi = document.createElementNS(xhtmlNamespace, "li");
        tocLi.appendChild(tocA);
        var indexLi = links[links.length-1].parentNode;
        var indexGrand = indexLi.parentNode.parentNode;
        if (indexGrand.nodeName == "li") //hack
            indexLi = indexGrand;
        indexLi.parentNode.insertBefore(tocLi, indexLi.nextSibling);
    }
    var divs = document.getElementsByTagName("div");
    for (var i = divs.length; --i >= 0; ) {
        var div = divs[i];
        if (div.getAttribute("class")=="toc-title")
            div.parentNode.removeChild(div);
    }
}

var clickSeen = false;
function onClick(evt) {
    if (evt.target
        && evt.target.nodeName == "a"
        && evt.target.getAttribute("target") != "_blank")
        top.clickSeen = true;
};

function onUnload(evt) {
    if (! usingFrameset && !top.clickSeen) {
        var request = new XMLHttpRequest();
        request.open("GET","(WINDOW-CLOSED)");
        request.send(null);
    }
}

function useSidebar(hash) {
    if (hash.indexOf("sidebar=no") >= 0)
        return false;
    if (hash.indexOf("sidebar=yes") >= 0 || hash == "#sidebar")
        return true;
    return ! (navigator && navigator.epubReadingSystem);
}
if (location.href.indexOf("#main=") >= 0 || window.name == "slider") {
    window.addEventListener("load", onSidebarLoad, false);
} else {
    window.addEventListener("load", onMainLoad, false);
}
window.addEventListener("beforeunload", onUnload, false);
window.addEventListener("click", onClick, false);
