<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <!--<xsl:import href="html/chunktoc.xsl"/>-->
<xsl:import href="chunkfast.xsl"/>
<xsl:import href="docbook-to-html.xsl"/>

<xsl:param name="html.namespace"></xsl:param>
<!-- Dummy - not actually used, except needs to be non-empty,
     so output.html.stylesheets gets called. -->
<xsl:param name="html.stylesheet">style/kawa-l.css</xsl:param>

<xsl:template name="output.html.stylesheets">
  <xsl:variable name="href">
    <xsl:call-template name="relative.path.link">
      <xsl:with-param name="target.pathname" select="Community.html"/>
    </xsl:call-template>
  </xsl:variable>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<link rel="stylesheet" title="Kawa (navbar: fixed, left)"
  href="{$href}style/kawa-l.css" media="screen, print, projection, tv"/>
<link rel="alternate stylesheet" title="Kawa (navbar: fixed, right)"
  href="{$href}style/kawa-r.css" media="screen, print, projection, tv"/>
<link rel="alternate stylesheet" title="Single column, top navigation" href="{$href}style/kawa-1col.css" type="text/css"  media="handheld, screen, print, projection, tv"/>
</xsl:template>

<!-- Change metatitle (window titlebar) to "Kawa: PAGE-TITLE" -->
<xsl:template match="*" mode="object.title.markup.textonly">
  <xsl:variable name="title">
    <xsl:apply-templates select="." mode="object.title.markup"/>
  </xsl:variable>Kawa: <xsl:value-of select="normalize-space($title)"/>
</xsl:template>

<!-- Same as in common/common.xsl except for using $object/title. -->
<xsl:template name="object.id">
  <xsl:param name="object" select="."/>
  <xsl:choose>
    <xsl:when test="$object/@id">
      <xsl:value-of select="$object/@id"/>
    </xsl:when>
    <xsl:when test="$object/@xml:id">
      <xsl:value-of select="$object/@xml:id"/>
    </xsl:when>
    <!-- If $object has a title child, use that. -->
    <xsl:when test="$object/title">
      <xsl:value-of select="translate($object/title,' ','-')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="generate-id($object)"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="user.header.logo">
  <xsl:variable name="href">
    <xsl:call-template name="relative.path.link">
      <xsl:with-param name="target.pathname" select="Community.html"/>
    </xsl:call-template>
  </xsl:variable>
<div class="logo"><a href="{$href}index.html" title="Kawa Home"><img src="{$href}style/kawa-logo.png"/></a></div>
</xsl:template>

</xsl:stylesheet>
