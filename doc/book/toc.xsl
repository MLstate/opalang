<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:outline="http://code.google.com/p/wkhtmltopdf/outline"
                xmlns="http://www.w3.org/1999/xhtml">
  <xsl:output doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
              doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
              indent="yes" />
  <xsl:template match="outline:outline">
    <html>
      <head>
        <title>Table of Content</title>
        <link rel="stylesheet" href="opalang.css" />
        <style>
          h1 {
            text-align:    center;
            font-size:     20px;
            font-family:   serif;
            margin-bottom: 1cm;
          }
          div {border-bottom: 1px dashed rgb(200,200,200);}
          span {float: right;}
          li {list-style: none;}
          ul {
            font-size:   20px;
            font-family: serif;
          }
          ul ul {font-size: 14px; }
          ul ul ul {font-size: 12px; }
          ul {padding-left: 0em;}
          ul ul {padding-left: 1em;}
          a {text-decoration:none; color: black;}
          .toc {
            border-bottom:1px solid #c6c6c2;
            border-top:   1px solid #c6c6c2;
            box-shadow:   1cm 1cm;
            -webkit-box-shadow: 1cm 1cm;
            padding: 5mm;
          }
          .logo {
          margin-left: auto;
          margin-right: auto;
          margin-top: 1cm;
          margin-bottom: 1cm;
          }
        </style>
      </head>
      <body>
        <div class="toc">
          <h1 class="toc-header">Table of Contents</h1>
          <ul><xsl:apply-templates select="outline:item/outline:item"/></ul>
        </div>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="outline:item">
    <li>
      <xsl:if test="@title!=''">
        <div>
          <a>
            <xsl:if test="@link">
              <xsl:attribute name="href"><xsl:value-of select="@link"/></xsl:attribute>
            </xsl:if>
            <xsl:if test="@backLink">
              <xsl:attribute name="name"><xsl:value-of select="@backLink"/></xsl:attribute>
            </xsl:if>
            <xsl:value-of select="@title" />
          </a>
          <span> <xsl:value-of select="@page" /> </span>
        </div>
      </xsl:if>
      <ul>
        <xsl:apply-templates select="outline:item"/>
      </ul>
    </li>
  </xsl:template>
</xsl:stylesheet>
