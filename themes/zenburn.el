<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: zenburn.el</title><link rel="alternate" type="application/wiki" title="Редактировать" href="https://www.emacswiki.org/emacs?action=edit;id=zenburn.el" /><link type="text/css" rel="stylesheet" href="https://www.emacswiki.org/light.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="https://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: zenburn.el" href="https://www.emacswiki.org/emacs?action=rss;rcidonly=zenburn.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="https://www.emacswiki.org/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="https://www.emacswiki.org/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="https://www.emacswiki.org/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for zenburn.el only"
      href="https://www.emacswiki.org/emacs?action=rss;rcidonly=zenburn.el" /><meta content="width=device-width" name="viewport" />
<script type="text/javascript" src="/outliner.0.5.0.62-toc.js"></script>
<script type="text/javascript">

  function addOnloadEvent(fnc) {
    if ( typeof window.addEventListener != "undefined" )
      window.addEventListener( "load", fnc, false );
    else if ( typeof window.attachEvent != "undefined" ) {
      window.attachEvent( "onload", fnc );
    }
    else {
      if ( window.onload != null ) {
	var oldOnload = window.onload;
	window.onload = function ( e ) {
	  oldOnload( e );
	  window[fnc]();
	};
      }
      else
	window.onload = fnc;
    }
  }

  // https://stackoverflow.com/questions/280634/endswith-in-javascript
  if (typeof String.prototype.endsWith !== 'function') {
    String.prototype.endsWith = function(suffix) {
      return this.indexOf(suffix, this.length - suffix.length) !== -1;
    };
  }

  var initToc=function() {

    var outline = HTML5Outline(document.body);
    if (outline.sections.length == 1) {
      outline.sections = outline.sections[0].sections;
    }

    if (outline.sections.length > 1
	|| outline.sections.length == 1
           && outline.sections[0].sections.length > 0) {

      var toc = document.getElementById('toc');

      if (!toc) {
	var divs = document.getElementsByTagName('div');
	for (var i = 0; i < divs.length; i++) {
	  if (divs[i].getAttribute('class') == 'toc') {
	    toc = divs[i];
	    break;
	  }
	}
      }

      if (!toc) {
	var h2 = document.getElementsByTagName('h2')[0];
	if (h2) {
	  toc = document.createElement('div');
	  toc.setAttribute('class', 'toc');
	  h2.parentNode.insertBefore(toc, h2);
	}
      }

      if (toc) {
        var html = outline.asHTML(true);
        toc.innerHTML = html;

	items = toc.getElementsByTagName('a');
	for (var i = 0; i < items.length; i++) {
	  while (items[i].textContent.endsWith('✎')) {
            var text = items[i].childNodes[0].nodeValue;
	    items[i].childNodes[0].nodeValue = text.substring(0, text.length - 1);
	  }
	}
      }
    }
  }

  addOnloadEvent(initToc);
  </script>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" /></head><body class="default"><div class="header"><a href="https://www.emacswiki.org/emacs/%d0%9a%d0%b0%d1%80%d1%82%d0%b0%d0%a1%d0%b0%d0%b9%d1%82%d0%b0" class="logo"><img src="https://www.emacswiki.org/images/logo218x38.png" class="logo" alt="[Домой]" /></a><div class="menu"><span class="gotobar bar"><a class="local" href="https://www.emacswiki.org/emacs/%d0%9a%d0%b0%d1%80%d1%82%d0%b0%d0%a1%d0%b0%d0%b9%d1%82%d0%b0">КартаСайта</a> <a class="local" href="https://www.emacswiki.org/emacs/%d0%9f%d0%be%d1%81%d0%bb%d0%b5%d0%b4%d0%bd%d0%b8%d0%b5%d0%98%d0%b7%d0%bc%d0%b5%d0%bd%d0%b5%d0%bd%d0%b8%d1%8f">ПоследниеИзменения</a> <a href="https://www.emacswiki.org/emacs/%d0%a2%d0%b5%d1%80%d1%80%d0%b8%d1%82%d0%be%d1%80%d0%b8%d1%8f%d0%95%d0%bc%d0%b0%d0%ba%d1%81%d0%9b%d0%b8%d1%81%d0%bf%d0%b0" class="local">ТерриторияЕмаксЛиспа</a> <a class="local" href="https://www.emacswiki.org/emacs/%d0%9a%d0%b0%d0%ba%d0%98%d1%87%d1%82%d0%be">КакИчто</a> <a class="local" href="https://www.emacswiki.org/emacs/%d0%98%d0%b7%d0%b2%d0%b5%d1%81%d1%82%d0%b8%d1%8f">Известия</a> </span><form method="get" action="https://www.emacswiki.org/emacs" enctype="multipart/form-data" accept-charset="utf-8" class="search"><p><label for="search">Поиск:</label> <input type="text" name="search"  size="20" id="search" accesskey="f" /> <label for="searchlang">Язык:</label> <input type="text" name="lang"  size="10" id="searchlang" /> <input type="submit" name="dosearch" value="Вперед!" /></p></form></div><h1><a rel="nofollow" href="https://www.emacswiki.org/emacs?search=%22zenburn%5c.el%22" title="Щелкните для поиска ссылок на эту страницу">zenburn.el</a></h1></div><div class="wrapper"><div class="content browse"><p>This page contains an uploaded file:</p><p><a class="upload" href="http://www.emacswiki.org/emacs/download/zenburn.el">zenburn.el</a></p></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="translation bar"><br />  <a href="https://www.emacswiki.org/emacs?action=translate;id=zenburn.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_uk_zh" class="translation new" rel="nofollow">Добавить перевод</a></span><div class="edit bar"><a class="comment local edit" accesskey="c" href="https://www.emacswiki.org/emacs/Comments_on_zenburn.el">Обсуждение</a> <a title="Щелкните, чтобы править" href="https://www.emacswiki.org/emacs?action=edit;id=zenburn.el" class="edit" rel="nofollow" accesskey="e">Редактировать</a> <a rel="nofollow" class="history" href="https://www.emacswiki.org/emacs?action=history;id=zenburn.el">История</a> <a class="admin" rel="nofollow" href="https://www.emacswiki.org/emacs?action=admin;id=zenburn.el">Администрирование</a></div><div class="time">Редактировалось последний раз 2011-05-13 08:45 UTC пользователем host187.mdv.spnet.net <a class="diff" rel="nofollow" href="https://www.emacswiki.org/emacs?action=browse;diff=2;id=zenburn.el">(изменения)</a></div><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a class="licence" href="https://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="https://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="https://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="https://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="https://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="https://creativecommons.org/">CreativeCommons</a>
<a href="https://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
<p class="legal" style="padding-top: 0.5em">Please note our <a href="Privacy">Privacy Statement</a>.</p>
</div>
</body>
</html>
