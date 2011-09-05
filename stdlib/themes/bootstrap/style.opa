/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/

/*Author: Ida Swarczewskaja, MLstate */

/**
 * {1 About this module}
 * This module allows you to use Twitter's bootstrap style (http://twitter.github.com/bootstrap/) directly in your application.
 * It also includes several sets of icons.
 *
 * {2 Where should I start}
 * The only thing you need to do is to import this package
 *
 * {3 How to use icons}
 * There are different sizes:
 * - to use 16x16 pixels icon, set class "icon"
 * - to use 32x32 pixels icon, set class "icon32"
 * There are different colors:
 * - gray is the default
 * - to use black color, set class "icon-white"
 * - to use white color, set class "icon-black"
 * There are different icons:
 * - triangle (icon-triangle-n,icon-triangle-e, ...)
 * - arrow (icon-arrow-n, icon-arrowthick-n, icon-arrowreturn-se, ...)
 * - icon-plus, icon-minus, icon-close, icon-check, icon-help, icon-notice ...
 */

/* Publish resources */

@private
option : DynamicResource.config = {
sufix=some("icon.png")
prefix=none
onaccess=none
}

@private
param = {
  expiration={none}
  consumption={unlimited}
  visibility={shared}
}

@private
publish_img(name) = DynamicResource.publish_extend(name,param,option)


/* resources */
@private
files_img = @static_include_directory("stdlib/themes/bootstrap/images")

@private
uri_img = Map.map(publish_img,files_img)

@private
get_img(name) =
  x = Map.get("stdlib/themes/bootstrap/images/{name}",uri_img) ? ""
  {url=Url.make(x)}


/* css */

@private
icon16 = css
/* Icons 16px */
/*Default 16px gray icon for light backgrounds*/
.icon {
  width: 16px;
  height: 16px;
  background: {get_img("icons16-gray.png")} no-repeat;
  display: inline-block;
  vertical-align: top;
}
.icon:hover {background: {get_img("icons16-darkGray.png")} no-repeat;}

/*Color icon for active, selected for light backgrounds*/
.selected .icon {background: {get_img("icons16-color.png")} no-repeat;}

/*White icons for vivid and dark backgrounds*/
.icon.icon-white {background: {get_img("icons16-white.png")} no-repeat;}

/*Black icons for gray backgrounds*/
.icon.icon-black {background: {get_img("icons16-black.png")} no-repeat;}

/* positioning */
/* triangle */
.icon.icon-triangle-n, .icon.icon-triangle-n:hover { background-position: 0 0; }
.icon.icon-triangle-ne, .icon.icon-triangle-ne:hover { background-position: -16px 0; }
.icon.icon-triangle-e, .icon.icon-triangle-e:hover { background-position: -32px 0; }
.icon.icon-triangle-se, .icon.icon-triangle-se:hover { background-position: -48px 0; }
.icon.icon-triangle-s, .icon.icon-triangle-s:hover { background-position: -64px 0; }
.icon.icon-triangle-sw, .icon.icon-triangle-sw:hover { background-position: -80px 0; }
.icon.icon-triangle-w, .icon.icon-triangle-w:hover { background-position: -96px 0; }
.icon.icon-triangle-nw, .icon.icon-triangle-nw:hover { background-position: -112px 0; }
.icon.icon-triangle-ns, .icon.icon-triangle-ns:hover { background-position: -128px 0; }
.icon.icon-triangle-ew, .icon.icon-triangle-ew:hover { background-position: -144px 0; }

/* arrow stop */
.icon.icon-arrowstop-n, .icon.icon-arrowstop-n:hover { background-position: -160px 0; }
.icon.icon-arrowstop-e, .icon.icon-arrowstop-e:hover { background-position: -176px 0; }
.icon.icon-arrowstop-s, .icon.icon-arrowstop-s:hover { background-position: -192px 0; }
.icon.icon-arrowstop-w, .icon.icon-arrowstop-w:hover { background-position: -208px 0; }

/* arrow transfert, shuffle */
.icon.icon-transefr-ew, .icon.icon-transefr-ew:hover { background-position: -224px 0; }
.icon.icon-shuffle, .icon.icon-shuffle:hover { background-position: -240px 0; }

/* carat */
.icon.icon-carat-1-n, .icon.icon-carat-1-n:hover { background-position: 0 -16px; }
.icon.icon-carat-1-ne, .icon.icon-carat-1-ne:hover { background-position: -16px -16px; }
.icon.icon-carat-1-e, .icon.icon-carat-1-e:hover { background-position: -32px -16px; }
.icon.icon-carat-1-se, .icon.icon-carat-1-se:hover { background-position: -48px -16px; }
.icon.icon-carat-1-s, .icon.icon-carat-1-s:hover { background-position: -64px -16px; }
.icon.icon-carat-1-sw, .icon.icon-carat-1-sw:hover { background-position: -80px -16px; }
.icon.icon-carat-1-w, .icon.icon-carat-1-w:hover { background-position: -96px -16px; }
.icon.icon-carat-1-nw, .icon.icon-carat-1-nw:hover { background-position: -112px -16px; }
.icon.icon-carat-2-ns, .icon.icon-carat-2-ns:hover { background-position: -128px -16px; }
.icon.icon-carat-2-ew , .icon.icon-carat-2-ew :hover { background-position: -144px -16px; }

/* symbols */
.icon.icon-plus, .icon.icon-plus:hover { background-position: -160px -16px; }
.icon.icon-minus, .icon.icon-minus:hover { background-position: -176px -16px; }
.icon.icon-close, .icon.icon-close:hover { background-position: -192px -16px; }
.icon.icon-check, .icon.icon-check:hover { background-position: -208px -16px; }
.icon.icon-help, .icon.icon-help:hover { background-position: -224px -16px; }
.icon.icon-notice, .icon.icon-notice:hover { background-position: -240px -16px; }

/* arrow */
.icon.icon-arrow-n, .icon.icon-arrow-n:hover { background-position: 0 -32px; }
.icon.icon-arrow-ne, .icon.icon-arrow-ne:hover { background-position: 16px -32px; }
.icon.icon-arrow-e, .icon.icon-arrow-e:hover { background-position: -32px -32px; }
.icon.icon-arrow-se, .icon.icon-arrow-se:hover { background-position: -48px -32px; }
.icon.icon-arrow-s, .icon.icon-arrow-s:hover { background-position: -64px -32px; }
.icon.icon-arrow-sw, .icon.icon-arrow-sw:hover { background-position: -80px -32px; }
.icon.icon-arrow-w, .icon.icon-arrow-w:hover { background-position: -96px -32px; }
.icon.icon-arrow-nw, .icon.icon-arrow-nw:hover { background-position: -112px -32px; }
.icon.icon-arrow-n-s, .icon.icon-arrow-n-s:hover { background-position: -128px -32px; }
.icon.icon-arrow-ne-sw, .icon.icon-arrow-ne-sw:hover { background-position: -144px -32px; }
.icon.icon-arrow-e-w, .icon.icon-arrow-e-w:hover { background-position: -160px -32px; }
.icon.icon-arrow-se-nw, .icon.icon-arrow-se-nw:hover { background-position: -176px -32px; }

/* arrow dialog */
.icon.icon-arrow-nesw, .icon.icon-arrow-nesw:hover { background-position: -192px -32px; }
.icon.icon-arrow-4diag, .icon.icon-arrow-4diag:hover { background-position: -208px -32px; }
.icon.icon-newwin, .icon.icon-newwin:hover { background-position: -224px -32px; }
.icon.icon-extlink, .icon.icon-extlink:hover { background-position: -240px -32px; }

/* arrow thick */
.icon.icon-arrowthick-n, .icon.icon-arrowthick-n:hover { background-position: 0 -48px; }
.icon.icon-arrowthick-ne, .icon.icon-arrowthick-ne:hover { background-position: -16px -48px; }
.icon.icon-arrowthick-e, .icon.icon-arrowthick-e:hover { background-position: -32px -48px; }
.icon.icon-arrowthick-se, .icon.icon-arrowthick-se:hover { background-position: -48px -48px; }
.icon.icon-arrowthick-s, .icon.icon-arrowthick-s:hover { background-position: -64px -48px; }
.icon.icon-arrowthick-sw, .icon.icon-arrowthick-sw:hover { background-position: -80px -48px; }
.icon.icon-arrowthick-w, .icon.icon-arrowthick-w:hover { background-position: -96px -48px; }
.icon.icon-arrowthick-nw, .icon.icon-arrowthick-nw:hover { background-position: -112px -48px; }

/* arrow return thick */
.icon.icon-undo, .icon.icon-undo:hover { background-position: -128px -48px; }
.icon.icon-redo, .icon.icon-redo:hover { background-position: -144px -48px; }
.icon.icon-replyall, .icon.icon-replyall:hover { background-position: -160px -48px; }
.icon.icon-refresh, .icon.icon-refresh:hover { background-position: -176px -48px; }

/* bullets */
.icon.icon-bullet-on, .icon.icon-bullet-on:hover { background-position: -192px -48px; }
.icon.icon-bullet-off, .icon.icon-bullet-off:hover { background-position: -208px -48px; }
.icon.icon-star-on, .icon.icon-star-on:hover { background-position: -224px -48px; }
.icon.icon-star-off, .icon.icon-star-off:hover { background-position: -240px -48px; }

/* arrow return */
.icon.icon-arrowreturn-se, .icon.icon-arrowreturn-se:hover { background-position: 0 -64px; }
.icon.icon-arrowreturn-sw, .icon.icon-arrowreturn-sw:hover { background-position: -16px -64px; }
.icon.icon-arrowreturn-ne, .icon.icon-arrowreturn-ne:hover { background-position: -32px -64px; }
.icon.icon-arrowreturn-nw, .icon.icon-arrowreturn-nw:hover { background-position: -48px -64px; }
.icon.icon-arrowreturn-ws, .icon.icon-arrowreturn-ws:hover { background-position: -64px -64px; }
.icon.icon-arrowreturn-es, .icon.icon-arrowreturn-es:hover { background-position: -80px -64px; }
.icon.icon-arrowreturn-wn, .icon.icon-arrowreturn-wn:hover { background-position: -96px -64px; }
.icon.icon-arrowreturn-en, .icon.icon-arrowreturn-en:hover { background-position: -112px -64px; }

/* arrow refresh */
.icon.icon-arrowrefresh-w, .icon.icon-arrowrefresh-w:hover { background-position: -128px -64px; }
.icon.icon-arrowrefresh-n, .icon.icon-arrowrefresh-n:hover { background-position: -144px -64px; }
.icon.icon-arrowrefresh-e, .icon.icon-arrowrefresh-e:hover { background-position: -160px -64px; }
.icon.icon-arrowrefresh-s, .icon.icon-arrowrefresh-s:hover { background-position: -176px -64px; }

/* search, zoom */
.icon.icon-search, .icon.icon-search:hover { background-position: -192px -64px; }
.icon.icon-zoomin, .icon.icon-zoomin:hover { background-position: -208px -64px; }
.icon.icon-zoomout, .icon.icon-zoomout:hover { background-position: -224px -64px; }

/* rss */
.icon.icon-rssfeed, .icon.icon-rssfeed:hover { background-position: -240px -64px; }

/* user */
.icon.icon-home, .icon.icon-home:hover { background-position: 0 -80px; }
.icon.icon-user, .icon.icon-user:hover { background-position: -16px -80px; }
.icon.icon-print, .icon.icon-print:hover { background-position: -32px -80px; }
.icon.icon-save, .icon.icon-save:hover { background-position: -48px -80px; }
.icon.icon-book, .icon.icon-book:hover { background-position: -64px -80px; }
.icon.icon-book2, .icon.icon-book2:hover { background-position: -80px -80px; }
.icon.icon-folder-collapsed, .icon.icon-folder-collapsed:hover { background-position: -96px -80px; }
.icon.icon-folder-open, .icon.icon-folder-open:hover { background-position: -112px -80px; }

/* bookmark */
.icon.icon-flag, .icon.icon-flag:hover { background-position: -128px -80px; }
.icon.icon-bookmark, .icon.icon-bookmark:hover { background-position: -144px -80px; }
.icon.icon-heart, .icon.icon-heart:hover { background-position: -160px -80px; }

/* cancel */
.icon.icon-cancel, .icon.icon-cancel:hover { background-position: -176px -80px; }
.icon.icon-trash, .icon.icon-trash:hover { background-position: -192px -80px; }

/* tag */
.icon.icon-pin, .icon.icon-pin:hover { background-position: -208px -80px; }
.icon.icon-tag, .icon.icon-tag:hover { background-position: -224px -80px; }
.icon.icon-lightbulb, .icon.icon-lightbulb:hover { background-position: -240px -80px; }

/* settings */
.icon.icon-gear, .icon.icon-gear:hover { background-position: 0 -96px; }
.icon.icon-wrench, .icon.icon-wrench:hover { background-position: -16px -96px; }
.icon.icon-locked, .icon.icon-locked:hover { background-position: -32px -96px; }
.icon.icon-unlocked, .icon.icon-unlocked:hover { background-position: -48px -96px; }
.icon.icon-key, .icon.icon-key:hover { background-position: -64px -96px; }

/* office */
.icon.icon-clipboard, .icon.icon-clipboard:hover { background-position: -80px -96px; }
.icon.icon-scissors, .icon.icon-scissors:hover { background-position: -96px -96px; }
.icon.icon-edit, .icon.icon-edit:hover { background-position: -112px -96px; }
.icon.icon-page, .icon.icon-page:hover { background-position: -128px -96px; }
.icon.icon-copy, .icon.icon-copy:hover { background-position: -144px -96px; }
.icon.icon-note, .icon.icon-note:hover { background-position: -160px -96px; }
.icon.icon-pdf, .icon.icon-pdf:hover { background-position: -176px -96px; }
.icon.icon-doc, .icon.icon-doc:hover { background-position: -192px -96px; }
.icon.icon-xls, .icon.icon-xls:hover { background-position: -208px -96px; }
.icon.icon-document, .icon.icon-document:hover { background-position: -224px -96px; }
.icon.icon-script, .icon.icon-script:hover { background-position: -240px -96px; }

.icon.icon-calendar, .icon.icon-calendar:hover { background-position: 0 -112px; }
.icon.icon-calendar2, .icon.icon-calendar2:hover { background-position: -16px -112px; }
.icon.icon-clock, .icon.icon-clock:hover { background-position: -32px -112px; }
.icon.icon-mail-closed, .icon.icon-mail-closed:hover { background-position: -48px -112px; }
.icon.icon-mail-open, .icon.icon-mail-open:hover { background-position: -64px -112px; }
.icon.icon-mail2-closed, .icon.icon-mail2-closed:hover { background-position: -80px -112px; }
.icon.icon-mail2-open, .icon.icon-mail2-open:hover { background-position: -96px -112px; }
.icon.icon-link, .icon.icon-link:hover { background-position: -112px -112px; }
.icon.icon-unlink, .icon.icon-unlink:hover { background-position: -128px -112px; }
.icon.icon-web, .icon.icon-web:hover { background-position: -144px -112px; }
.icon.icon-web2, .icon.icon-web2:hover { background-position: -160px -112px; }
.icon.icon-contacts, .icon.icon-contacts:hover { background-position: -176px -112px; }
.icon.icon-profile, .icon.icon-profile:hover { background-position: -192px -112px; }
.icon.icon-image, .icon.icon-image:hover { background-position: -208px -112px; }
.icon.icon-suitcase, .icon.icon-suitcase:hover { background-position: -224px -112px; }
.icon.icon-suitcase2, .icon.icon-suitcase2:hover { background-position: -240px -112px; }

/* circle symbols */
.icon.icon-close, .icon.icon-close:hover { background-position: 0 -128px; }
.icon.icon-add, .icon.icon-add:hover { background-position: -16px -128px; }
.icon.icon-remove, .icon.icon-remove:hover { background-position: -32px -128px; }
.icon.icon-info, .icon.icon-info:hover { background-position: -48px -128px; }

/* alert */
.icon.icon-alert, .icon.icon-alert:hover { background-position: -64px -128px; }

/* comments */
.icon.icon-comment-text, .icon.icon-comment-text:hover { background-position: -80px -128px; }
.icon.icon-comment-video, .icon.icon-comment-video:hover { background-position: -96px -128px; }
.icon.icon-comment, .icon.icon-comment:hover { background-position: -112px -128px; }

/* shopping */
.icon.icon-cart, .icon.icon-cart:hover { background-position: -128px -128px; }
.icon.icon-basket, .icon.icon-basket:hover { background-position: -144px -128px; }

/* media */
.icon.icon-messages, .icon.icon-messages:hover { background-position: -160px -128px; }
.icon.icon-users, .icon.icon-users:hover { background-position: -176px -128px; }
.icon.icon-video, .icon.icon-video:hover { background-position: -192px -128px; }
.icon.icon-audio, .icon.icon-audio:hover { background-position: -208px -128px; }
.icon.icon-volume-off, .icon.icon-volume-off:hover { background-position: -224px -128px; }
.icon.icon-volume-on, .icon.icon-volume-on:hover { background-position: -240px -128px; }


@private
icon32 = css

/* ---------------- Icons 32px ---------------- */
/* Default 32px gray icon for light backgrounds */
.icon32 {
  width: 32px;
  height: 32px;
  background: {get_img("icons32-gray.png")} no-repeat;
  background-repeat: no-repeat;
  display: inline-block;
  vertical-align: top;
}
.icon32:hover {background: {get_img("icons32-darkGray.png")} no-repeat;}

/* Color icon for active, selected for light backgrounds */
.selected .icon32 {background: {get_img("icons32-color.png")} no-repeat;}

/* White icons for vivid and dark backgrounds */
.icon32.icon-white {background: {get_img("icons32-white.png")} no-repeat;}

/* Black icons for gray backgrounds */
.icon32.icon-black {background: {get_img("icons32-black.png")} no-repeat;}

/* positioning */
/* triangle */
.icon32.icon-triangle-n, .icon32.icon-triangle-n:hover { background-position: 0 0; }
.icon32.icon-triangle-ne, .icon32.icon-triangle-ne:hover { background-position: -32px 0; }
.icon32.icon-triangle-e, .icon32.icon-triangle-e:hover { background-position: -64px 0; }
.icon32.icon-triangle-se, .icon32.icon-triangle-se:hover { background-position: -96px 0; }
.icon32.icon-triangle-s, .icon32.icon-triangle-s:hover { background-position: -128px 0; }
.icon32.icon-triangle-sw, .icon32.icon-triangle-sw:hover { background-position: -160px 0; }
.icon32.icon-triangle-w, .icon32.icon-triangle-w:hover { background-position: -192px 0; }
.icon32.icon-triangle-nw, .icon32.icon-triangle-nw:hover { background-position: -224px 0; }
.icon32.icon-triangle-ns, .icon32.icon-triangle-ns:hover { background-position: -256px 0; }
.icon32.icon-triangle-ew, .icon32.icon-triangle-ew:hover { background-position: -288px 0; }

/* arrow stop */
.icon32.icon-arrowstop-n, .icon32.icon-arrowstop-n:hover { background-position: -320px 0; }
.icon32.icon-arrowstop-e, .icon32.icon-arrowstop-e:hover { background-position: -352px 0; }
.icon32.icon-arrowstop-s, .icon32.icon-arrowstop-s:hover { background-position: -384px 0; }
.icon32.icon-arrowstop-w, .icon32.icon-arrowstop-w:hover { background-position: -416px 0; }

/* arrow transfert, shuffle */
.icon32.icon-transefr-ew, .icon32.icon-transefr-ew:hover { background-position: -448px 0; }
.icon32.icon-shuffle, .icon32.icon-shuffle:hover { background-position: -480px 0; }

/* carat */
.icon32.icon-carat-n, .icon32.icon-carat-n:hover { background-position: 0 -32px; }
.icon32.icon-carat-ne, .icon32.icon-carat-ne:hover { background-position: -32px -32px; }
.icon32.icon-carat-e, .icon32.icon-carat-e:hover { background-position: -64px -32px; }
.icon32.icon-carat-se, .icon32.icon-carat-se:hover { background-position: -96px -32px; }
.icon32.icon-carat-s, .icon32.icon-carat-s:hover { background-position: -128px -32px; }
.icon32.icon-carat-sw, .icon32.icon-carat-sw:hover { background-position: -160px -32px; }
.icon32.icon-carat-w, .icon32.icon-carat-w:hover { background-position: -192px -32px; }
.icon32.icon-carat-nw, .icon32.icon-carat-nw:hover { background-position: -224px -32px; }
.icon32.icon-carat-ns, .icon32.icon-carat-ns:hover { background-position: -256px -32px; }
.icon32.icon-carat-ew , .icon32.icon-carat-ew :hover { background-position: -288px -32px; }

/* symbols */
.icon32.icon-plus, .icon32.icon-plus:hover { background-position: -320px -32px; }
.icon32.icon-minus, .icon32.icon-minus:hover { background-position: -352px -32px; }
.icon32.icon-close, .icon32.icon-close:hover { background-position: -384px -32px; }
.icon32.icon-check, .icon32.icon-check:hover { background-position: -416px -32px; }
.icon32.icon-help, .icon32.icon-help:hover { background-position: -448px -32px; }
.icon32.icon-notice, .icon32.icon-notice:hover { background-position: -480px -32px; }

/* arrow */
.icon32.icon-arrow-n, .icon32.icon-arrow-n:hover { background-position: 0 -64px; }
.icon32.icon-arrow-ne, .icon32.icon-arrow-ne:hover { background-position: 16px -64px; }
.icon32.icon-arrow-e, .icon32.icon-arrow-e:hover { background-position: -64px -64px; }
.icon32.icon-arrow-se, .icon32.icon-arrow-se:hover { background-position: -96px -64px; }
.icon32.icon-arrow-s, .icon32.icon-arrow-s:hover { background-position: -128px -64px; }
.icon32.icon-arrow-sw, .icon32.icon-arrow-sw:hover { background-position: -160px -64px; }
.icon32.icon-arrow-w, .icon32.icon-arrow-w:hover { background-position: -192px -64px; }
.icon32.icon-arrow-nw, .icon32.icon-arrow-nw:hover { background-position: -224px -64px; }
.icon32.icon-arrow-n-s, .icon32.icon-arrow-n-s:hover { background-position: -256px -64px; }
.icon32.icon-arrow-ne-sw, .icon32.icon-arrow-ne-sw:hover { background-position: -288px -64px; }
.icon32.icon-arrow-e-w, .icon32.icon-arrow-e-w:hover { background-position: -320px -64px; }
.icon32.icon-arrow-se-nw, .icon32.icon-arrow-se-nw:hover { background-position: -352px -64px; }

/* arrow dialog */
.icon32.icon-arrow-nesw, .icon32.icon-arrow-nesw:hover { background-position: -384px -64px; }
.icon32.icon-arrow-4diag, .icon32.icon-arrow-4diag:hover { background-position: -416px -64px; }
.icon32.icon-newwin, .icon32.icon-newwin:hover { background-position: -448px -64px; }
.icon32.icon-extlink, .icon32.icon-extlink:hover { background-position: -480px -64px; }

/* arrow thick */
.icon32.icon-arrowthick-n, .icon32.icon-arrowthick-n:hover { background-position: 0 -96px; }
.icon32.icon-arrowthick-ne, .icon32.icon-arrowthick-ne:hover { background-position: -32px -96px; }
.icon32.icon-arrowthick-e, .icon32.icon-arrowthick-e:hover { background-position: -64px -96px; }
.icon32.icon-arrowthick-se, .icon32.icon-arrowthick-se:hover { background-position: -96px -96px; }
.icon32.icon-arrowthick-s, .icon32.icon-arrowthick-s:hover { background-position: -128px -96px; }
.icon32.icon-arrowthick-sw, .icon32.icon-arrowthick-sw:hover { background-position: -160px -96px; }
.icon32.icon-arrowthick-w, .icon32.icon-arrowthick-w:hover { background-position: -192px -96px; }
.icon32.icon-arrowthick-nw, .icon32.icon-arrowthick-nw:hover { background-position: -224px -96px; }

/* arrow return thick */
.icon32.icon-undo, .icon32.icon-undo:hover { background-position: -256px -96px; }
.icon32.icon-redo, .icon32.icon-redo:hover { background-position: -288px -96px; }
.icon32.icon-replyall, .icon32.icon-replyall:hover { background-position: -320px -96px; }
.icon32.icon-refresh, .icon32.icon-refresh:hover { background-position: -352px -96px; }

/* bullets */
.icon32.icon-bullet-on, .icon32.icon-bullet-on:hover { background-position: -384px -96px; }
.icon32.icon-bullet-off, .icon32.icon-bullet-off:hover { background-position: -416px -96px; }
.icon32.icon-star-on, .icon32.icon-star-on:hover { background-position: -448px -96px; }
.icon32.icon-star-off, .icon32.icon-star-off:hover { background-position: -480px -96px; }

/* arrow return */
.icon32.icon-arrowreturn-se, .icon32.icon-arrowreturn-se:hover { background-position: 0 -128px; }
.icon32.icon-arrowreturn-sw, .icon32.icon-arrowreturn-sw:hover { background-position: -32px -128px; }
.icon32.icon-arrowreturn-ne, .icon32.icon-arrowreturn-ne:hover { background-position: -64px -128px; }
.icon32.icon-arrowreturn-nw, .icon32.icon-arrowreturn-nw:hover { background-position: -96px -128px; }
.icon32.icon-arrowreturn-ws, .icon32.icon-arrowreturn-ws:hover { background-position: -128px -128px; }
.icon32.icon-arrowreturn-es, .icon32.icon-arrowreturn-es:hover { background-position: -160px -128px; }
.icon32.icon-arrowreturn-wn, .icon32.icon-arrowreturn-wn:hover { background-position: -192px -128px; }
.icon32.icon-arrowreturn-en, .icon32.icon-arrowreturn-en:hover { background-position: -224px -128px; }

/* arrow refresh */
.icon32.icon-arrowrefresh-w, .icon32.icon-arrowrefresh-w:hover { background-position: -256px -128px; }
.icon32.icon-arrowrefresh-n, .icon32.icon-arrowrefresh-n:hover { background-position: -288px -128px; }
.icon32.icon-arrowrefresh-e, .icon32.icon-arrowrefresh-e:hover { background-position: -320px -128px; }
.icon32.icon-arrowrefresh-s, .icon32.icon-arrowrefresh-s:hover { background-position: -352px -128px; }

/* search, zoom */
.icon32.icon-search, .icon32.icon-search:hover { background-position: -384px -128px; }
.icon32.icon-zoomin, .icon32.icon-zoomin:hover { background-position: -416px -128px; }
.icon32.icon-zoomout, .icon32.icon-zoomout:hover { background-position: -448px -128px; }

/* rss */
.icon32.icon-rssfeed, .icon32.icon-rssfeed:hover { background-position: -480px -128px; }

/* user */
.icon32.icon-home, .icon32.icon-home:hover { background-position: 0 -160px; }
.icon32.icon-user, .icon32.icon-user:hover { background-position: -32px -160px; }
.icon32.icon-print, .icon32.icon-print:hover { background-position: -64px -160px; }
.icon32.icon-save, .icon32.icon-save:hover { background-position: -96px -160px; }
.icon32.icon-book, .icon32.icon-book:hover { background-position: -128px -160px; }
.icon32.icon-book2, .icon32.icon-book2:hover { background-position: -160px -160px; }
.icon32.icon-folder-collapsed, .icon32.icon-folder-collapsed:hover { background-position: -192px -160px; }
.icon32.icon-folder-open, .icon32.icon-folder-open:hover { background-position: -224px -160px; }

/* bookmark */
.icon32.icon-flag, .icon32.icon-flag:hover { background-position: -256px -160px; }
.icon32.icon-bookmark, .icon32.icon-bookmark:hover { background-position: -288px -160px; }
.icon32.icon-heart, .icon32.icon-heart:hover { background-position: -320px -160px; }

/* cancel */
.icon32.icon-cancel, .icon32.icon-cancel:hover { background-position: -352px -160px; }
.icon32.icon-trash, .icon32.icon-trash:hover { background-position: -384px -160px; }

/* tag */
.icon32.icon-pin, .icon32.icon-pin:hover { background-position: -416px -160px; }
.icon32.icon-tag, .icon32.icon-tag:hover { background-position: -448px -160px; }
.icon32.icon-lightbulb, .icon32.icon-lightbulb:hover { background-position: -480px -160px; }

/* settings */
.icon32.icon-gear, .icon32.icon-gear:hover { background-position: 0 -192px; }
.icon32.icon-wrench, .icon32.icon-wrench:hover { background-position: -32px -192px; }
.icon32.icon-locked, .icon32.icon-locked:hover { background-position: -64px -192px; }
.icon32.icon-unlocked, .icon32.icon-unlocked:hover { background-position: -96px -192px; }
.icon32.icon-key, .icon32.icon-key:hover { background-position: -128px -192px; }

/* office */
.icon32.icon-clipboard, .icon32.icon-clipboard:hover { background-position: -160px -192px; }
.icon32.icon-scissors, .icon32.icon-scissors:hover { background-position: -192px -192px; }
.icon32.icon-edit, .icon32.icon-edit:hover { background-position: -224px -192px; }
.icon32.icon-page, .icon32.icon-page:hover { background-position: -256px -192px; }
.icon32.icon-copy, .icon32.icon-copy:hover { background-position: -288px -192px; }
.icon32.icon-note, .icon32.icon-note:hover { background-position: -320px -192px; }
.icon32.icon-pdf, .icon32.icon-pdf:hover { background-position: -352px -192px; }
.icon32.icon-doc, .icon32.icon-doc:hover { background-position: -384px -192px; }
.icon32.icon-xls, .icon32.icon-xls:hover { background-position: -416px -192px; }
.icon32.icon-document, .icon32.icon-document:hover { background-position: -448px -192px; }
.icon32.icon-script, .icon32.icon-script:hover { background-position: -480px -192px; }

.icon32.icon-calendar, .icon32.icon-calendar:hover { background-position: 0 -224px; }
.icon32.icon-calendar2, .icon32.icon-calendar2:hover { background-position: -32px -224px; }
.icon32.icon-clock, .icon32.icon-clock:hover { background-position: -64px -224px; }
.icon32.icon-mail-closed, .icon32.icon-mail-closed:hover { background-position: -96px -224px; }
.icon32.icon-mail-open, .icon32.icon-mail-open:hover { background-position: -128px -224px; }
.icon32.icon-mail2-closed, .icon32.icon-mail2-closed:hover { background-position: -160px -224px; }
.icon32.icon-mail2-open, .icon32.icon-mail2-open:hover { background-position: -192px -224px; }
.icon32.icon-link, .icon32.icon-link:hover { background-position: -224px -224px; }
.icon32.icon-unlink, .icon32.icon-unlink:hover { background-position: -256px -224px; }
.icon32.icon-web, .icon32.icon-web:hover { background-position: -288px -224px; }
.icon32.icon-web2, .icon32.icon-web2:hover { background-position: -320px -224px; }
.icon32.icon-contacts, .icon32.icon-contacts:hover { background-position: -352px -224px; }
.icon32.icon-profile, .icon32.icon-profile:hover { background-position: -384px -224px; }
.icon32.icon-image, .icon32.icon-image:hover { background-position: -416px -224px; }
.icon32.icon-suitcase, .icon32.icon-suitcase:hover { background-position: -448px -224px; }
.icon32.icon-suitcase2, .icon32.icon-suitcase2:hover { background-position: -480px -224px; }

/* circle symbols */
.icon32.icon-close, .icon32.icon-close:hover { background-position: 0 -256px; }
.icon32.icon-add, .icon32.icon-add:hover { background-position: -32px -256px; }
.icon32.icon-remove, .icon32.icon-remove:hover { background-position: -64px -256px; }
.icon32.icon-info, .icon32.icon-info:hover { background-position: -96px -256px; }

/* alert */
.icon32.icon-alert, .icon32.icon-alert:hover { background-position: -128px -256px; }

/* comments */
.icon32.icon-comment-text, .icon32.icon-comment-text:hover { background-position: -160px -256px; }
.icon32.icon-comment-video, .icon32.icon-comment-video:hover { background-position: -192px -256px; }
.icon32.icon-comment, .icon32.icon-comment:hover { background-position: -224px -256px; }

/* shopping */
.icon32.icon-cart, .icon32.icon-cart:hover { background-position: -256px -256px; }
.icon32.icon-basket, .icon32.icon-basket:hover { background-position: -288px -256px; }

/* media */
.icon32.icon-messages, .icon32.icon-messages:hover { background-position: -320px -256px; }
.icon32.icon-users, .icon32.icon-users:hover { background-position: -352px -256px; }
.icon32.icon-video, .icon32.icon-video:hover { background-position: -384px -256px; }
.icon32.icon-audio, .icon32.icon-audio:hover { background-position: -416px -256px; }
.icon32.icon-volume-off, .icon32.icon-volume-off:hover { background-position: -448px -256px; }
.icon32.icon-volume-on, .icon32.icon-volume-on:hover { background-position: -480px -256px; }


/* register resources */

_ = Client_code.register_css_declaration([icon16,icon32])

@private
version = "1.1.1" // use a Reference in order to modify it at runtime?
_ = Resource.register_external_css("http://twitter.github.com/bootstrap/assets/css/bootstrap-{version}.min.css")

Bootstrap = {
  import(v) =
    do Resource.unregister_external_css("http://twitter.github.com/bootstrap/assets/css/bootstrap-{version}.min.css")
    Resource.register_external_css("http://twitter.github.com/bootstrap/assets/css/bootstrap-{v}.min.css")
}
