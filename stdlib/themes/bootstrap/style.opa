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
.icon.icon-triangle-n { background-position: 0 0; }
.icon.icon-triangle-ne { background-position: -16px 0; }
.icon.icon-triangle-e { background-position: -32px 0; }
.icon.icon-triangle-se { background-position: -48px 0; }
.icon.icon-triangle-s { background-position: -64px 0; }
.icon.icon-triangle-sw { background-position: -80px 0; }
.icon.icon-triangle-w { background-position: -96px 0; }
.icon.icon-triangle-nw { background-position: -112px 0; }
.icon.icon-triangle-ns { background-position: -128px 0; }
.icon.icon-triangle-ew { background-position: -144px 0; }

/* arrow stop */
.icon.icon-arrowstop-n { background-position: -160px 0; }
.icon.icon-arrowstop-e { background-position: -176px 0; }
.icon.icon-arrowstop-s { background-position: -192px 0; }
.icon.icon-arrowstop-w { background-position: -208px 0; }

/* arrow transfert, shuffle */
.icon.icon-transefr-ew { background-position: -224px 0; }
.icon.icon-shuffle { background-position: -240px 0; }

/* carat */
.icon.icon-carat-1-n { background-position: 0 -16px; }
.icon.icon-carat-1-ne { background-position: -16px -16px; }
.icon.icon-carat-1-e { background-position: -32px -16px; }
.icon.icon-carat-1-se { background-position: -48px -16px; }
.icon.icon-carat-1-s { background-position: -64px -16px; }
.icon.icon-carat-1-sw { background-position: -80px -16px; }
.icon.icon-carat-1-w { background-position: -96px -16px; }
.icon.icon-carat-1-nw { background-position: -112px -16px; }
.icon.icon-carat-2-ns { background-position: -128px -16px; }
.icon.icon-carat-2-ew  { background-position: -144px -16px; }

/* symbols */
.icon.icon-plus { background-position: -160px -16px; }
.icon.icon-minus { background-position: -176px -16px; }
.icon.icon-close { background-position: -192px -16px; }
.icon.icon-check { background-position: -208px -16px; }
.icon.icon-help { background-position: -224px -16px; }
.icon.icon-notice { background-position: -240px -16px; }

/* arrow */
.icon.icon-arrow-n { background-position: 0 -32px; }
.icon.icon-arrow-ne { background-position: 16px -32px; }
.icon.icon-arrow-e { background-position: -32px -32px; }
.icon.icon-arrow-se { background-position: -48px -32px; }
.icon.icon-arrow-s { background-position: -64px -32px; }
.icon.icon-arrow-sw { background-position: -80px -32px; }
.icon.icon-arrow-w { background-position: -96px -32px; }
.icon.icon-arrow-nw { background-position: -112px -32px; }
.icon.icon-arrow-n-s { background-position: -128px -32px; }
.icon.icon-arrow-ne-sw { background-position: -144px -32px; }
.icon.icon-arrow-e-w { background-position: -160px -32px; }
.icon.icon-arrow-se-nw { background-position: -176px -32px; }

/* arrow dialog */
.icon.icon-arrow-nesw { background-position: -192px -32px; }
.icon.icon-arrow-4diag { background-position: -208px -32px; }
.icon.icon-newwin { background-position: -224px -32px; }
.icon.icon-extlink { background-position: -240px -32px; }

/* arrow thick */
.icon.icon-arrowthick-n { background-position: 0 -48px; }
.icon.icon-arrowthick-ne { background-position: -16px -48px; }
.icon.icon-arrowthick-e { background-position: -32px -48px; }
.icon.icon-arrowthick-se { background-position: -48px -48px; }
.icon.icon-arrowthick-s { background-position: -64px -48px; }
.icon.icon-arrowthick-sw { background-position: -80px -48px; }
.icon.icon-arrowthick-w { background-position: -96px -48px; }
.icon.icon-arrowthick-nw { background-position: -112px -48px; }

/* arrow return thick */
.icon.icon-undo { background-position: -128px -48px; }
.icon.icon-redo { background-position: -144px -48px; }
.icon.icon-replyall { background-position: -160px -48px; }
.icon.icon-refresh { background-position: -176px -48px; }

/* bullets */
.icon.icon-bullet-on { background-position: -192px -48px; }
.icon.icon-bullet-off { background-position: -208px -48px; }
.icon.icon-star-on { background-position: -224px -48px; }
.icon.icon-star-off { background-position: -240px -48px; }

/* arrow return */
.icon.icon-arrowreturn-se { background-position: 0 -64px; }
.icon.icon-arrowreturn-sw { background-position: -16px -64px; }
.icon.icon-arrowreturn-ne { background-position: -32px -64px; }
.icon.icon-arrowreturn-nw { background-position: -48px -64px; }
.icon.icon-arrowreturn-ws { background-position: -64px -64px; }
.icon.icon-arrowreturn-es { background-position: -80px -64px; }
.icon.icon-arrowreturn-wn { background-position: -96px -64px; }
.icon.icon-arrowreturn-en { background-position: -112px -64px; }

/* arrow refresh */
.icon.icon-arrowrefresh-w { background-position: -128px -64px; }
.icon.icon-arrowrefresh-n { background-position: -144px -64px; }
.icon.icon-arrowrefresh-e { background-position: -160px -64px; }
.icon.icon-arrowrefresh-s { background-position: -176px -64px; }

/* search, zoom */
.icon.icon-search { background-position: -192px -64px; }
.icon.icon-zoomin { background-position: -208px -64px; }
.icon.icon-zoomout { background-position: -224px -64px; }

/* rss */
.icon.icon-rssfeed { background-position: -240px -64px; }

/* user */
.icon.icon-home { background-position: 0 -80px; }
.icon.icon-user { background-position: -16px -80px; }
.icon.icon-print { background-position: -32px -80px; }
.icon.icon-save { background-position: -48px -80px; }
.icon.icon-book { background-position: -64px -80px; }
.icon.icon-book2 { background-position: -80px -80px; }
.icon.icon-folder-collapsed { background-position: -96px -80px; }
.icon.icon-folder-open { background-position: -112px -80px; }

/* bookmark */
.icon.icon-flag { background-position: -128px -80px; }
.icon.icon-bookmark { background-position: -144px -80px; }
.icon.icon-heart { background-position: -160px -80px; }

/* cancel */
.icon.icon-cancel { background-position: -176px -80px; }
.icon.icon-trash { background-position: -192px -80px; }

/* tag */
.icon.icon-pin { background-position: -208px -80px; }
.icon.icon-tag { background-position: -224px -80px; }
.icon.icon-lightbulb { background-position: -240px -80px; }

/* settings */
.icon.icon-gear { background-position: 0 -96px; }
.icon.icon-wrench { background-position: -16px -96px; }
.icon.icon-locked { background-position: -32px -96px; }
.icon.icon-unlocked { background-position: -48px -96px; }
.icon.icon-key { background-position: -64px -96px; }

/* office */
.icon.icon-clipboard { background-position: -80px -96px; }
.icon.icon-scissors { background-position: -96px -96px; }
.icon.icon-edit { background-position: -112px -96px; }
.icon.icon-page { background-position: -128px -96px; }
.icon.icon-copy { background-position: -144px -96px; }
.icon.icon-note { background-position: -160px -96px; }
.icon.icon-pdf { background-position: -176px -96px; }
.icon.icon-doc { background-position: -192px -96px; }
.icon.icon-xls { background-position: -208px -96px; }
.icon.icon-document { background-position: -224px -96px; }
.icon.icon-script { background-position: -240px -96px; }

.icon.icon-calendar { background-position: 0 -112px; }
.icon.icon-calendar2 { background-position: -16px -112px; }
.icon.icon-clock { background-position: -32px -112px; }
.icon.icon-mail-closed { background-position: -48px -112px; }
.icon.icon-mail-open { background-position: -64px -112px; }
.icon.icon-mail2-closed { background-position: -80px -112px; }
.icon.icon-mail2-open { background-position: -96px -112px; }
.icon.icon-link { background-position: -112px -112px; }
.icon.icon-unlink { background-position: -128px -112px; }
.icon.icon-web { background-position: -144px -112px; }
.icon.icon-web2 { background-position: -160px -112px; }
.icon.icon-contacts { background-position: -176px -112px; }
.icon.icon-profile { background-position: -192px -112px; }
.icon.icon-image { background-position: -208px -112px; }
.icon.icon-suitcase { background-position: -224px -112px; }
.icon.icon-suitcase2 { background-position: -240px -112px; }

/* circle symbols */
.icon.icon-close { background-position: 0 -128px; }
.icon.icon-add { background-position: -16px -128px; }
.icon.icon-remove { background-position: -32px -128px; }
.icon.icon-info { background-position: -48px -128px; }

/* alert */
.icon.icon-alert { background-position: -64px -128px; }

/* comments */
.icon.icon-comment-text { background-position: -80px -128px; }
.icon.icon-comment-video { background-position: -96px -128px; }
.icon.icon-comment { background-position: -112px -128px; }

/* shopping */
.icon.icon-cart { background-position: -128px -128px; }
.icon.icon-basket { background-position: -144px -128px; }

/* media */
.icon.icon-messages { background-position: -160px -128px; }
.icon.icon-users { background-position: -176px -128px; }
.icon.icon-video { background-position: -192px -128px; }
.icon.icon-audio { background-position: -208px -128px; }
.icon.icon-volume-off { background-position: -224px -128px; }
.icon.icon-volume-on { background-position: -240px -128px; }


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
.icon32.icon-triangle-n { background-position: 0 0; }
.icon32.icon-triangle-ne { background-position: -32px 0; }
.icon32.icon-triangle-e { background-position: -64px 0; }
.icon32.icon-triangle-se { background-position: -96px 0; }
.icon32.icon-triangle-s { background-position: -128px 0; }
.icon32.icon-triangle-sw { background-position: -160px 0; }
.icon32.icon-triangle-w { background-position: -192px 0; }
.icon32.icon-triangle-nw { background-position: -224px 0; }
.icon32.icon-triangle-ns { background-position: -256px 0; }
.icon32.icon-triangle-ew { background-position: -288px 0; }

/* arrow stop */
.icon32.icon-arrowstop-n { background-position: -320px 0; }
.icon32.icon-arrowstop-e { background-position: -352px 0; }
.icon32.icon-arrowstop-s { background-position: -384px 0; }
.icon32.icon-arrowstop-w { background-position: -416px 0; }

/* arrow transfert, shuffle */
.icon32.icon-transefr-ew { background-position: -448px 0; }
.icon32.icon-shuffle { background-position: -480px 0; }

/* carat */
.icon32.icon-carat-n { background-position: 0 -32px; }
.icon32.icon-carat-ne { background-position: -32px -32px; }
.icon32.icon-carat-e { background-position: -64px -32px; }
.icon32.icon-carat-se { background-position: -96px -32px; }
.icon32.icon-carat-s { background-position: -128px -32px; }
.icon32.icon-carat-sw { background-position: -160px -32px; }
.icon32.icon-carat-w { background-position: -192px -32px; }
.icon32.icon-carat-nw { background-position: -224px -32px; }
.icon32.icon-carat-ns { background-position: -256px -32px; }
.icon32.icon-carat-ew  { background-position: -288px -32px; }

/* symbols */
.icon32.icon-plus { background-position: -320px -32px; }
.icon32.icon-minus { background-position: -352px -32px; }
.icon32.icon-close { background-position: -384px -32px; }
.icon32.icon-check { background-position: -416px -32px; }
.icon32.icon-help { background-position: -448px -32px; }
.icon32.icon-notice { background-position: -480px -32px; }

/* arrow */
.icon32.icon-arrow-n { background-position: 0 -64px; }
.icon32.icon-arrow-ne { background-position: 16px -64px; }
.icon32.icon-arrow-e { background-position: -64px -64px; }
.icon32.icon-arrow-se { background-position: -96px -64px; }
.icon32.icon-arrow-s { background-position: -128px -64px; }
.icon32.icon-arrow-sw { background-position: -160px -64px; }
.icon32.icon-arrow-w { background-position: -192px -64px; }
.icon32.icon-arrow-nw { background-position: -224px -64px; }
.icon32.icon-arrow-n-s { background-position: -256px -64px; }
.icon32.icon-arrow-ne-sw { background-position: -288px -64px; }
.icon32.icon-arrow-e-w { background-position: -320px -64px; }
.icon32.icon-arrow-se-nw { background-position: -352px -64px; }

/* arrow dialog */
.icon32.icon-arrow-nesw { background-position: -384px -64px; }
.icon32.icon-arrow-4diag { background-position: -416px -64px; }
.icon32.icon-newwin { background-position: -448px -64px; }
.icon32.icon-extlink { background-position: -480px -64px; }

/* arrow thick */
.icon32.icon-arrowthick-n { background-position: 0 -96px; }
.icon32.icon-arrowthick-ne { background-position: -32px -96px; }
.icon32.icon-arrowthick-e { background-position: -64px -96px; }
.icon32.icon-arrowthick-se { background-position: -96px -96px; }
.icon32.icon-arrowthick-s { background-position: -128px -96px; }
.icon32.icon-arrowthick-sw { background-position: -160px -96px; }
.icon32.icon-arrowthick-w { background-position: -192px -96px; }
.icon32.icon-arrowthick-nw { background-position: -224px -96px; }

/* arrow return thick */
.icon32.icon-undo { background-position: -256px -96px; }
.icon32.icon-redo { background-position: -288px -96px; }
.icon32.icon-replyall { background-position: -320px -96px; }
.icon32.icon-refresh { background-position: -352px -96px; }

/* bullets */
.icon32.icon-bullet-on { background-position: -384px -96px; }
.icon32.icon-bullet-off { background-position: -416px -96px; }
.icon32.icon-star-on { background-position: -448px -96px; }
.icon32.icon-star-off { background-position: -480px -96px; }

/* arrow return */
.icon32.icon-arrowreturn-se { background-position: 0 -128px; }
.icon32.icon-arrowreturn-sw { background-position: -32px -128px; }
.icon32.icon-arrowreturn-ne { background-position: -64px -128px; }
.icon32.icon-arrowreturn-nw { background-position: -96px -128px; }
.icon32.icon-arrowreturn-ws { background-position: -128px -128px; }
.icon32.icon-arrowreturn-es { background-position: -160px -128px; }
.icon32.icon-arrowreturn-wn { background-position: -192px -128px; }
.icon32.icon-arrowreturn-en { background-position: -224px -128px; }

/* arrow refresh */
.icon32.icon-arrowrefresh-w { background-position: -256px -128px; }
.icon32.icon-arrowrefresh-n { background-position: -288px -128px; }
.icon32.icon-arrowrefresh-e { background-position: -320px -128px; }
.icon32.icon-arrowrefresh-s { background-position: -352px -128px; }

/* search, zoom */
.icon32.icon-search { background-position: -384px -128px; }
.icon32.icon-zoomin { background-position: -416px -128px; }
.icon32.icon-zoomout { background-position: -448px -128px; }

/* rss */
.icon32.icon-rssfeed { background-position: -480px -128px; }

/* user */
.icon32.icon-home { background-position: 0 -160px; }
.icon32.icon-user { background-position: -32px -160px; }
.icon32.icon-print { background-position: -64px -160px; }
.icon32.icon-save { background-position: -96px -160px; }
.icon32.icon-book { background-position: -128px -160px; }
.icon32.icon-book2 { background-position: -160px -160px; }
.icon32.icon-folder-collapsed { background-position: -192px -160px; }
.icon32.icon-folder-open { background-position: -224px -160px; }

/* bookmark */
.icon32.icon-flag { background-position: -256px -160px; }
.icon32.icon-bookmark { background-position: -288px -160px; }
.icon32.icon-heart { background-position: -320px -160px; }

/* cancel */
.icon32.icon-cancel { background-position: -352px -160px; }
.icon32.icon-trash { background-position: -384px -160px; }

/* tag */
.icon32.icon-pin { background-position: -416px -160px; }
.icon32.icon-tag { background-position: -448px -160px; }
.icon32.icon-lightbulb { background-position: -480px -160px; }

/* settings */
.icon32.icon-gear { background-position: 0 -192px; }
.icon32.icon-wrench { background-position: -32px -192px; }
.icon32.icon-locked { background-position: -64px -192px; }
.icon32.icon-unlocked { background-position: -96px -192px; }
.icon32.icon-key { background-position: -128px -192px; }

/* office */
.icon32.icon-clipboard { background-position: -160px -192px; }
.icon32.icon-scissors { background-position: -192px -192px; }
.icon32.icon-edit { background-position: -224px -192px; }
.icon32.icon-page { background-position: -256px -192px; }
.icon32.icon-copy { background-position: -288px -192px; }
.icon32.icon-note { background-position: -320px -192px; }
.icon32.icon-pdf { background-position: -352px -192px; }
.icon32.icon-doc { background-position: -384px -192px; }
.icon32.icon-xls { background-position: -416px -192px; }
.icon32.icon-document { background-position: -448px -192px; }
.icon32.icon-script { background-position: -480px -192px; }

.icon32.icon-calendar { background-position: 0 -224px; }
.icon32.icon-calendar2 { background-position: -32px -224px; }
.icon32.icon-clock { background-position: -64px -224px; }
.icon32.icon-mail-closed { background-position: -96px -224px; }
.icon32.icon-mail-open { background-position: -128px -224px; }
.icon32.icon-mail2-closed { background-position: -160px -224px; }
.icon32.icon-mail2-open { background-position: -192px -224px; }
.icon32.icon-link { background-position: -224px -224px; }
.icon32.icon-unlink { background-position: -256px -224px; }
.icon32.icon-web { background-position: -288px -224px; }
.icon32.icon-web2 { background-position: -320px -224px; }
.icon32.icon-contacts { background-position: -352px -224px; }
.icon32.icon-profile { background-position: -384px -224px; }
.icon32.icon-image { background-position: -416px -224px; }
.icon32.icon-suitcase { background-position: -448px -224px; }
.icon32.icon-suitcase2 { background-position: -480px -224px; }

/* circle symbols */
.icon32.icon-close { background-position: 0 -256px; }
.icon32.icon-add { background-position: -32px -256px; }
.icon32.icon-remove { background-position: -64px -256px; }
.icon32.icon-info { background-position: -96px -256px; }

/* alert */
.icon32.icon-alert { background-position: -128px -256px; }

/* comments */
.icon32.icon-comment-text { background-position: -160px -256px; }
.icon32.icon-comment-video { background-position: -192px -256px; }
.icon32.icon-comment { background-position: -224px -256px; }

/* shopping */
.icon32.icon-cart { background-position: -256px -256px; }
.icon32.icon-basket { background-position: -288px -256px; }

/* media */
.icon32.icon-messages { background-position: -320px -256px; }
.icon32.icon-users { background-position: -352px -256px; }
.icon32.icon-video { background-position: -384px -256px; }
.icon32.icon-audio { background-position: -416px -256px; }
.icon32.icon-volume-off { background-position: -448px -256px; }
.icon32.icon-volume-on { background-position: -480px -256px; }


/* register resources */

_ = Client_code.register_css_declaration([icon16,icon32])
_ = Resource.register_external_css("http://twitter.github.com/bootstrap/assets/css/bootstrap-1.1.1.min.css")
