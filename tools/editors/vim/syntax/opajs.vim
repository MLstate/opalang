" Vim syntax file
" Language:     OPAJS
" Filenames:    *.opa *.js.opa
" Maintainers:  Raja Boujbel <firstname.name@mlstate.com>

if version < 600
  syntax clear
elseif exists("b:current_syntax") && b:current_syntax == "opajs"
  finish
endif

syn case match

syn spell default

" Opa is case-sensitive
syn case match

syn keyword  opajsType           int list string option float void bool
syn keyword  opajsIf             if else
syn keyword  opajsMatch          match
syn keyword  opajsKeywords       type and recursive as module
syn keyword  opajsPackage        import import-plugin package
syn keyword  opajsDatabase       database db
syn keyword  opajsParser         parser
syn keyword  opajsCss            css
syn keyword  opajsDirectiveKW    xml typeval static_content_directory static_resource_directory static_source_content static_binary_content static_include_directory catch client fail typeof lazy lazy_record thread_context with_thread_context throw track wrap unwrap callcc uncps atomic js_ident expand spawn wait server unsafe_cast toplevel assert opensums publish both prefer_client prefer_server prefer_both both_implem private public package nonexpansive asynchronous compiletime sliced_expr may_cps llarray specialize server_private opacapi stringifier xmlizer serializer comparator abstract

syn region   opajsCodeinString   matchgroup=opajsKeywords start=+[^\\]{+ matchgroup=opajsKeywords end=+}+ containedin=opajsString,opajsHtml contains=ALL
syn region   opajsString         start=+"+ skip=+\\"+ end=+"+
syn region   opajsHtml           start=+<[^:]\S+ skip=+->+ end=+>+ contains=opajsColor,opajsString

syn match    opajsConstructor    "\[\s*\]"
syn match    opajsBypass         "%%"

syn match    opajsFunDef         "->"
syn match    opajsRefAssign      "<-"
syn match    opajsEndStatement   ";;"
syn match    opajsSemiColon      ";"
syn match    opajsPipe           "|"
syn match    opajsAffect         "="
syn match    opajsCoerce         ":"
syn match    opajsDots           "\.\.\."
syn match    opajsUnderscore     "\<_\>"
syn match    opajsOperator       "|>"
syn match    opajsOperator       "+>"
syn match    opajsOperator       "<+>"
syn match    opajsOperator       "++"
syn match    opajsOperator       "\^"
syn match    opajsOperator       ">="
syn match    opajsOperator       "<="
syn match    opajsOperator       "\<<\>"
syn match    opajsOperator       "\<>\>"
syn match    opajsDb             "@"
syn match    opajsDb             "?"
syn match    opajsHtmlIdentifier "#"

syn match    opajsModule         "\<\u\(\w\+\.\?\)\?"

syn match    opajsDirective      "@\w\+\(_\w\+\)\?" containedin=ALLBUT,opajsComment,opajsOneLineComent
syn match    opajsHtmlIdentifier "#\l\+\(_\w\+\)\?" containedin=ALLBUT,opajsComment,opajsOneLineComent,opajsString

syn region   opajsComment        start=+/\*+ end=+\*/+ contains=@SpellopajsComment,opajsOneLineComent
syn match    opajsOneLineComent  "//.*$" contains=@Spell
syn keyword  opajsTodo           TODO FIXME XXX containedin=opajsComment,opajsOneLineComent

syn match    opajsNumber         "\<-\=\d\(_\|\d\)*[l|L|n]\?\>"
syn match    opajsNumber         "\<-\=0[x|X]\(\x\|_\)\+[l|L|n]\?\>"
syn match    opajsNumber         "\<-\=0[o|O]\(\o\|_\)\+[l|L|n]\?\>"
syn match    opajsNumber         "\<-\=0[b|B]\([01]\|_\)\+[l|L|n]\?\>"
syn match    opajsFloat          "\<-\=\d\(_\|\d\)*\.\(_\|\d\)*\([eE][-+]\=\d\(_\|\d\)*\)\=\>"
syn match    opajsColor          "\#\x\{6}\|\#\x\{6}"

syn match    opajsBraceErr       "}"
syn match    opajsDblBraceErr    "}}"
syn match    opajsBrackErr       "\]"
syn match    opajsParenErr       ")"
syn match    opajsHtmlErr        "</>"
syn match    opajsBypassErr      "%%"
syn match    opajsThenErr        "\<else\>"
syn match    opajsEndErr         "\<end\>"

" try to include html syntax
" non concluant : opajs syntax est inclu dans html syntax
" syn include  @Html             syntax/html.vim
"
syn region   opajsEncl           transparent matchgroup=opajsKeywords start="("         matchgroup=opajsKeywords end=")"        contains=ALLBUT,opajsParenErr
syn region   opajsEncl           transparent matchgroup=opajsKeywords start="{{"        matchgroup=opajsKeywords end="}}"       contains=ALLBUT,opajsBraceErr
syn region   opajsEncl           transparent matchgroup=opajsKeywords start="{"         matchgroup=opajsKeywords end="}"        contains=ALLBUT,opajsBraceErr
syn region   opajsEnclBrack      transparent matchgroup=opajsKeywords start="\["        matchgroup=opajsKeywords end="\]"       contains=ALLBUT,opajsBrackErr
syn region   opajsEncl           transparent matchgroup=opajsKeywords start="<>"        matchgroup=opajsKeywords end="</>"      contains=ALLBUT,opajsHtmlErr,opajsModule
syn region   opajsEncl           transparent matchgroup=opajsKeywords start="%%"        matchgroup=opajsKeywords end="%%"       contains=ALLBUT,opajsBypassErr
syn region   opajsBegin          transparent matchgroup=opajsKeywords start="\<begin\>" matchgroup=opajsKeywords end="\<end\>"  contains=ALLBUT,opajsEndErr
syn region   opajsIf             transparent matchgroup=opajsKeywords start="\<if\>"    matchgroup=opajsKeywords end=")"        contains=ALLBUT,opajsThenErr


syn match    opajsDbIdentifier   "/\w\+\(\(/\w\+\|\[.\+]\)\)*" contains=opajsString,opajsEnclBrack
syn match    opajsServer         "server\(\s\|\n\|\r\)*="

syn sync minlines=50
syn sync maxlines=500

if !exists("did_opajs_syntax_inits")
  let did_opajs_syntax_inits = 1
  command -nargs=+ HiLink hi link <args>
else
  command -nargs=+ HiLink hi def link <args>
endif


HiLink   opajsComment          Comment
HiLink   opajsOneLineComent    Comment
HiLink   opajsTodo             Todo

HiLink   opajsString           String
HiLink   opajsStringinString   String

HiLink   opajsModule           Include

HiLink   opajsType             Type

HiLink   opajsKeywords         Keyword
HiLink   opajsMatch            Keyword
HiLink   opajsIf               Keyword
HiLink   opajsDb               Keyword
HiLink   opajsDatabase         Keyword
HiLink   opajsServer           Keyword
HiLink   opajsParser           Keyword
HiLink   opajsCss              Keyword
HiLink   opajsPackage          Keyword

HiLink   opajsFunDef           Keyword
HiLink   opajsRefAssign        Keyword
HiLink   opajsEndStatement     Keyword
HiLink   opajsPipe             Keyword
HiLink   opajsAffect           Keyword
HiLink   opajsCoerce           Keyword
HiLink   opajsSemiColon        Keyword
HiLink   opajsDots             Keyword
HiLink   opajsUnderscore       Keyword
HiLink   opajsOperator         Keyword


HiLink   opajsNumber	         Number
HiLink   opajsFloat	         Float
HiLink   opajsColor            Number

HiLink   opajsConstructor      Constant

HiLink   opajsBraceErr         Error
HiLink   opajsBrackErr         Error
HiLink   opajsParenErr         Error
HiLink   opajsHtmlErr          Error
HiLink   opajsThenErr          Error
HiLink   opajsEndErr           Error

HiLink   opajsHtml             Special

HiLink   opajsDirective        Identifier
HiLink   opajsDirectiveKW      Identifier
HiLink   opajsDbIdentifier     Identifier
HiLink   opajsHtmlIdentifier   Identifier

delcommand HiLink

let b:current_syntax = "opajs"
