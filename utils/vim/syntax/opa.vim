" Vim syntax file
" Language:     OPA
" Filenames:    *.opa
" Maintainers:  Raja Boujbel <firstname.name@mlstate.com>

if version < 600
  syntax clear
elseif exists("b:current_syntax") && b:current_syntax == "opa"
  finish
endif

syn case match

syn spell default

syn keyword  opaType           int list char string option float void bool
syn keyword  opaIf             if then else
syn keyword  opaMatch          match with end
syn keyword  opaKeywords       do type and rec as
syn keyword  opaPackage        import import-plugin package
syn keyword  opaDatabase       database db
syn keyword  opaParser         parser
syn keyword  opaCss            css

syn region   opaCodeinString   matchgroup=opaKeywords start=+[^\\]{+ matchgroup=opaKeywords end=+}+ containedin=opaString,opaHtml contains=ALL
syn region   opaString         start=+"+ skip=+\\"+ end=+"+
syn region   opaHtml           start=+<[^:]\S+ skip=+->+ end=+>+ contains=opaColor,opaString

syn match    opaConstructor    "\[\s*\]"
syn match    opaBypass         "%%"

syn match    opaFunDef         "->"
syn match    opaRefAssign      "<-"
syn match    opaEndStatement   ";;"
syn match    opaSemiColon      ";"
syn match    opaPipe           "|"
syn match    opaAffect         "="
syn match    opaCoerce         ":"
syn match    opaDots           "\.\.\."
syn match    opaUnderscore     "\<_\>"
syn match    opaOperator       "|>"
syn match    opaOperator       "+>"
syn match    opaOperator       "<+>"
syn match    opaOperator       "++"
syn match    opaOperator       "\^"
syn match    opaOperator       ">="
syn match    opaOperator       "<="
syn match    opaOperator       "\<<\>"
syn match    opaOperator       "\<>\>"
syn match    opaDb             "@"
syn match    opaDb             "?"
syn match    opaHtmlIdentifier "#"

syn match    opaModule         "\<\u\(\w\+\.\?\)\?"

syn match    opaDirective      "@\w\+\(_\w\+\)\?" containedin=ALLBUT,opaComment,opaOneLineComent
syn match    opaHtmlIdentifier "#\l\+\(_\w\+\)\?" containedin=ALLBUT,opaComment,opaOneLineComent,opaString

syn region   opaComment        start=+/\*+ end=+\*/+ contains=@SpellopaComment,opaOneLineComent
syn match    opaOneLineComent  "//.*$" contains=@Spell
syn keyword  opaTodo           TODO FIXME XXX containedin=opaComment,opaOneLineComent

syn match    opaNumber         "\<-\=\d\(_\|\d\)*[l|L|n]\?\>"
syn match    opaNumber         "\<-\=0[x|X]\(\x\|_\)\+[l|L|n]\?\>"
syn match    opaNumber         "\<-\=0[o|O]\(\o\|_\)\+[l|L|n]\?\>"
syn match    opaNumber         "\<-\=0[b|B]\([01]\|_\)\+[l|L|n]\?\>"
syn match    opaFloat          "\<-\=\d\(_\|\d\)*\.\(_\|\d\)*\([eE][-+]\=\d\(_\|\d\)*\)\=\>"
syn match    opaColor          "\#\x\{6}\|\#\x\{6}"

" Char definition according lexer defintion
syn match    opaChar           "'.'\|'\\[\'nbtr]'\|'\\\d\+'"

syn match    opaBraceErr       "}"
syn match    opaDblBraceErr    "}}"
syn match    opaBrackErr       "\]"
syn match    opaParenErr       ")"
syn match    opaHtmlErr        "</>"
syn match    opaBypassErr      "%%"
syn match    opaThenErr        "\<else\>"
syn match    opaEndErr         "\<end\>"

" try to include html syntax
" non concluant : opa syntax est inclu dans html syntax
" syn include  @Html             syntax/html.vim
"
syn region   opaEncl           transparent matchgroup=opaKeywords start="("         matchgroup=opaKeywords end=")"        contains=ALLBUT,opaParenErr
syn region   opaEncl           transparent matchgroup=opaKeywords start="{{"        matchgroup=opaKeywords end="}}"       contains=ALLBUT,opaBraceErr
syn region   opaEncl           transparent matchgroup=opaKeywords start="{"         matchgroup=opaKeywords end="}"        contains=ALLBUT,opaBraceErr
syn region   opaEnclBrack      transparent matchgroup=opaKeywords start="\["        matchgroup=opaKeywords end="\]"       contains=ALLBUT,opaBrackErr
syn region   opaEncl           transparent matchgroup=opaKeywords start="<>"        matchgroup=opaKeywords end="</>"      contains=ALLBUT,opaHtmlErr,opaModule
syn region   opaEncl           transparent matchgroup=opaKeywords start="%%"        matchgroup=opaKeywords end="%%"       contains=ALLBUT,opaBypassErr
syn region   opaBegin          transparent matchgroup=opaKeywords start="\<begin\>" matchgroup=opaKeywords end="\<end\>"  contains=ALLBUT,opaEndErr
syn region   opaIf             transparent matchgroup=opaKeywords start="\<if\>"    matchgroup=opaKeywords end="\<then\>" contains=ALLBUT,opaThenErr


syn match    opaDbIdentifier   "/\w\+\(\(/\w\+\|\[.\+]\)\)*" contains=opaString,opaEnclBrack
syn match    opaServer         "server\(\s\|\n\|\r\)*="

syn sync minlines=50
syn sync maxlines=500

if !exists("did_opa_syntax_inits")
  let did_opa_syntax_inits = 1
  command -nargs=+ HiLink hi link <args>
else
  command -nargs=+ HiLink hi def link <args>
endif


HiLink   opaComment          Comment
HiLink   opaOneLineComent    Comment
HiLink   opaTodo             Todo

HiLink   opaString           String
HiLink   opaStringinString   String

HiLink   opaModule           Include

HiLink   opaType             Type

HiLink   opaKeywords         Keyword
HiLink   opaMatch            Keyword
HiLink   opaIf               Keyword
HiLink   opaDb               Keyword
HiLink   opaDatabase         Keyword
HiLink   opaServer           Keyword
HiLink   opaParser           Keyword
HiLink   opaCss              Keyword
HiLink   opaPackage          Keyword

HiLink   opaFunDef           Keyword
HiLink   opaRefAssign        Keyword
HiLink   opaEndStatement     Keyword
HiLink   opaPipe             Keyword
HiLink   opaAffect           Keyword
HiLink   opaCoerce           Keyword
HiLink   opaSemiColon        Keyword
HiLink   opaDots             Keyword
HiLink   opaUnderscore       Keyword
HiLink   opaOperator         Keyword


HiLink   opaNumber	         Number
HiLink   opaFloat	         Float
HiLink   opaColor            Number
HiLink   opaChar             Character

HiLink   opaConstructor      Constant

HiLink   opaBraceErr         Error
HiLink   opaBrackErr         Error
HiLink   opaParenErr         Error
HiLink   opaHtmlErr          Error
HiLink   opaThenErr          Error
HiLink   opaEndErr           Error

HiLink   opaHtml             Special

HiLink   opaDirective        Identifier
HiLink   opaDbIdentifier     Identifier
HiLink   opaHtmlIdentifier   Identifier

delcommand HiLink

let b:current_syntax = "opa"
