#   Copyright © 2011 MLstate
#
#   This file is part of Opa.
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

from pygments.lexer import RegexLexer
from pygments.token import *
import pygments
import pygments.formatters
import sys

class OpaLexer(RegexLexer):
    """
      Lexer for the Opa language
    """

    name = 'Opa'
    aliases = ['opa']
    filenames = ['*.opa']

    # most of these aren't strictly keywords
    # but if you color only real keywords, you might just
    # as well not color anything
    keywords = [
        'and','as',
        'begin',
        'css',
        'database','db','do',
        'else','end','external',
        'forall',
        'if','import',
        'match',
        'package','parser',
        'rec',
        'server',
        'then','type',
        'val',
        'with',
        'xml_parser'
    ]

    # matches both stuff and `stuff`
    ident_re = r'(([a-zA-Z_]\w*)|(`[^`]*`))'

    op_re = r'[.=\-<>,@~%/+?*&^!]'
    punc_re = r'[()\[\],;|]' # '{' and '}' are treated elsewhere
                               # because they are also used for inserts

    tokens = {
        # copied from the caml lexer, should be adapted
        'escape-sequence': [
            (r'\\[\\\"\'ntr}]', String.Escape),
            (r'\\[0-9]{3}', String.Escape),
            (r'\\x[0-9a-fA-F]{2}', String.Escape),
        ],

        # factorizing these rules, because they are inserted many times
        'comments': [
            (r'/\*', Comment, 'nested-comment'),
            (r'//.*?$', Comment),
        ],
        'comments-and-spaces': [
            pygments.lexer.include('comments'),
            (r'\s+', Text),
        ],

        'root': [
            pygments.lexer.include('comments-and-spaces'),
            # keywords
            (r'\b(%s)\b' % '|'.join(keywords), Keyword),
            # directives
            # we could parse the actual set of directives instead of anything
            # starting with @, but this is troublesome
            # because it needs to be adjusted all the time
            # and assuming we parse only sources that compile, it is useless
            (r'@'+ident_re+r'\b', Name.Builtin.Pseudo),

            # number literals
            (r'-?.[\d]+([eE][+\-]?\d+)', Number.Float),
            (r'-?\d+.\d*([eE][+\-]?\d+)', Number.Float),
            (r'-?\d+[eE][+\-]?\d+', Number.Float),
            (r'0[xX][\da-fA-F]+', Number.Hex),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[bB][01]+', Number.Binary),
            (r'\d+', Number.Integer),
            # color literals
            (r'#[\da-fA-F]{3,6}', Number.Integer),

            # string literals
            (r'"', String.Double, 'string'),
            # char literal, should be checked because this is the regexp from the caml lexer
            (r"'(?:(\\[\\\"'ntbr ])|(\\[0-9]{3})|(\\x[0-9a-fA-F]{2})|.)'",
             String.Char),

            # this is meant to deal with embedded exprs in strings
            # every time we find a '}' we pop a state so that if we were
            # inside a string, we are back in the string state
            # as a consequence, we must also push a state every time we find a '{'
            # or else we will have errors when parsing {} for instance
            (r'{', Operator, '#push'),
            (r'}', Operator, '#pop'),

            # html literals
            # this is a much more strict that the actual parser,
            # since a<b would not be parsed as html
            # but then again, the parser is way too lax, and we can't hope
            # to have something as tolerant
            (r'<(?=[a-zA-Z>])', String.Single, 'html-open-tag'),

            # db path
            # matching the '[_]' in '/a[_]' because it is a part
            # of the syntax of the db path definition
            # unfortunately, i don't know how to match the ']' in
            # /a[1], so this is somewhat inconsistent
            (r'[@?!]?(/\w+)+(\[_\])?', Name.Variable),
            # putting the same color on <- as on db path, since
            # it can be used only to mean Db.write
            (r'<-(?!'+op_re+r')', Name.Variable),

            # 'modules'
            # although modules are not distinguished by their names as in caml
            # the standard library seems to follow the convention that modules
            # only area capitalized
            (r'\b([A-Z]\w*)(?=\.)', Name.Namespace),

            # operators
            # = has a special role because this is the only
            # way to syntactic distinguish binding constructions
            # unfortunately, this colors the equal in {x=2} too
            (r'=(?!'+op_re+r')', Keyword),
            (r'(%s)+' % op_re, Operator),
            (r'(%s)+' % punc_re, Operator),

            # coercions
            (r':', Operator, 'type'),
            # type variables
            # we need this rule because we don't parse specially type definitions
            # so in "type t('a) = ...", "'a" is parsed by 'root'
            ("'"+ident_re, Keyword.Type),

            # id literal, #something, or #{expr}
            (r'#'+ident_re, String.Single),
            (r'#(?={)', String.Single),

            # identifiers
            # this avoids to color '2' in 'a2' as an integer
            (ident_re, Text),

            # default, not sure if that is needed or not
            # (r'.', Text),
        ],

        # it is quite painful to have to parse types to know where they end
        # this is the general rule for a type
        # a type is either:
        # * -> ty
        # * type-with-slash
        # * type-with-slash -> ty
        # * type-with-slash (, type-with-slash)+ -> ty
        #
        # the code is pretty funky in here, but this code would roughly translate
        # in caml to:
        # let rec type stream =
        # match stream with
        # | [< "->";  stream >] -> type stream
        # | [< "";  stream >] ->
        #   type_with_slash stream
        #   type_lhs_1 stream;
        # and type_1 stream = ...
        'type': [
            pygments.lexer.include('comments-and-spaces'),
            (r'->', Keyword.Type),
            (r'', Keyword.Type, ('#pop', 'type-lhs-1', 'type-with-slash')),
        ],

        # parses all the atomic or closed constructions in the syntax of type expressions
        # record types, tuple types, type constructors, basic type and type variables
        'type-1': [
            pygments.lexer.include('comments-and-spaces'),
            (r'\(', Keyword.Type, ('#pop', 'type-tuple')),
            (r'~?{', Keyword.Type, ('#pop', 'type-record')),
            (ident_re+r'\(', Keyword.Type, ('#pop', 'type-tuple')),
            (ident_re, Keyword.Type, '#pop'),
            ("'"+ident_re, Keyword.Type),
            # this case is not in the syntax but sometimes
            # we think we are parsing types when in fact we are parsing
            # some css, so we just pop the states until we get back into
            # the root state
            (r'', Keyword.Type, '#pop'),
        ],

        # type-with-slash is either:
        # * type-1
        # * type-1 (/ type-1)+
        'type-with-slash': [
            pygments.lexer.include('comments-and-spaces'),
            (r'', Keyword.Type, ('#pop', 'slash-type-1', 'type-1')),
        ],
        'slash-type-1': [
            pygments.lexer.include('comments-and-spaces'),
            ('/', Keyword.Type, ('#pop', 'type-1')),
            # same remark as above
            (r'', Keyword.Type, '#pop'),
        ],

        # we go in this state after having parsed a type-with-slash
        # while trying to parse a type
        # and at this point we must determine if we are parsing an arrow
        # type (in which case we must continue parsing) or not (in which
        # case we stop)
        'type-lhs-1': [
            pygments.lexer.include('comments-and-spaces'),
            (r'->', Keyword.Type, ('#pop', 'type')),
            (r'(?=,)', Keyword.Type, ('#pop', 'type-arrow')),
            (r'', Keyword.Type, '#pop'),
        ],
        'type-arrow': [
            pygments.lexer.include('comments-and-spaces'),
            # the look ahead here allows to parse f(x : int, y : float -> truc) correctly
            (r',(?=[^:]*?->)', Keyword.Type, 'type-with-slash'),
            (r'->', Keyword.Type, ('#pop', 'type')),
            # same remark as above
            (r'', Keyword.Type, '#pop'),
        ],

        # no need to do precise parsing for tuples and records
        # because they are closed constructions, so we can simply
        # find the closing delimiter
        # note that this function would be not work if the source
        # contained identifiers like `{)` (although it could be patched
        # to support it)
        'type-tuple': [
            pygments.lexer.include('comments-and-spaces'),
            (r'[^\(\)/*]+', Keyword.Type),
            (r'[/*]', Keyword.Type),
            (r'\(', Keyword.Type, '#push'),
            (r'\)', Keyword.Type, '#pop'),
        ],
        'type-record': [
            pygments.lexer.include('comments-and-spaces'),
            (r'[^{}/*]+', Keyword.Type),
            (r'[/*]', Keyword.Type),
            (r'{', Keyword.Type, '#push'),
            (r'}', Keyword.Type, '#pop'),
        ],

#        'type-tuple': [
#            pygments.lexer.include('comments-and-spaces'),
#            (r'\)', Keyword.Type, '#pop'),
#            (r'', Keyword.Type, ('#pop', 'type-tuple-1', 'type-1')),
#        ],
#        'type-tuple-1': [
#            pygments.lexer.include('comments-and-spaces'),
#            (r',?\s*\)', Keyword.Type, '#pop'), # ,) is a valid end of tuple, in (1,)
#            (r',', Keyword.Type, 'type-1'),
#        ],
#        'type-record':[
#            pygments.lexer.include('comments-and-spaces'),
#            (r'}', Keyword.Type, '#pop'),
#            (r'~?(?:\w+|`[^`]*`)', Keyword.Type, 'type-record-field-expr'),
#        ],
#        'type-record-field-expr': [
#
#        ],

        'nested-comment': [
            (r'[^/*]+', Comment),
            (r'/\*', Comment, '#push'),
            (r'\*/', Comment, '#pop'),
            (r'[/*]', Comment),
        ],

        # the coy pasting between string and single-string
        # is kinda sad. Is there a way to avoid that??
        'string': [
            (r'[^\\"{]+', String.Double),
            (r'"', String.Double, '#pop'),
            (r'{', Operator, 'root'),
            pygments.lexer.include('escape-sequence'),
        ],
        'single-string': [
            (r'[^\\\'{]+', String.Double),
            (r'\'', String.Double, '#pop'),
            (r'{', Operator, 'root'),
            pygments.lexer.include('escape-sequence'),
        ],

        # all the html stuff
        # can't really reuse some existing html parser
        # because we must be able to parse embedded expressions

        # we are in this state after someone parsed the '<' that
        # started the html literal
        'html-open-tag': [
            (r'[\w\-:]+', String.Single, ('#pop', 'html-attr')),
            (r'>', String.Single, ('#pop', 'html-content')),
        ],

        # we are in this state after someone parsed the '</' that
        # started the end of the closing tag
        'html-end-tag': [
            # this is a star, because </> is allowed
            (r'[\w\-:]*>', String.Single, '#pop'),
        ],

        # we are in this state after having parsed '<ident(:ident)?'
        # we thus parse a possibly empty list of attributes
        'html-attr': [
            (r'\s+', Text),
            (r'[\w\-:]+=', String.Single, 'html-attr-value'),
            (r'/>', String.Single, '#pop'),
            (r'>', String.Single, ('#pop', 'html-content')),
        ],

        'html-attr-value': [
            (r"'", String.Single, ('#pop', 'single-string')),
            (r'"', String.Single, ('#pop', 'string')),
            (r'#'+ident_re, String.Single, '#pop'),
            (r'#(?={)', String.Single, ('#pop', 'root')),
            (r'{', Operator, ('#pop', 'root')), # this is a tail call!
        ],

        # we should probably deal with '\' escapes here
        'html-content': [
            (r'<!--', Comment, 'html-comment'),
            (r'</', String.Single, ('#pop', 'html-end-tag')),
            (r'<', String.Single, 'html-open-tag'),
            (r'{', Operator, 'root'),
            (r'.|\s+', String.Single),
        ],

        'html-comment': [
            (r'-->', Comment, '#pop'),
            (r'[^\-]+|-', Comment),
        ],
    }

# when this file in runned with the interpreter,
# it colors all the opa files whose names are given
# on the command line
if __name__ == '__main__':
    for i in sys.argv:
        if i[-4:] == ".opa":
            print("opening "+i)
            code = open(i, 'r').read()
            formatter = pygments.formatters.HtmlFormatter(full=True, linenos=True)
            print pygments.highlight(code, OpaLexer(), formatter)
