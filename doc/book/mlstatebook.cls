% (c) 2006-2009 MLstate
%    All rights reserved.
%    This file is confidential and intended solely for the addressee(s).
%    Any unauthorized use or dissemination is prohibited.
% Author: Henri Binsztok
% with some code from Tufte-handout class
% NOT SUITABLE FOR DISTRIBUTION OR SALE

\NeedsTeXFormat{LaTeX2e}[1994/06/01]

\ProvidesClass{mlstatebook}[2008/11/16 v2.0.1 Tufte-handout class]

%%
% a4paper option

\newif\if@tufteh@afourpaper \@tufteh@afourpaperfalse
\DeclareOption{a4paper}{\@tufteh@afourpapertrue}

%%
% sfsidenotes option -- typesets sidenotes in sans serif typeface

\newif\if@tufteh@sfsidenotes \@tufteh@sfsidenotesfalse
\DeclareOption{sfsidenotes}{\@tufteh@sfsidenotestrue}

%%
% symmetric option -- puts marginpar space to the outside edge of the page
%   Note: this forces twoside

\newif\if@tufteh@symmetric\@tufteh@symmetricfalse
\DeclareOption{symmetric}{
  \@tufteh@symmetrictrue
  \PassOptionsToClass{twoside}{article}
}

%%
% justified option -- uses fully justified text (flush left and flush
% right) instead of ragged right.

\newif\if@tufteh@justified\@tufteh@justifiedfalse
\DeclareOption{justified}{\@tufteh@justifiedtrue}


% FIXME: should probably specify options not supported like Mittelbach's aipproc.cls

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions

\LoadClass{article}

%%
% Detect whether we're in two-side mode or not.  (Used to set up running
% heads later.)

\newif\if@tufteh@twoside\let\if@tufteh@twoside\if@twoside

%%
% Set page layout geometry

\RequirePackage[letterpaper,includemp,width=6.5in,marginparsep=0.375in,marginparwidth=2in]{geometry}
\RequirePackage[utf8]{inputenc}

\if@tufteh@afourpaper
  \geometry{a4paper,includemp,left=15mm,marginparsep=10mm,marginparwidth=50mm,right=15mm}
\fi

\if@tufteh@symmetric
\else
  \geometry{asymmetric}
\fi

\RequirePackage{ifthen}

\RequirePackage{fourier}
\renewcommand{\familydefault}{\sfdefault}
\sloppy

\RequirePackage{color}
\definecolor{mlstatetext}{rgb}{0,0,0}
\definecolor{mlstate}{RGB}{16,54,89}
%\definecolor{mlstatelight}{RGB}{35,167,200}
\definecolor{mlstatelight}{RGB}{2,173,244}
\definecolor{mlstategrey}{RGB}{136,136,136}

\RequirePackage[colorlinks=true,linkcolor=mlstategrey,urlcolor=mlstatelight]{hyperref}

\RequirePackage{graphicx}
\newcommand{\fig}[3][1.0]{
  \begin{figure}[htb]
    \begin{center}
      \resizebox{#1\linewidth}{!}{\includegraphics{#2.pdf}}
    \end{center}
    \caption{\label{fig:#2}\textcolor{mlstategrey}{{#3}}}
  \end{figure}
}
\newcommand{\wfig}[2][]{
  \begin{marginfigure}
    \includegraphics[width=\marginparwidth]{#2}
    \ifthenelse{\equal{#1}{}}{} {%
      \caption{\label{fig:#2}\textcolor{mlstategrey}{{#1}}}
    }    
  \end{marginfigure}
}
\newcommand{\wfigc}[2]{\wfig[#2]{#1}}
% \newcommand{\wfigc}[2]{
%   \begin{marginfigure}
%     \includegraphics[width=\marginparwidth]{#1}
%     \caption{\textcolor{mlstategrey}{{#2}}}
%     \label{fig:#1}
%   \end{marginfigure}
% }
\newcommand{\figpng}[3][1.0]{
  \begin{figure}[htb]
    \begin{center}
      \resizebox{#1\linewidth}{!}{\includegraphics{#2.png}}
    \end{center}
    \caption{\textcolor{mlstategrey}{{#3}}}
    \label{fig:#2}
  \end{figure}
}

\RequirePackage{eso-pic}
\newcommand\BackgroundPic{
\put(-2.5,0){
\parbox[b][\paperheight]{\paperwidth}{%
%\vfill
%\centering
\includegraphics[width=\paperwidth,
keepaspectratio]{pied_de_page_a4_300.png}%
%\vfill
}}}
%\AddToShipoutPicture{\BackgroundPic}


%%
% Configuration Français
%\RequirePackage[francais]{babel}
\parskip=3pt
\parindent=0em 

%%
% Separation marginpars by a line's worth of space.

\setlength\marginparpush{0.4\baselineskip}

%%
% Font for margin items

\if@tufteh@sfsidenotes
  \newcommand{\@tufteh@marginfont}{\normalfont\scriptsize\sffamily}
\else
  \newcommand{\@tufteh@marginfont}{\normalfont\scriptsize}
\fi

%%
% \RaggedRight allows hyphenation

\RequirePackage{ragged2e}
\setlength{\RaggedRightRightskip}{\z@ plus 0.08\hsize}

%%
% Set the justification baesed on the `justified' class option

\newcommand{\@tufteh@justification}{%
  \if@tufteh@justified
    \justifying
  \else
    \RaggedRight
  \fi
}

%%
% Turn off section numbering

\setcounter{secnumdepth}{3}

%%
% Tighten up space between displays (e.g., a figure or table) and make symmetric

\setlength\abovedisplayskip{6pt plus 2pt minus 4pt}
\setlength\belowdisplayskip{6pt plus 2pt minus 4pt}

%%
% To implement full-width display environments

\RequirePackage[strict]{changepage}
\RequirePackage{ifpdf}




% Compute length used for full-width displays

\newlength{\@tufteh@overhang}
\setlength{\@tufteh@overhang}{\marginparwidth}
\addtolength{\@tufteh@overhang}{\marginparsep}

%%
% Alter \maketitle from article.cls

\renewcommand\maketitle{\par
  \global\let\and\relax
  \global\let\thanks\footnote
  \begingroup
    \newpage
    \global\@topnum\z@
    \@maketitle
  \endgroup
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  %\global\let\@author\@empty
  %\global\let\@date\@empty
  %\global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
}
\def\@maketitle{%
  \newpage
  \noindent\sffamily\large{\allcaps{\@title}}\\
  \vskip0.5\baselineskip
  \noindent\normalsize{\allcaps{\@author}}\\
  \vskip0.3\baselineskip
  \noindent{\allcaps{\@date}}
  \thispagestyle{plain}
}

%%
% Abstract

\renewenvironment{abstract}%
 {\begin{quotation}
   \begin{sffamily}
    \begin{small}}%
 {  \end{small}
   \end{sffamily}
  \end{quotation}}

%%
% Require paralist package for tighter lists

\RequirePackage{paralist}

% Add rightmargin to compactenum

\def\@compactenum@{%
  \expandafter\list\csname label\@enumctr\endcsname{%
    \usecounter{\@enumctr}%
    \rightmargin=2em% added this
    \parsep\plparsep
    \itemsep\plitemsep
    \topsep\pltopsep
    \partopsep\plpartopsep
    \def\makelabel##1{\hss\llap{##1}}}}

%%
% Improved letterspacing of small caps and all-caps text.
%
% First, try to use the `microtype' package, if it's available. 
% Failing that, try to use the `soul' package, if it's available.
% Failing that, well, I give up.

\RequirePackage{textcase} % provides \MakeTextUppercase and \MakeTextLowercase
\def\allcapsspacing{\relax}
\def\smallcapsspacing{\relax}
\newcommand{\allcaps}[1]{\MakeTextUppercase{\allcapsspacing{#1}}}
\newcommand{\smallcaps}[1]{\MakeTextLowercase{\textsc{#1}}}

\newcommand{\@tufteh@loadsoul}{%
  \IfFileExists{soul.sty}{%
    \RequirePackage{soul}
    \sodef\allcapsspacing{\upshape}{0.15em}{0.65em}{0.6em}
    \sodef\smallcapsspacing{\scshape}{0.075em}{0.5em}{0.6em}
  }{
    \PackageWarningNoLine{tufte-handout}{Couldn't locate `soul' package.}
  }% soul not installed... giving up.
}

% If we're using pdfLaTeX v1.40+, use the letterspace package. 
% If we're using pdfLaTex < v1.40, use the soul package.
% If we're using XeLaTeX, use XeLaTeX letterspacing options.
% Otherwise fall back on the soul package.

\ifpdf\PackageInfo{tufte-handout}{ifpdf = true}
\else\PackageInfo{tufte-handout}{ifpdf = false}
\fi

\ifxetex\PackageInfo{tufte-handout}{ifxetex = true}
\else\PackageInfo{tufte-handout}{ifxetex = false}
\fi



% Check pdfLaTeX version
\def\@tufteh@pdftexversion{0}
\ifx\normalpdftexversion\@undefined \else
  \let\pdftexversion \normalpdftexversion
  \let\pdftexrevision\normalpdftexrevision
  \let\pdfoutput     \normalpdfoutput
\fi
\ifx\pdftexversion\@undefined \else
  \ifx\pdftexversion\relax \else
    \def\@tufteh@pdftexversion{6}
    \ifnum\pdftexversion < 140
      \def\@tufteh@pdftexversion{5}
    \fi
  \fi
\fi

\ifnum\@tufteh@pdftexversion<6
  % pdfLaTeX version is too old or not using pdfLaTeX
  \ifxetex
    % TODO use xetex letterspacing
    \PackageInfo{tufte-handout}{XeTeX detected. Reverting to `soul' package for letterspacing.}
    \@tufteh@loadsoul
  \else
    \PackageInfo{tufte-handout}{Old version of pdfTeX detected.  Reverting to `soul' package for letterspacing.}
    \@tufteh@loadsoul
  \fi
\else
  \IfFileExists{letterspace.sty}{%
    \PackageInfo{tufte-handout}{Modern version of pdfTeX detected.  Using `letterspace' package.}
    \RequirePackage{letterspace}
    % Set up letterspacing (using microtype package) -- requires pdfTeX v1.40+
    \renewcommand{\smallcapsspacing}[1]{\textls[50]{##1}}
    \renewcommand{\allcaps}[1]{\textls[200]{\MakeTextUppercase{##1}}}
    \renewcommand{\smallcaps}[1]{\textsc{\MakeTextLowercase{##1}}}
  }{% microtype failed, check for soul
    \PackageInfo{tufte-handout}{Modern version of pdfTeX detected, but `letterspace' package not installed.  Reverting to `soul' package for letterspacing.}
    \@tufteh@loadsoul
  }
\fi


\DeclareTextFontCommand{\textsmallcaps}{\scshape}
\renewcommand{\textsc}[1]{\textsmallcaps{\smallcapsspacing{#1}}}


%%
% An environment for paragraph-style section

\providecommand\newthought[1]{\vspace{1.8\baselineskip plus 3pt minus 2pt}%
                           {\noindent\textsc{#1}}}

%%
% Transform existing \footnotes into \sidenotes
% Sidenote: ``Where God meant footnotes to go.'' ---Tufte

\RequirePackage[side,multiple]{footmisc}
\providecommand*{\footnotelayout}{\@tufteh@marginfont\@tufteh@justification}
\renewcommand{\footnotelayout}{\@tufteh@marginfont\@tufteh@justification}

% Override footmisc's definition to set the sidenote marks (numbers) inside the
% sidenote's text block.
\long\def\@makefntext#1{\@textsuperscript{\@tufteh@marginfont\tiny\@thefnmark}\,\footnotelayout#1}

% Set the in-text footnote mark in the same typeface as the body text itself.
\def\@makefnmark{\hbox{\@textsuperscript{\normalfont\scriptsize\@thefnmark}}}

\let\sidenote\footnote

%%
% Sidenote without the footnote mark

\providecommand\marginnote[1]%
  {\marginpar{\@tufteh@marginfont\@tufteh@justification #1}}

%%
% Citations should go in the margin as well

\RequirePackage{natbib}
\RequirePackage{bibentry}	% allows bibitems to be typeset outside thebibliography environment
\nobibliography*		% pre-loads the bibliography keys
\renewcommand{\cite}[1]{\sidenote{\label{#1}\bibentry{#1}.}}
\newcommand\recite[1]{\footref{#1}}
\providecommand{\doi}[1]{\textsc{doi:} #1} % pre-defining this so it may be used before the \bibliography command it issued

% TODO: Rewrite \cite so that you can specify multiple bib keys
%       at once.  For example, \cite{Author01,Author02}
% TODO: Combine sequences of citations so that 2,3,4,6 becomes 2-4,6
%       but be careful of hyperref interaction

%%
% Make Tuftian-style section headings

% TODO: I'd prefer to use the 'titlesec' package for this formatting, but
%       I'll do it old-style for now.  --Kevin
% \newcommand\chapter{\clearpage\@startsection {section}{0}{\z@}%
%                                    {-3.5ex \@plus -1ex \@minus -.2ex}%
%                                    {2.3ex \@plus.2ex}%
%                                    {\normalfont\Huge\color{mlstate}}}
% \renewcommand\section{\@startsection {subsection}{1}{\z@}%
%                                    {-3.5ex \@plus -1ex \@minus -.2ex}%
%                                    {2.3ex \@plus.2ex}%
%                                    {\normalfont\Large\color{mlstate}}}
% \renewcommand\subsection{\@startsection{subsubsection}{2}{\z@}%
%                                      {-3.25ex\@plus -1ex \@minus -.2ex}%
%                                      {1.5ex \@plus .2ex}%
%                                      {\normalfont\large\color{mlstate}}}

\RequirePackage{needspace}
\def\renewcounter#1{%
    \@ifundefined{c@#1}
    {\@latex@error{counter #1 undefined}\@ehc}%
    \relax
    \let\@ifdefinable\@rc@ifdefinable
    \@ifnextchar[{\@newctr{#1}}{}}

\newcounter{mlpart}
\newcounter{chapter}[mlpart]
\renewcounter{section}[chapter]

\renewcommand\part[1]{%
  \clearpage%
  \refstepcounter{mlpart}%
  \addcontentsline{toc}{part}{Section \arabic{mlpart}: #1}%
  \begin{fullwidth}%
  \hangpara{1}{\noindent{\normalfont\Huge\color{mlstate} Section \arabic{mlpart}: #1}}%
  \end{fullwidth}%
  \needspace{1\baselineskip}%
%   \vspace{1cm}%
  \edef\@currentlabel{\noexpand\themlpart.\noexpand\thechapter}%
  \needspace{2\baselineskip}
%
}

\newcommand\chapter[1]{
  \refstepcounter{chapter}%
  \setcounter{section}{0}%
  \addcontentsline{toc}{section}{\arabic{mlpart}.\arabic{chapter}: #1}%
  \edef\@currentlabel{\noexpand\themlpart.\noexpand\thechapter}%
%
  \vspace{5mm}%
%
  \needspace{3\baselineskip}%
  \begin{fullwidth}%
  \hangpara{1}{\noindent{\normalfont\LARGE\color{mlstate} \arabic{mlpart}.\arabic{chapter}: #1}}%
  \FloatBarrier%
  \end{fullwidth}%
%
}

\newcommand\Section[1]{
  \refstepcounter{section}%
  \addcontentsline{toc}{subsection}{\arabic{mlpart}.\arabic{chapter}.\arabic{section}. #1}%
  \edef\@currentlabel{\noexpand\themlpart.\noexpand\thechapter.\noexpand\thesection}%
  \vspace{3mm}%
%
  \needspace{2\baselineskip}%
  \begin{fullwidth}%
  \hangpara{1}{\normalfont\Large\color{mlstate} \arabic{mlpart}.\arabic{chapter}.\arabic{section}: #1}%
%   \FloatBarrier%
  \end{fullwidth}%
}

\newcommand\Mutesection[1]{
  \Floatbarrier%
  \refstepcounter{section}%
  \addcontentsline{toc}{subsection}{\arabic{mlpart}.\arabic{chapter}.\arabic{section}. #1}%
  \edef\@currentlabel{\noexpand\themlpart.\noexpand\thechapter.\noexpand\thesection}%
}

\newcommand\Subsection[1]{
  \refstepcounter{subsection}%
  \addcontentsline{toc}{subsubsection}{\roman{subsection}) #1}%
  \edef\@currentlabel{\noexpand\themlpart.\noexpand\thechapter.\noexpand\thesection.\noexpand\roman{subsection}}%
%
  \vspace{2mm}%
%
  \needspace{2\baselineskip}%
  \noindent{\normalfont\large\color{mlstate} \roman{subsection}) #1}\\%
}

\renewcommand\subparagraph{%
  \PackageError{tufte-handout}{\noexpand\subparagraph is undefined by this class.%
    \MessageBreak See Robert Bringhurst's _The Elements of 
    \MessageBreak Typographic Style_, section 4.2.2.
    \MessageBreak \noexpand\subparagraph was used}
    {From Bringhurst's _The Elements of Typographic Style_, section 4.2.2: Use as 
    \MessageBreak many levels of headings as you need, no more and no fewer.  Also see the many 
    \MessageBreak related threads on Ask E.T. at http://www.edwardtufte.com/.}
}

\RequirePackage{listings}


%%
% The placeins package provides the \FloatBarrier command.  This forces
% LaTeX to place all of the floats before proceeding.  We'll use this to
% keep the float (figure and table) numbers in sequence.
\RequirePackage[section]{placeins}

%%
% The xspace package provides support for adding proper spaces after macros.
%
\RequirePackage{xspace}

%%
% Margin figure environment

\newsavebox{\@tufteh@marginfigbox}
\newenvironment{marginfigure}[1]
    [-1.2ex]
    {\FloatBarrier% process all floats before this point so the figure numbers stay in order.
    \begin{lrbox}{\@tufteh@marginfigbox}
    \begin{minipage}{\marginparwidth}
        \@tufteh@marginfont
        \def\@captype{figure}
        \vspace*{#1}
        \@tufteh@justification
    }    
    {\end{minipage}%
     \end{lrbox}%
     \marginpar{\usebox{\@tufteh@marginfigbox}}}


%%
% Margin table environment
\newsavebox{\@tufteh@margintablebox}
\newenvironment{margintable}[1]
   [-1.2ex]
   {\FloatBarrier% process all floats before this point so the figure numbers stay in order.
   \begin{lrbox}{\@tufteh@margintablebox}
   \begin{minipage}{\marginparwidth}
       \@tufteh@marginfont
       \def\@captype{table}
       \vspace*{#1}
       \@tufteh@justification
   }
   {\end{minipage}%
    \end{lrbox}%
    \marginpar{\usebox{\@tufteh@margintablebox}}}


%%
% Full-width figure

\renewenvironment{figure*}[1]%
  [htbp]%
  {\@float{figure}[#1]%
   \if@tufteh@symmetric
     \begin{adjustwidth}[]{}{-\@tufteh@overhang}%
   \else
     \begin{adjustwidth}{}{-\@tufteh@overhang}%
   \fi
   \begin{minipage}{\linewidth}}%
  {\end{minipage}%
   \end{adjustwidth}%
   \end@float}

%%
% Full-width table

\renewenvironment{table*}[1]
  [htbp]%
  {\@float{table}[#1]%
   \if@tufteh@symmetric
     \begin{adjustwidth}[]{}{-\@tufteh@overhang}%
   \else
     \begin{adjustwidth}{}{-\@tufteh@overhang}%
   \fi
   \begin{minipage}{\linewidth}}%
  {\end{minipage}%
   \end{adjustwidth}%
   \end@float}

%%
% Full-page-width area

\newenvironment{fullwidth}
  {\if@tufteh@symmetric%
     \begin{adjustwidth}[]{}{-\@tufteh@overhang}%
   \else%
     \begin{adjustwidth}{}{-\@tufteh@overhang}%
   \fi%
  }
  {\end{adjustwidth}}

%%
% Format the captions in a style similar to the sidenotes

\RequirePackage[format=default,font={rm,scriptsize},justification=raggedright,singlelinecheck=false]{caption}

% if the `sfsidenotes' option is specified, set the captions in sf, too.
\if@tufteh@sfsidenotes
  \captionsetup{font={sf,small}}
\else
  \captionsetup{font={rm,scriptsize}}
\fi

% if the `justified' option is specified, set the captions in flush left
% and flush right
\if@tufteh@justified
  \captionsetup{justification=justified}
\else
  \captionsetup{justification=raggedright}
\fi

%%
% If the Palatino typeface (and its math symbol set) are installed, load them.

\IfFileExists{palatino.sty}{%
  \RequirePackage{palatino}
  \IfFileExists{mathpazo.sty}{\RequirePackage[osf,sc]{mathpazo}}{}
}{} % if the Palatino typefaces aren't found, do nothing.


%%
% If the Bera Mono typeface is available, use it.

\IfFileExists{beramono.sty}{%
  \RequirePackage[T1]{fontenc}
  \RequirePackage[scaled=0.85]{beramono}
}{}


%%
% Sets up the running heads and folios.

\RequirePackage{fancyhdr}

% Set the header/footer width to be the body text block plus the margin
% note area.
\if@tufteh@symmetric
  \fancyhfoffset[LE,RO]{\@tufteh@overhang}
\else
  \fancyhfoffset[RE,RO]{\@tufteh@overhang}
\fi

% The 'fancy' page style is the default style for all pages.
\fancyhf{} % clear header and footer fields
\if@tufteh@twoside
  \fancyhead[LE]{\thepage\quad\smallcaps{\@author}}
  \fancyhead[RO]{\smallcaps{\@title}\quad\thepage}
\else
  \fancyhead[RE,RO]{\smallcaps{\@title}\quad\thepage}
\fi
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

% The 'plain' page style is used on chapter opening pages.
\fancypagestyle{plain}{
  \fancyhf{} % clear header and footer fields
  \if@tufteh@twoside
    \fancyfoot[LE,RO]{\thepage}
  \else
    \fancyfoot[RE,RO]{\thepage}
  \fi
}

% The 'empty' page style suppresses all headers and footers.
% It's used on title pages and `intentionally blank' pages.
\fancypagestyle{empty}{
  \fancyhf{} % clear header and footer fields
}

% Set the default page style to 'fancy'
\pagestyle{fancy}

%%
% Prints the list of class options and their states.
\newcommand{\printclassoptions}{%
  \texttt{symmetric}---\if@tufteh@symmetric true\else false\fi\\
  \texttt{a4paper}---\if@tufteh@afourpaper true\else false\fi\\
  \texttt{twoside}---\if@tufteh@twoside true\else false\fi\\
  \texttt{sfsidenotes}---\if@tufteh@sfsidenotes true\else false\fi\\
  \texttt{justified}---\if@tufteh@justified true\else false\fi
}

\newcommand{\intro}[1]{{\large{\textcolor{mlstategrey}{#1}}}}
\RequirePackage{eurosym}
\newcommand{\opa}{OPA\xspace}
\RequirePackage{longtable}
\RequirePackage{hanging}

\newcommand{\mlstateclosing}{
\ifthenelse{{\isodd{\thepage}}}{\newpage ~}{}
\newpage
\pagestyle{empty}
\pagecolor{mlstate}
{
\color{white}

Please contact us by email at \href{mailto:contact@mlstate.com}{contact@mlstate.com} or phone at +33~1~55~43~76~50 for any inquiry or comment regarding this document.

\vspace{10cm}

\begin{fullwidth}
\begin{center}
{\small 
\color{white}{
\begin{tabular}{c}
\includegraphics[width=3cm]{mlstate_white.pdf} \\
\\
MLstate is a brand of Proviciel SA \\
\\
{\bf Company Headquarters}: \\
37 rue des Mathurins \\
75008 Paris, France \\
\\
\hline
\\
Société Anonyme au capital de 46 849 euros \\
RCS Paris 499 630 408 \\
\\
\hline
\\
{\bf Offices}: \\
15 rue Jean-Baptiste Berlier \\
75013 Paris, France \\
\end{tabular}
}
}
\end{center}
\end{fullwidth}
}
}

%%
% Set raggedright and paragraph indentation for document
\AtBeginDocument{\@tufteh@justification\setlength\parindent{1em}}
%\AtEndDocument{\mlstateclosing}

\endinput
