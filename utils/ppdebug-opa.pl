#!/usr/bin/perl -w

$debugmodule = "DebugVariables";

use Getopt::Std;
%options=();
getopts("drhf:",\%options);

sub HELP_MESSAGE {
    print STDERR <<EOF;

Preprocesses ocaml source files for debug

File syntax (quick summary):
#<Debug> / #<Else> / #<End>
	Filter according to PPDEBUG mode (-d switch)
#<If:VAR TEST> / #<Else> / #<End>
	Do a run-time check on environment variable VAR (applying function
	TEST on its value if TEST is present). Disabled in release mode (-r
	switch). If #<Else> is absent, unit is assumed.
	If TEST is used, the character \$ can be used as the ppdebug module
	prefix (${debugmodule}.).
#<Ifstatic:VAR REGEXP> / #<Else> / #<End>
	Do a static check at compile-time on the value of environment
	variable VAR.
#<Debugvar:VAR>
	Define VAR as the default to test in following #<If> tests.

See $debugmodule for more information on the syntax.

Invocation: $0 [-d] [-r] [file]
	-d	enable debug sections
	-r	release mode: comment out dynamic environment checks
	file	file to parse, by default stdin

The output is put to stdout
EOF
    exit 0;
}

local $SIG{__WARN__} = sub { die $_[0] }; # Exit on anything suspicious

$file = $ARGV[0];
$file = defined($file) ? $file : "/dev/stdin";
$ppdebug = $options{d} ? 1 : ($ENV{'MLSTATE_PPDEBUG'});
$release = $options{r};

open F,$file or die "Couldn't open $file";

sub filterline {
    my $rec = shift;
    $line = <F>;
    return $line unless defined($line);

    sub nextline {
      print;
      $_ = filterline(1);
      die "ppdebug parse error (unclosed block ?) in $file, line ${.}.\n" unless defined($line);
      return $line = $_;
    }

    # Default debug variable definition
    if ($line =~ /^(.*)(#<Debugvar:([A-Z0-9_]+)>)(.*)$/) {
      $debugvar = $3;
      $line = "$1/* $2 */$4\n";
    }

    # Old ppdebug style
    if ($line =~ /#<>/) {
      if ($ppdebug) { $line =~ s%(#<>)%/* $1 */%; }
      else { $line =~ s%(#<>.*)%/* $1 */%; }
      return $line;
    }
    elsif ($line =~ /#<</) {
      if ($ppdebug) { $line =~ s%(#<<)(.*?)(>>#\s*;?)%/* $1 */$2/* $3 */%g; }
      else { $line =~ s%(#<<.*?>>#\s*;?)%/* $1 */%g; }
      return $line;
    }

    # New advanced ppdebug style
    elsif ($line =~ /^(.*)(#<(Debug|Ifstatic:\s*(.*?))>)(.*)$/) {
        my $enable;
        my $linebeg = "$1/* $2 */";
        my $lineend = "$5";
        if ($3 eq "Debug") {
          $enable = $ppdebug;
        } else {
          $4 =~ /^([A-Z0-9_]+)\s+(.*?)\s*$/ || die "Bad Ifstatic syntax";
          $enable = defined($ENV{$1}) && ($ENV{$1} =~ /^$2$/);
        }
        print $linebeg.($enable ? " " : "/* ");
        $_ = "$lineend\n";

        nextline until /^(.*?)(#<(Else|End)>)(.*)$/;
        $linebeg = $1.($enable ? "" : "*/")."/* $2 */";
        $lineend = $4;
        if ($3 eq "Else") {
            print "$linebeg".($enable ? "/* " : " ");
            $_ = "$lineend\n";
            nextline until /^(.*)(#<End>)(.*)$/;
            $line = $1.($enable ? "*/" : "")."/* $2 */$3\n";
        }
        else {
          $line = "$linebeg$lineend\n";
        }
        print $line;
        return filterline($rec);
    }
    elsif ($line =~ /^(.*)(#<If(:([A-Z0-9_]+))?((\s+|\$).*?)?>)(.*)$/) {
        my $linebeg = "$1/* $2 */";
        my $lineend = "$7";
        my ($var,$toggle) = ($4, $5);
        $var = $debugvar unless defined($var);
        $var = lc($var);
        die "Error: undefined debug var in $file, line ${.}. Use either #<If:VAR> or #<Debugvar:VAR>\n"
          unless defined($var);

        if (defined($toggle)) { $toggle =~ s%\$%${debugmodule}.%g; }
        else { $toggle = "${debugmodule}.default"; }

        print $linebeg.($release ? "/* " : " ")."if ($toggle)(${debugmodule}.$var) then ( ";
        $_ = "$lineend\n";

        nextline until /^(.*?)(#<(Else|End)>)(.*)$/;
        $linebeg = $1."/* $2 */ ) else".($release ? " */" : "")." ( ";
        $lineend = $4;

        if ($3 eq "Else") {
            print $linebeg;
            $_ = "$lineend\n";
            nextline until /^(.*)(#<End>)(.*)/;
            $line = "$1/* $2 */ ) $3\n";
        } else {
            $line = "$linebeg () ) $lineend\n";
        }
        print $line;
        return filterline($rec);
    }
    elsif ($line =~ /(#<(.*?)>)/ && $2 !~ /^Debugvar:[A-Z0-9_]+$/ && (!$rec || $2 !~ /^(Else|End)$/)) {
      die "Error: non-parsed ppdebug directive $1 in $file, line ${.}.\n";
    }
    else {
        return ($line);
    }
}

print while (defined ($_ = filterline));
