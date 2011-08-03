#!/bin/bash

export ROOT=`pwd`

# The name of the application
opatrack='opatrack'

# The location of cgi script for building evolution scripts
CGI_LOCATION='http://kahan/cgi-bin/hudson'

# Conventions over name of files and directories.
# <!> Keep it synchronized with module PassTrack.
passes_list='passes.list'
printers_list='printers.list'
trackers_list='trackers.list'
printers_dir='printers'
trackers_dir='trackers'
internal_dir='internal'
check_dir='check'
time_dir='time'
all_time='all.time'
size_file='size'

# Global variables
dir2_mode=0
entityident='entity'
html_trace="$entityident.html"
raw_trace="$entityident.opatrack"

set_entity_ident() {
    local entity="$1"
    entityident="$entity"
    html_trace="$entityident.html"
    raw_trace="$entityident.opatrack"
}

# Command line options.
# <!> Keep alphabetic order for maintenance.
COLOR=1
DEBUG=0
DIR=''
DIR2=''
LOGFILE=/dev/stdout
REPORT_MODE=0
TOOL=meld
TOOL_FLAGS=''
VIEWER=less
VIEWER_FLAGS=''
SUM=0
SUMTARGET=''

menuitem() {
    key="$1"
    shift
    msg="$*"
    if [ "$COLOR" -eq 1 ]; then
        printf "\t[34m%2s[0m : %s\n" "$key" "$msg"
    else
        printf "\t%2s : %s\n" "$key" "$msg"
    fi
}

title() {
    if [ "$COLOR" -eq 1 ]; then
        echo "[32m$*[0m"
    else
        echo "$*"
    fi
}

warn() {
    if [ "$COLOR" -eq 1 ]; then
        echo "[33m[1m$*[0m"
    else
        echo "$*"
    fi
}

# Help messsage
help() {
    cat <<EOF
$opatrack: the MLstate pass track analyser
Usage: $opatrack [options] ?directory ?directory2
Options:
        -h|--help        : displays this help
        --no-color       : disables colors
        -t|--tool        : set diff tool. (def is $TOOL)
        --tflag <opt>    : add to tool options <opt>
        -v|--viewer      : set diff viewer. (def is $VIEWER)
        --vflag <opt>    : add to viewer options <opt>
        --report         : do not start main, but report mode
        --entity-ident _ : set the entity ident (def is $entityident)
        --html _         : set the name of the html trace (def is entityident.html)
        --sum _ <dirs>   : set the directory target for suming everyother _tracks directory
        --debug          : debug mode (more output for reporting, or summing)
        --log _          : precise a log for outputing modes (default is /dev/stdout)

See the documentation of module PassTracker for more information.
EOF
}

# Arg.parse
while [ $# -gt 0 ]; do
    case $1 in
        -h|--help|-help|help)
            help
            exit 0
            ;;
        --no-color)
            COLOR=0
            ;;
        -t|--tool)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            TOOL="$1"
            ;;
        --tflag)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            TOOL_FLAGS="$TOOL_FLAGS $1"
            ;;
        -v|--viewer)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            VIEWER="$1"
            ;;
        --vflag)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            VIEWER_FLAGS="$VIEWER_FLAGS $1"
            ;;
        --report)
            REPORT_MODE=1
            ;;
        --entity-ident)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            set_entity_ident "$1"
            ;;
        --html)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            html_trace="$1"
            ;;
        --sum)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            SUM=1
            SUMTARGET="$1"
            shift
            DIR="$@"
            break
            ;;
        --debug)
            DEBUG=1
            ;;
        --log)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            LOGFILE=$1
            ;;
        *)
            if [ -z "$DIR" ] ; then
                DIR="$1"
            else
                if [ -z "$DIR2" ] ; then
                    DIR2="$1"
                    dir2_mode=1
                else
                    warn "$opatrack: unrecognized option '$1'"
                    warn "Try '$opatrack --help' for more information."
                    exit 1
                fi
            fi
            ;;
    esac
    shift
done

sum_size() {
    local A=$1
    local B=$2
    local T=$3
    local line
    rm -f $T
    while read number field ; do
        line=$(cat $B | grep $field)
        number2=$(echo $line | cut -d' ' -f1)
        number=$(echo "$number + $number2" | bc)
        if [ -n "$number" ] ; then
            echo "$number $field" >> $T
        fi
    done < $A
}

get_line() {
    local file=$1
    local line=$2
    line=$(head -n $line $file | tail -n 1)
    echo $line
}

merge_files() {
    local private_FILE_I=$1
    local private_FILE_J=$2
    local private_I=1
    local private_J=1
    local private_FILE_I_LEN=$(cat $private_FILE_I | wc -l)
    local private_FILE_J_LEN=$(cat $private_FILE_J | wc -l)
    local LINES=''
    local line_I=''
    local line_J=''
    local rest_I=0
    local rest_J=0
    local succ_I=0
    local succ_J=0

    while [ 1 ] ; do
        if (($private_I > $private_FILE_I_LEN)) && (($private_J > $private_FILE_J_LEN)) ; then
            break;
        fi
        if (($private_I > $private_FILE_I_LEN)) ; then
            local rest_J=$(($private_FILE_J_LEN - $private_J + 1))
            tail -n $rest_J $private_FILE_J
            break;
        fi
        if (($private_J > $private_FILE_J_LEN)) ; then
            local rest_I=$(($private_FILE_I_LEN - $private_I + 1))
            tail -n $rest_I $private_FILE_I
            break;
        fi
        line_I=$(get_line $private_FILE_I $private_I)
        line_J=$(get_line $private_FILE_J $private_J)
        if [ "$line_I" = "$line_J" ] ; then
            echo "$line_I"
            let "private_I=$private_I+1"
            let "private_J=$private_J+1"
        else
            # depending of a simple heuristic, priority to left or to right
            local succ_I=$(get_line $private_FILE_I $(($private_I+1)))
            local succ_J=$(get_line $private_FILE_J $(($private_J+1)))
            if [ "$line_I" = "$succ_J" ] ; then
                echo "$line_J"
                let "private_J=$private_J+1"
            elif [ "$line_J" = "$succ_I" ] ; then
                echo "$line_I"
                let "private_I=$private_I+1"
            else
                # warning, will probably skip some lines
                echo "$line_I"
                let "private_I=$private_I+1"
            fi
        fi
    done
    echo $LINES
}

sum2() {
    local A=$1
    local B=$2
    local T=$3
    local pass ta tb tc

    local PASSES=$(merge_files $A/$passes_list $B/$passes_list)
    mkdir -p $T/$time_dir
    cp $A/*.list $T/
    rm -f $T/$passes_list
    if [ -f $A/$time_dir/$all_time ] && [ -f $B/$time_dir/$all_time ] ; then
        echo "$(cat $A/$time_dir/$all_time) + $(cat $B/$time_dir/$all_time)" | bc > $T/$time_dir/$all_time
    fi
    for pass in $PASSES ; do
        if [ -f $A/$time_dir/$pass.time ] && [ -f $B/$time_dir/$pass.time ] ; then
            echo "$(cat $A/$time_dir/$pass.time) + $(cat $B/$time_dir/$pass.time)" | bc > $T/$time_dir/$pass.time
            echo $pass >> $T/$passes_list
        else
            if [ -f $A/$time_dir/$pass.time ] ; then
                # echo "only A for $pass"
                cat $A/$time_dir/$pass.time > $T/$time_dir/$pass.time
                echo $pass >> $T/$passes_list
            elif [ -f $B/$time_dir/$pass.time ] ; then
                # echo "only B for $pass"
                cat $B/$time_dir/$pass.time > $T/$time_dir/$pass.time
                echo $pass >> $T/$passes_list
            fi
        fi
        if [ -f $A/$pass/$printers_dir/size ] && [ -f $B/$pass/$printers_dir/size ] ; then
            mkdir -p $T/$pass/$printers_dir
            sum_size $A/$pass/$printers_dir/size $B/$pass/$printers_dir/size $T/$pass/$printers_dir/size
        fi
        if [ -f $A/$pass/$printers_dir/size_client ] && [ -f $B/$pass/$printers_dir/size_client ] ; then
            mkdir -p $T/$pass/$printers_dir
            sum_size $A/$pass/$printers_dir/size_client $B/$pass/$printers_dir/size_client $T/$pass/$printers_dir/size_client
        fi
    done
}

sumN() {
    local T=$1
    shift
    local N=$@
    local first acc target
    local x=$(echo $N | wc -w)
    first=$(echo $N | cut -d' ' -f1)
    acc=$(mktemp)
    rm -rf $acc
    mkdir -p $acc
    cp -r $first/* $acc/
    N=$(echo $N | cut -d' ' -f2-$x)
    for d in $N; do
        tmp1=$(mktemp)
        rm -rf $tmp1
        sum2 $acc $d $tmp1
        rm -rf $acc
        acc=$tmp1
    done
    mv $tmp1 $T
}

if [ "$SUM" = 1 ] ; then
    sumN $SUMTARGET $DIR
    exit 0
fi

# Utils
quit() {
    warn "Thank you for using $opatrack !"
    exit 0
}

if [ "$COLOR" -eq 1 ]; then
    TRACKPATH="[31m$opatrackm:[31m[0m\$ "
else
    TRACKPATH="$opatrack:\$ "
fi
set_track_path() {
    if [ "$COLOR" -eq 1 ]; then
        TRACKPATH="[31m$opatrack[0m:[33m/$1[31m[0m\$ "
    else
        TRACKPATH="$opatrack:/$1\$ "
    fi
}

concat_path() {
    local p1="$1"
    local p2="$2"
    if [ -z "$p1" ] ; then
        echo "$p2"
    else
        echo "$p1/$p2"
    fi
}

check_dir() {
    if [ ! -d "$1" ] ; then
        warn "[!] $opatrack: cannot find directory $1"
        exit 1
    fi
}

check_file() {
    if [ ! -f "$1" ] ; then
        warn "[!] $opatrack: cannot find file $1"
        exit 1
    fi
}

length_of_file() {
    wc -l $1 | sed -e 's/ *\(.*\)/\1/g' | cut -d' ' -f1
}

assert_eq() {
    if [ ! "$1" = "$2" ] ; then
        warn "$opatrack: internal error. assertion failure ($1 != $2)"
        exit 5
    fi
}

eof_check() {
    if [ "$1" = 1 ] && [ -z "$REPLY" ] ; then echo ; quit ; fi
    if [ "$REPLY" = "q" ] ; then echo ; quit ; fi
}

get_size() {
    # %d declarations
    # %d nodes
    local file="$1"
    if [ -e "$file" ] ; then
        local declarations=$(cat $file | grep 'declarations' | cut -d' ' -f1)
        local nodes=$(cat $file | grep 'nodes' | cut -d' ' -f1)
        echo $declarations $nodes
    else
        echo 0 0
    fi
}

# Evolution ratio between a last value, and a current value.
# the value is expressed like this :
# +3.4 means 1 * 3.4
# -2.5 means 1 / 2.5
# @param $1 last
# @param $2 current
evolution_ratio() {
    last=$1
    this=$2
    if [ ! -n "$last" ] || [ ! -n "$this" ] || [ "$last" = 0 ] || [ "$this" = 0 ] ; then
        echo 'nan'
        return
    fi

    ratio=$(echo "scale=4; $this / $last" | bc)
    if [ $(echo "$ratio < 1" | bc) = 1 ] ; then
        ratio=$(echo "scale=4; 1 / $ratio" | bc)
        echo "-$ratio"
    else
        echo "+$ratio"
    fi
}

private_fresh=0
fresh_index() {
    private_fresh=$(echo "$private_fresh + 1" | bc)
}

evolution_item() {
    local CGI_CALL="$1"
    shift
    local label="$1"
    shift
    local data="$*"
    fresh_index
    local this_fresh=$private_fresh
    local this_cgi_call=$CGI_CALL"&label=$label"
    cat <<EOF
      <b>$label</b> : $data
      [
      <a href='#' onclick='javascript:document.getElementById("evolution_$this_fresh").innerHTML=document.getElementById("evolution_$this_fresh").innerHTML != "" ? "" : "<object width=700px height=500px data=$this_cgi_call></object>"; return false;'>evolution</a>
      ]
      <div id="evolution_$this_fresh"></div>
EOF
}

# avoid to print .54 instead of 0.54
float_escaped() {
    local float="$1"
    if [ $(echo "$float < 1" | bc) = "1" ] ; then
        float="0"$float
    fi
    echo "$float"
}

# grep_indexed_auto_selection will select between args
# writting in the tempfile, and returning 0 if ok, 1 if KO
auto_selected_file=$(mktemp /tmp/fileXXXXXX)
grep_indexed_auto_selection() {
    echo '' > $auto_selected_file
    local ALL="$@"
    declare -a tab
    local loop=1
    local FILES="$ALL"
    if [ -z "$ALL" ] ; then
        return 1
    fi
    local IS_FILTER=0
    while [ "$loop" = 1 ] ; do
        loop=0
        i=0
        tab[1]=''
        count=$(echo $FILES | wc -w | sed -e 's/ *\(.*\)/\1/g')
        if [ "$count" -gt 150 ] ; then
            echo "There are $count elts, please give a <regexp> !"
            read -p "$TRACKPATH"
            eof_check "$?"
            rep="$REPLY"
            if [ "$rep" = "!" ] ; then break ; fi
            IS_FILTER=1
            FILES=$(for file in $FILES; do echo $file ; done | grep $rep)
            loop=1
            continue
        fi
        title "Selection:"
        for file in $FILES ; do
            let "i=$i+1"
            menuitem $i "$file"
            tab[$i]=$file
        done
        if [ "$i" = 0 ] ; then
            loop=1
            FILES="$ALL"
            IS_FILTER=0
            continue
        fi
        if [ "$i" = 1 ] ; then
            echo "${tab[1]}" > $auto_selected_file
            return 0
        fi
        if [ "$i" -lt 10 ] ; then
            echo "Choose an index, (or '!')"
            read -p "$TRACKPATH" -en1
            eof_check "$?"
        else
            echo "Choose an index, or '<regexp>' for filtering or '!' for review all"
            read -p "$TRACKPATH"
            eof_check "$?"
        fi
        rep=$REPLY
        if [ -z "$rep" ] ; then
            loop=1
            continue
        fi
        if [ ! "$rep" = "!" ] && [[ "$rep" != [0-9]* ]]; then
            IS_FILTER=1
            FILES=$(for file in $FILES; do echo $file ; done | grep $rep)
            loop=1
            continue
        else
            if [ "$rep" = "!" ] ; then
                if [ "$IS_FILTER" = 1 ] ; then
                    loop=1
                    FILES="$ALL"
                    IS_FILTER=0
                    continue
                else
                    return 1
                fi
            else
                if [[ "$rep" != [0-9]* ]]; then
                    warn "integer expected : $rep"
                    loop=1
                    continue
                else
                    choice="${tab[$rep]}"
                    if [ -n "$choice" ] ; then
                        echo "$choice" > $auto_selected_file
                        return 0
                    else
                        warn "[!] $rep : no such index."
                        loop=1
                    fi
                fi
            fi
        fi
    done
}

# 1 Retrieve informations

# 1.1 Find directory
# Get to the directory if specified
# If the directory ends with .opx, then go $DIR/_tracks instead
# Can be absolute or relative. Try both
case "$DIR" in
*.opx) DIR="$DIR/_tracks";;
esac
if [ ! -d "$ROOT/$DIR" ] ; then
    check_dir "$DIR"
else
    if [ -n "$DIR" ] ; then
        DIR="$ROOT/$DIR"
    else
        DIR="$ROOT"
    fi
fi

if [ "$dir2_mode" = 1 ] ; then
    case "$DIR2" in
        *.opx) DIR2="$DIR2/_tracks";;
    esac
    if [ ! -d "$ROOT/$DIR2" ] ; then
        check_dir "$DIR2"
    else
        if [ -n "$DIR2" ] ; then
            DIR2="$ROOT/$DIR2"
        else
            DIR2="$ROOT"
        fi
    fi
fi

# 1.2 Check for .list conf
passes_list="$DIR/$passes_list"
printers_list="$DIR/$printers_list"
trackers_list="$DIR/$trackers_list"
check_file "$passes_list"
check_file "$printers_list"
check_file "$trackers_list"

passes_len=$(length_of_file $passes_list)
printers_len=$(length_of_file $printers_list)
trackers_len=$(length_of_file $trackers_list)

# 1.3 Build tables for indexed access
declare -a passes_table
declare -a printers_table
declare -a trackers_table
declare -a trackers_tracked_table
declare -a trackers_length_table
ALL_PASSES=''
ALL_PRINTERS=''
ALL_TRACKERS=''

i=0
while read passname ; do
    let "i=$i+1"
    passes_table[$i]=$passname
done < $passes_list
ALL_PASSES=$(cat $passes_list)
assert_eq "$i" "$passes_len"

i=0
while read printer_id ; do
    let "i=$i+1"
    printers_table[$i]=$printer_id
done < $printers_list
ALL_PRINTERS=$(cat $printers_list)
assert_eq "$i" "$printers_len"

i=0
while read tracker_id ; do
    let "i=$i+1"
    trackers_table[$i]=$tracker_id
done < $trackers_list
ALL_TRACKERS=$(cat $trackers_list)
assert_eq "$i" "$trackers_len"

# collect all tracked values for a given tracker_id
for (( i=1; i<=$trackers_len; i++ )); do
    tracker_id="${trackers_table[$i]}"
    tracked_file="$DIR/$trackers_dir/$tracker_id.list"
    length=$(length_of_file $tracked_file)
    trackers_length_table[$i]="$length"
    trackers_tracked_table[$i]=$(cat $tracked_file)
done

print_tree() {
    title "Passes:"
    for (( i=1; i<=$passes_len; i++ )); do
        menuitem $i "${passes_table[$i]}"
    done

    title "Printers:"
    if [ "$printers_len" -gt 5 ] ; then
        echo -e "\t$printers_len printers"
    else
        for (( i=1; i<=$printers_len; i++ )); do
            menuitem $i "${printers_table[$i]}"
        done
    fi

    title "Trackers:"
    if [ "$trackers_len" -gt 5 ] ; then
        echo -e "\t$trackers_len trackers"
    else
        for (( i=1; i<=$trackers_len; i++ )); do
            menuitem $i "${trackers_table[$i]} (${trackers_length_table[$i]} elts)"
        done
    fi
}

index_pass() {
    local passname="$1"
    for (( i=1; i<=$passes_len; i++ )); do
        if [ "${passes_table[$i]}" = "$passname" ] ; then
            echo $i
            return 0
        fi
    done
    return 1
}

index_printer() {
    local printer_id="$1"
    for (( i=1; i<=$printers_len; i++ )); do
        if [ "${printers_table[$i]}" = "$printer_id" ] ; then
            echo $i
            return 0
        fi
    done
    return 1
}

index_tracker() {
    local tracker_id="$1"
    for (( i=1; i<=$trackers_len; i++ )); do
        if [ "${trackers_table[$i]}" = "$tracker_id" ] ; then
            echo $i
            return 0
        fi
    done
    return 1
}

# 2 Pre computation is done. Start the main loop
# 2.1 Some common help manual
general_help() {
cat <<EOF
Welcome to opatrack.
This script is meant to speed-up the diffing between expressions
along passes. You can choose between several modes.

Whenever you get request for an answer :
'?' : get some help about the specific local menu
'!' : get back to the backward menu
'q' : quit opatrack.

Have a nice bug tracking with opatrack !
EOF
}

# Add new modes there if you need.
# GUIDELINES ABOUT MODES :
# + should start a loop, and print $opatrack/$mode ~:$ before inviting read
# + they can starts sub-mode, and should not exit when finished (get back to the parent mode) (except if 'q' is read)
# + they should be documented, and provide an introduction, and a short helpfull help messages resuming command
# + they should return and get back to the parent if '!' is pressed.
# + if you add sub modes, the function should be prefixed with the name of the mode
# + read function should prefix request with TRACKPATH value
# + menus should be cool (hahaha)

# tool (meld)
launch_meld() {
    local file1="$1"
    local file2="$2"
    local file3="$3"
    local command=''

    if [ -n "$file3" ] ; then
        command="$TOOL $TOOL_FLAGS $file1 $file2 $file3"
    else
        command="$TOOL $TOOL_FLAGS $file1 $file2"
    fi
    echo "$command"
    $command
}

# viewer (less)
launch_less() {
    local file1="$1"
    local command=''
    command="$VIEWER $VIEWER_FLAGS $file1"
    echo "$command"
    $command
}

filtering_passes() {
    local file="$1"
    local pass
    for pass in $ALL_PASSES ; do
        if [ -e "$DIR/$pass/$file" ] ; then
            echo $pass
        fi
    done
}

# GENERIC NODE/LEAF management

# generic shema for the an application_node
# copy past it, and change name with a fresh name
application_node_name() {
    # dealing with path
    local selfpath='name'

    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================

    # other args are options of the node.
    help_for_name() {
        menuitem "a" "blabla"
        menuitem "b" "bloblo"
    }

    # introduction
    cat <<EOF
This is a help message which is printed only once, whenever
we enter the node. After that the loop will start.
EOF
    while [ 1 ] ; do
        title "Node"
        help_for_name

        # GENERIC ===================
        set_track_path "$selfpath"  #
        read -p "$TRACKPATH" -en1   #
        eof_check "$?"              #
        case $REPLY in              #
        # ===========================

            # insert there call to other node or leaf
            a)
                application_node_child "$selfpath" # optionnal args
                ;;
            !)
                return
                ;;
            *) # will lead to loop, and rewrite the help
                ;;
        esac
    done
}

# This node is for comparing _tracks from 2 different branches
# of the compiler (usefull for comparing code after a huge modif)
node_meld_2bran() {
    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    # get arguments
    local SUBDIR="$1"
    local FILE_FOR_MELD=$(concat_path "$SUBDIR" "$2")
    # dealing with path
    local selfpath='2bran'
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================

    # other args are options of the node.
    help_for_meld_2bran() {
        menuitem "!" "quit meld 2bran mode"
        menuitem "q" "quit opatrack"
    }

    if [ "$passes_len" -le 0 ] ; then
        warn "No pass: This mode is not available"
        return
    fi

    title "Meld 2bran: $FILE_FOR_MELD"
    local FILTERING_PASSES=$(filtering_passes "$FILE_FOR_MELD")
    local number_filtering=$(echo $FILTERING_PASSES | wc -w | sed -e 's/ *\(.*\)/\1/g')
    while true ; do
        set_track_path "$selfpath"
        help_for_meld_2bran
        echo "Choose the pass:"
        grep_indexed_auto_selection "$FILTERING_PASSES"
        code=$?
        if [ ! "$code" = 0 ] ; then
            return
        fi
        pass=$(cat $auto_selected_file)
        index_pass=$(index_pass $pass)
        code=$?
        if [ ! "$code" = 0 ] ; then
            warn "Pass: $pass Not found (i=$index_pass)"
            continue
        fi

        file1="$DIR/$pass/$FILE_FOR_MELD"
        file2="$DIR2/$pass/$FILE_FOR_MELD"

        # check that the same file also exists in the other branch
        if [ ! -f "$file2" ] ; then
            warn "File: $file2 Not available"
            continue
        fi

        if diff -q $file1 $file2 >/dev/null; then
            warn "The files are identical, not running $TOOL"
            warn "run $TOOL anyway (y/*) ?"
            read -p '' -n1
            eof_check "$?"
            echo ''
            if [ "$REPLY" = "y" ] ; then
                launch_meld "$file1" "$file2"
            fi
        else
            launch_meld "$file1" "$file2"
        fi

        if [ "$number_filtering" -le 1 ]; then
            break
        fi
    done
}

# find quickly the changes
node_meld_2chan() {
    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    # get arguments
    local SUBDIR="$1"
    local FILE_FOR_MELD=$(concat_path "$SUBDIR" "$2")
    local LOG="$3"
    # dealing with path
    local selfpath='2chan'
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================

    # other args are options of the node.
    help_for_meld_2chan() {
        menuitem "!" "quit meld 2chan mode"
        menuitem "q" "quit opatrack"
    }

    if [ "$passes_len" -le 0 ] ; then
        warn "No pass: This mode is not available"
        return
    fi

    title "Meld 2chan: $FILE_FOR_MELD"
    local FILTERING_PASSES=$(filtering_passes "$FILE_FOR_MELD")
    local number_filtering=$(echo $FILTERING_PASSES | wc -w | sed -e 's/ *\(.*\)/\1/g')
    set_track_path "$selfpath"
#    help_for_meld_2chan

    local no_changes=1
    local i=0

    if [ "$LOG" = 'LOG' ] ; then
        echo "list of passes with changes:"
    else
        # avoid to delete the previous line
        echo
    fi

    local always_meld=0

    for pass in $FILTERING_PASSES ; do

        let "i=$i+1"

        file1="$DIR/$pass/$FILE_FOR_MELD"
        file2="$DIR2/$pass/$FILE_FOR_MELD"

        # check that the same file also exists in the other branch
        if [ ! -f "$file2" ] ; then
            warn "File: $file2 Not available"
            continue
        fi

        if [ "$LOG" = 'LOG' ] ; then
            if ! diff -q $file1 $file2 >/dev/null; then
                no_changes=0
                echo "$pass" >> $LOGFILE
            fi

        else

            if diff -q $file1 $file2 >/dev/null; then
                printf "[F[2K[0G"
                printf "pass %d/%d (%s) : no changes\n" $i $number_filtering "$pass"
            else
                no_changes=0
                echo "$pass: changes"
                if [ "$always_meld" = 1 ] ; then
                    launch_meld "$file1" "$file2"
                else
                    warn "run $TOOL (y/a/*) ?"
                    read -p '' -n1
                    eof_check "$?"
                    echo ''
                    case $REPLY in
                        y)
                            launch_meld "$file1" "$file2"
                            ;;
                        a)
                            always_meld=1
                            launch_meld "$file1" "$file2"
                            ;;
                        !)
                            return
                            ;;
                        *)
                            ;;
                    esac
                fi
            fi
        fi
    done

    if [ "$no_changes" = 1 ] ; then
        echo "no changes on $FILE_FOR_MELD"
    fi
}

node_2branches() {
    # dealing with path
    local selfpath='2bran'

    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    local printers_dir=$1
    local FILE_FOR_MELD=$2
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================

    # other args are options of the node.
    help_for_name() {
        menuitem "d" "2-bran sub-mode"
        menuitem "c" "2-chan sub-mode"
        menuitem "l" "2-log sub-mode"
        menuitem "!" "quit printers mode"
        menuitem "q" "quit opatrack"
    }

    while [ 1 ] ; do
        title "2branches"
        help_for_name

        # GENERIC ===================
        set_track_path "$selfpath"  #
        read -p "$TRACKPATH" -en1   #
        eof_check "$?"              #
        case $REPLY in              #
        # ===========================

            # insert there call to other node or leaf
            d)
                node_meld_2bran "$selfpath" "$printers_dir" "$FILE_FOR_MELD"
                ;;
            c)
                node_meld_2chan "$selfpath" "$printers_dir" "$FILE_FOR_MELD"
                ;;
            l)
                node_meld_2chan "$selfpath" "$printers_dir" "$FILE_FOR_MELD" 'LOG'
                ;;
            !)
                return
                ;;
            *) # will lead to loop, and rewrite the help
                ;;
        esac
    done
}

node_meld_2cons() {
    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    # get arguments
    local SUBDIR="$1"
    local FILE_FOR_MELD=$(concat_path "$SUBDIR" "$2")
    local FREE="$3"
    # dealing with path
    local selfpath=''
    if [ "$FREE" = 'FREE' ] ; then
        selfpath='2free'
    else
        selfpath='2cons'
    fi
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================

    # other args are options of the node.
    help_for_meld_2cons() {
        menuitem "!" "quit meld 2cons mode"
        menuitem "q" "quit opatrack"
    }

    if [ "$passes_len" -le 1 ] ; then
        warn "Only 1 pass: Melds mode are not available"
        return
    fi

    title "Meld 2cons: $FILE_FOR_MELD"
    local FILTERING_PASSES=$(filtering_passes "$FILE_FOR_MELD")
    local number_filtering=$(echo $FILTERING_PASSES | wc -w | sed -e 's/ *\(.*\)/\1/g')
    while true ; do
        set_track_path "$selfpath"
        help_for_meld_2cons
        echo "Choose the left pass:"
        grep_indexed_auto_selection "$FILTERING_PASSES"
        code=$?
        if [ ! "$code" = 0 ] ; then
            return
        fi
        pass1=$(cat $auto_selected_file)
        index_pass1=$(index_pass $pass1)
        code=$?
        if [ ! "$code" = 0 ] ; then
            warn "Pass: $pass1 Not found (i=$index_pass1)"
            continue
        fi
        if [ "$index_pass1" = "$passes_len" ] ; then
            warn "[!] 2cons mode cannot be runned from last pass"
            continue
        fi

        if [ "$FREE" = 'FREE' ] ; then
            echo "Choose the right pass:"
            grep_indexed_auto_selection "$ALL_PASSES"
            code=$?
            if [ ! "$code" = 0 ] ; then
                return
            fi
            pass2=$(cat $auto_selected_file)
            index_pass2=$(index_pass $pass2)
            code=$?
            if [ ! "$code" = 0 ] ; then
                warn "Pass: $pass2 Not found (i=$index_pass2)"
                continue
            fi
        else
            index_pass2=$(($index_pass1+1))
            pass2=${passes_table[$index_pass2]}
        fi

        file1="$DIR/$pass1/$FILE_FOR_MELD"
        file2="$DIR/$pass2/$FILE_FOR_MELD"

        if diff -q $file1 $file2 >/dev/null; then
            warn "This pass caused no changes, not running $TOOL"
            warn "run $TOOL anyway (y/*) ?"
            read -p '' -n1
            eof_check "$?"
            echo ''
            if [ "$REPLY" = "y" ] ; then
                launch_meld "$file1" "$file2"
            fi
        else
            launch_meld "$file1" "$file2"
        fi

        if [ "$number_filtering" -le 2 ]; then
            break
        fi
    done
}

node_meld_3cons() {
    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    # get arguments
    local SUBDIR="$1"
    local FILE_FOR_MELD=$(concat_path "$SUBDIR" "$2")
    local FREE="$3"
    # dealing with path
    local selfpath=''
    if [ "$FREE" = 'FREE' ] ; then
        selfpath='3free'
    else
        selfpath='3cons'
    fi
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================


    # other args are options of the node.
    help_for_meld_3cons() {
        menuitem "!" "quit meld 2cons mode"
        menuitem "q" "quit opatrack"
    }

    if [ "$passes_len" -le 2 ] ; then
        warn "Only $passes_len pass(es): 3cons mode is not available"
        return
    fi

    title "Meld 3cons: $FILE_FOR_MELD"
    local FILTERING_PASSES=$(filtering_passes "$FILE_FOR_MELD")
    local number_filtering=$(echo $FILTERING_PASSES | wc -w | sed -e 's/ *\(.*\)/\1/g')
    while true ; do
        set_track_path "$selfpath"
        help_for_meld_3cons
        echo "Choose the first pass:"
        grep_indexed_auto_selection "$FILTERING_PASSES"
        code=$?
        if [ ! "$code" = 0 ] ; then
            return
        fi
        pass1=$(cat $auto_selected_file)
        index_pass1=$(index_pass $pass1)
        code=$?
        if [ ! "$code" = 0 ] ; then
            warn "Pass: $pass1 Not found (i=$index_pass1)"
            continue
        fi
        if [ "$index_pass1" -gt $(($passes_len - 2)) ] ; then
            warn "[!] 3cons mode cannot be runned from 2 last passes"
            continue
        fi

        if [ "$FREE" = 'FREE' ] ; then
            echo "Choose the midle pass:"
            grep_indexed_auto_selection "$ALL_PASSES"
            code=$?
            if [ ! "$code" = 0 ] ; then
                return
            fi
            pass2=$(cat $auto_selected_file)
            index_pass2=$(index_pass $pass2)
            code=$?
            if [ ! "$code" = 0 ] ; then
                warn "Pass: $pass2 Not found (i=$index_pass2)"
                continue
            fi
            echo "Choose the right pass:"
            grep_indexed_auto_selection "$ALL_PASSES"
            code=$?
            if [ ! "$code" = 0 ] ; then
                return
            fi
            pass3=$(cat $auto_selected_file)
            index_pass3=$(index_pass $pass3)
            code=$?
            if [ ! "$code" = 0 ] ; then
                warn "Pass: $pass3 Not found (i=$index_pass3)"
                continue
            fi
        else
            index_pass2=$(($index_pass1+1))
            pass2=${passes_table[$index_pass2]}

            index_pass3=$(($index_pass1+2))
            pass3=${passes_table[$index_pass3]}
        fi

        launch_meld "$DIR/$pass1/$FILE_FOR_MELD" "$DIR/$pass2/$FILE_FOR_MELD" "$DIR/$pass3/$FILE_FOR_MELD"

        if [ "$number_filtering" -le 3 ]; then
            break
        fi
    done
}

node_printers() {
    # dealing with path
    local selfpath='printers'

    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================
    local FILE_FOR_MELD=''

    # other args are options of the node.
    help_for_printers() {
        menuitem "2" "2-cons sub-mode"
        menuitem "3" "3-cons sub-mode"
        menuitem "f" "2-free sub-mode"
        menuitem "g" "3-free sub-mode"
        if [ "$dir2_mode" = 1 ] ; then
            menuitem "d" "2-branches mode"
        fi
        menuitem "!" "quit printers mode"
        menuitem "q" "quit opatrack"
    }

    local all_printers_path="$selfpath"

    if [ "$printers_len" -le 0 ] ; then
        warn "No printer available"
        return
    fi

    while [ 1 ] ; do
        set_track_path "$all_printers_path"
        # select the file
        grep_indexed_auto_selection "$ALL_PRINTERS"
        FILE_FOR_MELD=$(cat $auto_selected_file)

        if [ -z "$FILE_FOR_MELD" ] ; then
            warn "No file to meld."
            return
        fi

        # introduction
        selfpath="$all_printers_path/$FILE_FOR_MELD"
        while [ 1 ] ; do
            title "Printer: $FILE_FOR_MELD"
            help_for_printers

            # GENERIC ===================
            set_track_path "$selfpath"  #
            read -p "$TRACKPATH" -en1   #
            eof_check "$?"              #
            case $REPLY in              #
                # ===========================

                # insert there call to other node or leaf
                2)
                    node_meld_2cons "$selfpath" "$printers_dir" "$FILE_FOR_MELD" 'CONS'
                    ;;
                3)
                    node_meld_3cons "$selfpath" "$printers_dir" "$FILE_FOR_MELD" 'CONS'
                    ;;
                f)
                    node_meld_2cons "$selfpath" "$printers_dir" "$FILE_FOR_MELD" 'FREE'
                    ;;
                g)
                    node_meld_3cons "$selfpath" "$printers_dir" "$FILE_FOR_MELD" 'FREE'
                    ;;
                d)
                    if [ "$dir2_mode" = 0 ] ; then
                        echo "2branches mode is not available"
                    else
                        node_2branches "$selfpath" "$printers_dir" "$FILE_FOR_MELD"
                    fi
                    ;;
                !)
                    break
                    ;;
                *) # will lead to loop, and rewrite the help
                    ;;
            esac
        done
    done
}

node_trackers_this() {
    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    # get arguments
    local tracker_id="$1"
    # dealing with path
    local selfpath=$(concat_path "$parentpath" "$tracker_id") #
    set_track_path "$selfpath"                        #
    # =================================================
    local FILE_FOR_MELD=''

    # select the tracked values
    local tracker_id_index=$(index_tracker "$tracker_id")
    local code=$?
    if [ ! $code = 0 ] ; then
        warn "Unknown tracker_id: $tracker_id"
        return
    fi
    local ALL_TRACKED="${trackers_tracked_table[$tracker_id_index]}"

    local this_tracker_path="$selfpath"

    # other args are options of the node.
    help_for_trackers_this() {
        menuitem "2" "2-cons sub-mode"
        menuitem "3" "3-cons sub-mode"
        menuitem "f" "2-free sub-mode"
        menuitem "g" "3-free sub-mode"
        menuitem "!" "quit printers mode"
        menuitem "q" "quit opatrack"
    }

    while [ 1 ] ; do
        set_track_path "$this_tracker_path"
        grep_indexed_auto_selection "$ALL_TRACKED"
        FILE_FOR_MELD=$(cat $auto_selected_file)

        if [ -z "$FILE_FOR_MELD" ] ; then
            warn "No file to meld."
            return
        fi

        selfpath=$(concat_path "$this_tracker_path" "$FILE_FOR_MELD") #

        # introduction
        while [ 1 ] ; do
            title "Tracker $tracker_id: Tracked --> '$FILE_FOR_MELD'"
            help_for_trackers_this

            # GENERIC ===================
            set_track_path "$selfpath"  #
            read -p "$TRACKPATH" -en1   #
            eof_check "$?"              #
            case $REPLY in              #
                # ===========================

                # insert there call to other node or leaf
                2)
                    node_meld_2cons "$selfpath" "$trackers_dir/$tracker_id" "$FILE_FOR_MELD" 'CONS'
                    ;;
                3)
                    node_meld_3cons "$selfpath" "$trackers_dir/$tracker_id" "$FILE_FOR_MELD" 'CONS'
                    ;;
                f)
                    node_meld_2cons "$selfpath" "$trackers_dir/$tracker_id" "$FILE_FOR_MELD" 'FREE'
                    ;;
                g)
                    node_meld_3cons "$selfpath" "$trackers_dir/$tracker_id" "$FILE_FOR_MELD" 'FREE'
                    ;;
                !)
                    break
                    ;;
                *) # will lead to loop, and rewrite the help
                    ;;
            esac
        done
    done
}

node_trackers() {
    # dealing with path
    local selfpath='trackers'

    # GENERIC =========================================
    local parentpath="$1"                             #
    shift                                             #
    selfpath=$(concat_path "$parentpath" "$selfpath") #
    set_track_path "$selfpath"                        #
    # =================================================

    # select the tracker_id
    while [ 1 ] ; do
        set_track_path "$selfpath"  #
        title "Choose a tracker"
        grep_indexed_auto_selection "$ALL_TRACKERS"
        tracker_id=$(cat $auto_selected_file)

        if [ -z "$tracker_id" ] ; then
            warn "No such tracker."
            return
        fi

        node_trackers_this "$selfpath" "$tracker_id"
    done
}

main() {
    local selfpath=''
    help_for_main() {
        menuitem 'p' "printers"
        menuitem 't' "trackers"
        menuitem '@' "at pass"
        menuitem 'i' "internal errors"
        menuitem 'c' "conditions"
        menuitem 'x' "print tree"
        menuitem 'h' "general help"
        menuitem '?' "help"
        menuitem 'q' "quit"
    }
    print_tree
    while [ 1 ] ; do
        title "Choose a mode to run"
        help_for_main

        # GENERIC ===================
        set_track_path "$selfpath"  #
        read -p "$TRACKPATH" -en1   #
        eof_check "$?"              #
        case $REPLY in              #
        # ===========================
            p)
                node_printers "$selfpath"
                ;;
            t)
                node_trackers "$selfpath"
                ;;
            @)
                node_passes "$selfpath"
                ;;
            i)
                node_internal "$selfpath"
                ;;
            c)
                node_check "$selfpath"
                ;;
            x)
                print_tree
                ;;
            h)
                general_help
                read -p "press any key to continue..." -n1
                eof_check "$?"
                print_tree
                ;;
            !)
                return
                ;;
            *) # will lead to loop, and rewrite the help
                ;;
        esac
    done
}

# start the main menu
# todo : see in what case we are, add main2 for comparing 2 or 3 directories of compilation
if [ "$REPORT_MODE" = 0 ] ; then
    main
    exit 0
fi

# ===================================================================================
# ===================================================================================
# ===================================================================================

# this is the report mode

all_time_value=$(cat "$DIR/$time_dir/$all_time")
last_declarations=''
last_nodes=''

noninstant_passes=$(cat $passes_list | wc -l)
mean_time_ratio=$(bc <<<"scale=2; 100. / $noninstant_passes")

echo -n > $html_trace
echo -n > $raw_trace

# starts the raw trace with the global time
cat >> $raw_trace <<EOF
GLOBAL
Time $all_time_value

EOF

time_color () {
    local ratio=$1
    local value=$(bc -l <<<"256 * a($ratio / $mean_time_ratio) / (2*a(1))") # a() is arc-tangent
    local value=$(bc <<<"$value / 1")
    printf '#%02x00%02x' $value $((255 - value))
}

global_evolution_item () {
    local label=$1;
    local OBJECT="<object width=100% data=$CGI_LOCATION/opatrack-global.cgi?entityident=$entityident&label=$label></object>"
    cat <<EOF
<a href='#' onclick='javascript:document.getElementById("global_evolution").innerHTML = "$OBJECT"; return false;'>$label</a>
EOF
}

# transforms .12 into 0.12
# input is the standard input, not the command line
add_leading_zero() {
    sed -e 's/^\./0./'
}

# making bc output compatible with html
# which means a number before the decimal point
bc() {
    command bc "$@" | add_leading_zero
}

# same for the html trace
echo "<h4> date of creation : $(date)</h4>" >> $html_trace
echo "<h4> PassTracker : $entityident</h4>" >> $html_trace
CGI_CALL=$CGI_LOCATION"/opatrack.cgi?entityident=$entityident&passname=GLOBAL"
cat >> $html_trace <<EOF
<!-- GLOBAL -->
Raw log of the execution : <a href="$entityident.log"> there </a><br/>
Raw trace of tracker (e.g. for scripts) : <a href="$raw_trace"> there </a><br/>
<br/>
Show global evolution for [ $(S=""; for label in Time Declarations Nodes; do echo $S; global_evolution_item $label; S=" | "; done) ]<br/>
<div style="margin:auto; width:75%;" id="global_evolution"></div>
<br/>
<table>
<tr>
  <td><h4><a class="test" href="#" onclick="toggle_source(0); return false">Global Execution</a></h4></td>
  <td width="800px">
    <div style="border: 2px solid grey; margin-left: auto; padding: 0;" width="100%">
    <table style="border: none;" width="100%">
      <tr>
        <!-- TODO: the color depends on the historic red: slower than normal, magenta normal, and an other, quicker -->
        <td style="background: $(time_color "0.");" width="$mean_time_ratio%" height="10px"></td>
        <td style="background: $(time_color "100.");" width="$(bc <<<"scale=2; 100. - $mean_time_ratio")%" height="10px"></td>
      </tr>
    </table>
    <table style="border: none; border-top: 2px solid grey;" width="100%">
      <tr>
        <td style="background: green;" width="100%" height="10px"></td>
      </tr>
    </table>
    </div>
    <div class="test_source" id="test_source_0">
EOF

# This is the part which is toggled
evolution_item "$CGI_CALL" "Time" "$all_time_value" "sec" >> $html_trace

cat >> $html_trace <<EOF
    </div>
  </td>
</tr>
EOF

# first, going through all the passes to get the maximum
# number of nodes in the code
max_nodes=0
for ((i=1;i<=$passes_len;i++)); do
    passname="${passes_table[$i]}"
    pass_size_file="$DIR/$passname/$printers_dir/$size_file"
    pass_size=$(get_size $pass_size_file)
    nodes=$(echo $pass_size | cut -d' ' -f2)
    if ((max_nodes < nodes)); then max_nodes=$nodes; fi
done


for ((i=1;i<=$passes_len;i++)); do
    passname="${passes_table[$i]}"
    if [ -t 1 ]; then # only displaying when stdout is a tty
        [ $i -eq 1 ] || printf "[F[2K[0G" # go one line up, erase the whole line, go to 0th column
        printf "pass %d/%d (%s)\n" $i $passes_len "$passname"
    fi

    # time
    # echo "time"
    this_time=$(cat "$DIR/$time_dir/$passname.time")
    if [ ! -n "$this_time" ] ; then this_time="0" ; fi
    this_time_ratio=$(echo "scale=2; $this_time * 100 / $all_time_value" | bc)
    this_time_ratio_complement=$(echo "scale=2; 100 - $this_time_ratio" | bc)

    this_time_ratio=$(float_escaped "$this_time_ratio")
    this_time_ratio_complement=$(float_escaped "$this_time_ratio_complement")

    # size
    # echo "size"
    pass_size_file="$DIR/$passname/$printers_dir/$size_file"
    pass_size=$(get_size $pass_size_file)
    this_declarations=$(echo $pass_size | cut -d' ' -f1)
    this_nodes=$(echo $pass_size | cut -d' ' -f2)

    this_declarations_ratio=$(evolution_ratio $last_declarations $this_declarations)
    this_nodes_ratio=$(evolution_ratio $last_nodes $this_nodes)
    this_max_nodes_ratio=$(bc <<<"scale=2; $this_nodes * 100. / $max_nodes" 2>/dev/null)
    this_max_nodes_ratio_complement=$(bc <<<"scale=2; 100. - $this_max_nodes_ratio" 2>/dev/null)

    # speed per nodes
    # echo "speed"
    this_speed_per_node=$(bc <<< "scale=2; $this_nodes / $this_time" 2> /dev/null)

    last_declarations="$this_declarations"
    last_nodes="$this_nodes"


    # Raw trace
    # keep synchonized with label given in the html for cgi purpose
    cat >> $raw_trace <<EOF
$passname
Time $this_time
TimeRatio $this_time_ratio %
Declarations $this_declarations
DeclarationsRatio $this_declarations_ratio
Nodes $this_nodes
NodesRatio $this_nodes_ratio
MaxNodesRatio $this_max_nodes_ratio
SpeedPerNode $this_speed_per_node

EOF

    # Html trace
    # echo "html trace"
    CGI_CALL=$CGI_LOCATION"/opatrack.cgi?entityident=$entityident&passname=$passname"

    cat >> $html_trace <<EOF
<!-- PASS : $passname -->
<tr>
  <td><h4><a class="test" href="#" onclick="toggle_source($i); return false">$passname</a></h4></td>
  <td width="800px">
    <div style="border: 2px solid grey; margin-left: auto; padding: 0;" width="100%">
    <table style="border: none;" width="100%">
      <tr>
        <!-- TODO: the color depends on the historic red: slower than normal, magenta normal, and an other, quicker -->
        <td style="background: $(time_color $this_time_ratio);" width="$this_time_ratio%" height="10px"></td>
        $(if [ "$this_time_ratio_complement" != "0.0" ]; then
            echo "<td width=\"$this_time_ratio_complement%\" height=\"10px\"></td>"; fi)
      </tr>
    </table>
    <table style="border: none; border-top: 2px solid grey" width="100%">
      <tr>
        <td style="background: green;" width="$this_max_nodes_ratio%" height="10px"></td>
        $(if [ "$this_max_nodes_ratio_complement" != "0" ]; then
            echo "<td width=\"$this_max_nodes_ratio_complement%\" height=\"10px\"></td>"; fi)
      </tr>
    </table>
    </div>
    <div class="test_source" id="test_source_$i">
EOF

    # This is the part which is toggled
    # echo "evolution item"
    evolution_item "$CGI_CALL" "Time" "$this_time" "sec" >> $html_trace
    evolution_item "$CGI_CALL" "TimeRatio" "$this_time_ratio" "%" >> $html_trace
    evolution_item "$CGI_CALL" "Declarations" "$this_declarations" "decls" >> $html_trace
    evolution_item "$CGI_CALL" "DeclarationsRatio" "$this_declarations_ratio" >> $html_trace
    evolution_item "$CGI_CALL" "Nodes" "$this_nodes" "nodes" >> $html_trace
    evolution_item "$CGI_CALL" "NodesRatio" "$this_nodes_ratio" >> $html_trace
    evolution_item "$CGI_CALL" "MaxNodesRatio" "$this_max_nodes_ratio" >> $html_trace
    evolution_item "$CGI_CALL" "SpeedPerNode" "$this_speed_per_node" >> $html_trace

    cat >> $html_trace <<EOF
    </div>
  </td>
</tr>
EOF
done

cat >> $html_trace <<EOF
</table>
<br/>
EOF
