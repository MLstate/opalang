TYPE=$1
printit(){
    a=$1
    shift
    echo -n "\"$a\""
}
#to unescape and split uniq arg
printit $*

