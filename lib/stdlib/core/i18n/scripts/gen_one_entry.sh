printit(){
    a=$1
    shift
    echo "/ {\`$a\`}\t // $*"
}
#to unescape and split uniq arg
printit $*

