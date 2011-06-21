#!/bin/bash
#java-bench.lst obtained with: 
#   find /home/koper/soft/jdk-1_5_0-src-jrl/ -type f -name '*.java' ! -exec grep -q '#warn' {} \; -print > ./teerex/java/java-bench.lst 
#we ignore files containing #warn as they need some preprocessing

C=0
for i in `cat java-bench.lst`; do
    $1 $i > /dev/null
    [ $? -ne 0 ] && echo "ERROR while parsing $i" && exit 1
    let "C += 1"
done
echo "Parser $C files successfully..."
