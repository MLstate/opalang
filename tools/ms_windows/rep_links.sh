#!/bin/bash

#set -x

W=$PWD

touch LINKS.log

rep_link(){
	#local TARGET="$(dirname $1)/$(cat $1)"
	local TARGET=$(readlink $1)
	#if [ -f $TARGET ];
	if [ -L $1 ];
	then
		#echo "LINK $1 => $TARGET"
		rm $1
		cp -f $(dirname $1)/$TARGET $1
		echo $1 >> $W/LINKS.log
	#else
	#	echo "Not a link $1 (=>$TARGET)"
	fi
}


# PATH with ocamlfind and using+
# Solve the links hell
replace_links(){
for dir in compiler/opalang compiler/protocols compiler/ocamllang lib/plugins ocamllib/libnet;
do
cd $dir
for ext in trx ml mli nodejs js;
do 
	local LINKS=$(find . -name "*.$ext" -type l)
	#echo "LINKS=$LINKS"
	for i in $LINKS;
	do
		rep_link $i
	done
done
cd -
done


}

replace_links
