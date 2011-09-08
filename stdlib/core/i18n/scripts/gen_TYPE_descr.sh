TYPE=$1
FILE="language-subtag-registry"
wget -N http://www.iana.org/assignments/$FILE
grep -A 2 "Type: $TYPE" $FILE | grep Subtag | cut -d " " -f 2 > lang_subtag
grep -A 2 "Type: $TYPE" $FILE | grep Description | cut -d " " -f 2 > lang_descr
paste lang_subtag lang_descr | grep -v Private
rm -f lang_subtag lang_descr
