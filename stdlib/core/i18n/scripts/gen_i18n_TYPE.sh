TYPE=$1
gen(){
echo "// File generated automatically with $*"
echo "// using http://www.iana.org/assignments/language-subtag-registry"
echo "// DO NOT EDIT"
echo
echo "/* This file contains the utility functions for all $1 subtag described by iana.org language-subtag-registry */"
#echo package i18n
echo type I18n.$TYPE =
./gen_TYPE_descr.sh $TYPE | xargs -d "\n" -n 1 ./gen_one_entry.sh
echo
echo "I18n_$TYPE = {{"
echo
echo "parse(s) : option(I18n.$TYPE) ="
echo "  check(s) = match s"
echo -n "  "
./gen_TYPE_descr.sh $TYPE | xargs -d "\n" -n 1 ./gen_one_parse_entry.sh
echo
echo  "    -> true"
echo  "  _ -> false"
echo  "  end"
echo  "  if check(s) then some(OpaValue.Record.make_simple_record(OpaValue.Record.field_of_name_unsafe(s))) else none"
echo
echo "to_string(k:I18n.$TYPE) =  OpaValue.Record.get_uniq_field_name(k) ? error(\"I18n.$TYPE.to_string\")"
echo
echo "}}"
}



gen > i18n_$TYPE.opa
