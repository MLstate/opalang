set -e -x

# check wget

# Adding links to adapt the cygwin default installation
add_links(){
PREFIXGCC="/usr/bin/i686-w64-mingw32-"
for f in $PREFIXGCC*;
do
	#echo $f $DEST
	LINK=/usr/bin/${f#$PREFIXGCC}
	if [ ! -e $LINK ];
	then
		ln -s $f $LINK
	fi
done

if which java &>/dev/null; then
	echo Java is ok
else
	if which gij &>/dev/null; then
		echo gij link
    	ln -s /usr/bin/gij.exe /usr/bin/java.exe
    else
    	echo "Please install java or gij (Cygwin)"
    	sleep 3
	fi
fi
}

# helper functions to download and untar
go_in(){
	echo Installing $2 ...
	rm -rf $2
	wget -N $1$2.tar.gz
	tar xzvf $2.tar.gz
	cd $2
}

end_go_in(){
	cd -
}

FILE=setup.log

#cryptokit
cryptokit(){
go_in http://forge.ocamlcore.org/frs/download.php/891/ cryptokit-1.6
	ocamlfind remove cryptokit
	make build install
end_go_in
}


#ocamlssl, problem in second part with configure
ocamlssl(){
go_in http://www.openssl.org/source/ openssl-1.0.1c
	#echo Configuring
	make clean #> $ FILE
	perl Configure mingw shared --prefix=C:/ocamlmgw64 > $FILE
	#echo Compiling
	make #> $FILE
	#echo Installing
	make install #> $FILE # long
end_go_in
go_in https://github.com/savonet/ocaml-ssl/tarball/ ocaml-ssl-0.4.6
	cd ocaml-ssl
	./configure mingw
	echo TODO
end_go_in
}

camlzip(){
go_in http://zlib.net/ zlib-1.2.7
	INCLUDE_PATH=/usr/local/include LIBRARY_PATH=/usr/local/lib BINARY_PATH=/usr/local/bin make -f win32/Makefile.gcc all install
end_go_in
go_in http://forge.ocamlcore.org/frs/download.php/1037/ camlzip-1.05
	ocamlfind remove zip
	ocamlfind remove ocamlzip
	sed -i -e "s/*.cma/*.cmxa *.cma/g" Makefile
	sed -i -e "s/install zip/install camlzip/g" Makefile #WTF not the same name as on linux ?
	make all allopt install-findlib
end_go_in
}

ocamlgraph(){
go_in http://ocamlgraph.lri.fr/download/ ocamlgraph-1.8.2
	ocamlfind remove ocamlgraph
	ocamlfind remove graph
	./configure
	make byte opt install-findlib
end_go_in
}

ulex(){
go_in http://www.cduce.org/download/ ulex-1.1
	make all all.opt install
end_go_in
}

nodejs(){
wget http://nodejs.org/dist/v0.8.14/node-v0.8.14-x86.msi
msiexec /i node-v0.8.14-x86.msi
}

# uncomment all package you want to install
packages="nodejs cryptokit ocamlssl camlzip ocamlgraph ulex all"

#todo help and command line args
#nodejs
#cryptokit
#ocamlssl
#camlzip
#ocamlgraph
#ulex

echo 'open a root command line'
echo 'assoc .pl=PerlScript'
echo 'ftype PerlScript=perl.exe %1 %*'