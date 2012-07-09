(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(** A list of MIME types can be found here: http://www.iana.org/assignments/media-types/
    and here: http://servers.digitaldaze.com/extensions/mimetypes/

    Don't know where the current list come from... *)

(* FIXME: Maybe we should allow the developer to provide his own mime.types file,
   just like most web server does (Apache, Nginx ... *)

let mimetypes = "\
application/activemessage\r\
application/andrew-inset			ez\r\
application/applefile\r\
application/atomicmail\r\
application/batch-SMTP\r\
application/beep+xml\r\
application/cals-1840\r\
application/commonground\r\
application/cu-seeme				cu\r\
application/cybercash\r\
application/dca-rft\r\
application/dec-dx\r\
application/docbook+xml\r\
application/dsptype				tsp\r\
application/dvcs\r\
application/edi-consent\r\
application/edi-x12\r\
application/edifact\r\
application/eshop\r\
application/font-tdpfr\r\
application/futuresplash			spl\r\
application/ghostview\r\
application/hta					hta\r\
application/http\r\
application/hyperstudio\r\
application/iges\r\
application/index\r\
application/index.cmd\r\
application/index.obj\r\
application/index.response\r\
application/index.vnd\r\
application/iotp\r\
application/ipp\r\
application/isup\r\
application/java-archive			jar\r\
application/java-serialized-object		ser\r\
application/java-vm				class\r\
application/mac-binhex40			hqx\r\
application/mac-compactpro			cpt\r\
application/macwriteii\r\
application/marc\r\
application/mathematica				nb\r\
application/mathematica-old\r\
application/msaccess				mdb\r\
application/msword				doc dot\r\
application/news-message-id\r\
application/news-transmission\r\
application/ocsp-request\r\
application/ocsp-response\r\
application/octet-stream			bin\r\
application/oda					oda\r\
application/ogg					ogg\r\
application/parityfec\r\
application/pdf					pdf\r\
application/pgp-encrypted\r\
application/pgp-keys				key\r\
application/pgp-signature			pgp\r\
application/pics-rules				prf\r\
application/pkcs10\r\
application/pkcs7-mime\r\
application/pkcs7-signature\r\
application/pkix-cert\r\
application/pkix-crl\r\
application/pkixcmp\r\
application/postscript				ps ai eps\r\
application/prs.alvestrand.titrax-sheet\r\
application/prs.cww\r\
application/prs.nprend\r\
application/qsig\r\
application/rar					rar\r\
application/rdf+xml				rdf\r\
application/remote-printing\r\
application/riscos\r\
application/rss+xml				rss\r\
application/rtf\r\
application/sdp\r\
application/set-payment\r\
application/set-payment-initiation\r\
application/set-registration\r\
application/set-registration-initiation\r\
application/sgml\r\
application/sgml-open-catalog\r\
application/sieve\r\
application/slate\r\
application/smil				smi smil\r\
application/timestamp-query\r\
application/timestamp-reply\r\
application/vemmi\r\
application/whoispp-query\r\
application/whoispp-response\r\
application/wita\r\
application/wordperfect				wpd\r\
application/wordperfect5.1			wp5\r\
application/x400-bp\r\
application/xhtml+xml				xhtml xht\r\
application/xml					xml xsl\r\
application/xml-dtd\r\
application/xml-external-parsed-entity\r\
application/zip					zip\r\
application/vnd.3M.Post-it-Notes\r\
application/vnd.accpac.simply.aso\r\
application/vnd.accpac.simply.imp\r\
application/vnd.acucobol\r\
application/vnd.aether.imp\r\
application/vnd.anser-web-certificate-issue-initiation\r\
application/vnd.anser-web-funds-transfer-initiation\r\
application/vnd.audiograph\r\
application/vnd.bmi\r\
application/vnd.businessobjects\r\
application/vnd.canon-cpdl\r\
application/vnd.canon-lips\r\
application/vnd.cinderella			cdy\r\
application/vnd.claymore\r\
application/vnd.commerce-battelle\r\
application/vnd.commonspace\r\
application/vnd.comsocaller\r\
application/vnd.contact.cmsg\r\
application/vnd.cosmocaller\r\
application/vnd.ctc-posml\r\
application/vnd.cups-postscript\r\
application/vnd.cups-raster\r\
application/vnd.cups-raw\r\
application/vnd.cybank\r\
application/vnd.dna\r\
application/vnd.dpgraph\r\
application/vnd.dxr\r\
application/vnd.ecdis-update\r\
application/vnd.ecowin.chart\r\
application/vnd.ecowin.filerequest\r\
application/vnd.ecowin.fileupdate\r\
application/vnd.ecowin.series\r\
application/vnd.ecowin.seriesrequest\r\
application/vnd.ecowin.seriesupdate\r\
application/vnd.enliven\r\
application/vnd.epson.esf\r\
application/vnd.epson.msf\r\
application/vnd.epson.quickanime\r\
application/vnd.epson.salt\r\
application/vnd.epson.ssf\r\
application/vnd.ericsson.quickcall\r\
application/vnd.eudora.data\r\
application/vnd.fdf\r\
application/vnd.ffsns\r\
application/vnd.flographit\r\
application/vnd.framemaker\r\
application/vnd.fsc.weblaunch\r\
application/vnd.fujitsu.oasys\r\
application/vnd.fujitsu.oasys2\r\
application/vnd.fujitsu.oasys3\r\
application/vnd.fujitsu.oasysgp\r\
application/vnd.fujitsu.oasysprs\r\
application/vnd.fujixerox.ddd\r\
application/vnd.fujixerox.docuworks\r\
application/vnd.fujixerox.docuworks.binder\r\
application/vnd.fut-misnet\r\
application/vnd.grafeq\r\
application/vnd.groove-account\r\
application/vnd.groove-identity-message\r\
application/vnd.groove-injector\r\
application/vnd.groove-tool-message\r\
application/vnd.groove-tool-template\r\
application/vnd.groove-vcard\r\
application/vnd.hhe.lesson-player\r\
application/vnd.hp-HPGL\r\
application/vnd.hp-PCL\r\
application/vnd.hp-PCLXL\r\
application/vnd.hp-hpid\r\
application/vnd.hp-hps\r\
application/vnd.httphone\r\
application/vnd.hzn-3d-crossword\r\
application/vnd.ibm.MiniPay\r\
application/vnd.ibm.afplinedata\r\
application/vnd.ibm.modcap\r\
application/vnd.informix-visionary\r\
application/vnd.intercon.formnet\r\
application/vnd.intertrust.digibox\r\
application/vnd.intertrust.nncp\r\
application/vnd.intu.qbo\r\
application/vnd.intu.qfx\r\
application/vnd.irepository.package+xml\r\
application/vnd.is-xpr\r\
application/vnd.japannet-directory-service\r\
application/vnd.japannet-jpnstore-wakeup\r\
application/vnd.japannet-payment-wakeup\r\
application/vnd.japannet-registration\r\
application/vnd.japannet-registration-wakeup\r\
application/vnd.japannet-setstore-wakeup\r\
application/vnd.japannet-verification\r\
application/vnd.japannet-verification-wakeup\r\
application/vnd.koan\r\
application/vnd.lotus-1-2-3\r\
application/vnd.lotus-approach\r\
application/vnd.lotus-freelance\r\
application/vnd.lotus-notes\r\
application/vnd.lotus-organizer\r\
application/vnd.lotus-screencam\r\
application/vnd.lotus-wordpro\r\
application/vnd.mcd\r\
application/vnd.mediastation.cdkey\r\
application/vnd.meridian-slingshot\r\
application/vnd.mif\r\
application/vnd.minisoft-hp3000-save\r\
application/vnd.mitsubishi.misty-guard.trustweb\r\
application/vnd.mobius.daf\r\
application/vnd.mobius.dis\r\
application/vnd.mobius.msl\r\
application/vnd.mobius.plc\r\
application/vnd.mobius.txf\r\
application/vnd.motorola.flexsuite\r\
application/vnd.motorola.flexsuite.adsi\r\
application/vnd.motorola.flexsuite.fis\r\
application/vnd.motorola.flexsuite.gotap\r\
application/vnd.motorola.flexsuite.kmr\r\
application/vnd.motorola.flexsuite.ttc\r\
application/vnd.motorola.flexsuite.wem\r\
application/vnd.mozilla.xul+xml			xul\r\
application/vnd.ms-artgalry\r\
application/vnd.ms-asf\r\
application/vnd.ms-excel			xls xlb xlt\r\
application/vnd.ms-lrm\r\
application/vnd.ms-pki.seccat			cat\r\
application/vnd.ms-pki.stl			stl\r\
application/vnd.ms-powerpoint			ppt pps\r\
application/vnd.ms-project\r\
application/vnd.ms-tnef\r\
application/vnd.ms-works\r\
application/vnd.mseq\r\
application/vnd.msign\r\
application/vnd.music-niff\r\
application/vnd.musician\r\
application/vnd.netfpx\r\
application/vnd.noblenet-directory\r\
application/vnd.noblenet-sealer\r\
application/vnd.noblenet-web\r\
application/vnd.novadigm.EDM\r\
application/vnd.novadigm.EDX\r\
application/vnd.novadigm.EXT\r\
application/vnd.oasis.opendocument.chart	odc\r\
application/vnd.oasis.opendocument.database	odb\r\
application/vnd.oasis.opendocument.formula	odf\r\
application/vnd.oasis.opendocument.graphics	odg\r\
application/vnd.oasis.opendocument.graphics-template otg\r\
application/vnd.oasis.opendocument.image	odi\r\
application/vnd.oasis.opendocument.presentation	odp\r\
application/vnd.oasis.opendocument.presentation-template otp\r\
application/vnd.oasis.opendocument.spreadsheet	ods\r\
application/vnd.oasis.opendocument.spreadsheet-template ots\r\
application/vnd.oasis.opendocument.text		odt\r\
application/vnd.oasis.opendocument.text-master	odm\r\
application/vnd.oasis.opendocument.text-template ott\r\
application/vnd.oasis.opendocument.text-web	oth\r\
application/vnd.osa.netdeploy\r\
application/vnd.palm\r\
application/vnd.pg.format\r\
application/vnd.pg.osasli\r\
application/vnd.powerbuilder6\r\
application/vnd.powerbuilder6-s\r\
application/vnd.powerbuilder7\r\
application/vnd.powerbuilder7-s\r\
application/vnd.powerbuilder75\r\
application/vnd.powerbuilder75-s\r\
application/vnd.previewsystems.box\r\
application/vnd.publishare-delta-tree\r\
application/vnd.pvi.ptid1\r\
application/vnd.pwg-xhtml-print+xml\r\
application/vnd.rapid\r\
application/vnd.rim.cod				cod\r\
application/vnd.s3sms\r\
application/vnd.seemail\r\
application/vnd.shana.informed.formdata\r\
application/vnd.shana.informed.formtemplate\r\
application/vnd.shana.informed.interchange\r\
application/vnd.shana.informed.package\r\
application/vnd.smaf				mmf\r\
application/vnd.sss-cod\r\
application/vnd.sss-dtf\r\
application/vnd.sss-ntf\r\
application/vnd.stardivision.calc		sdc\r\
application/vnd.stardivision.draw		sda\r\
application/vnd.stardivision.impress		sdd sdp\r\
application/vnd.stardivision.math		smf\r\
application/vnd.stardivision.writer		sdw vor\r\
application/vnd.stardivision.writer-global	sgl\r\
application/vnd.street-stream\r\
application/vnd.sun.xml.calc			sxc\r\
application/vnd.sun.xml.calc.template		stc\r\
application/vnd.sun.xml.draw			sxd\r\
application/vnd.sun.xml.draw.template		std\r\
application/vnd.sun.xml.impress			sxi\r\
application/vnd.sun.xml.impress.template	sti\r\
application/vnd.sun.xml.math			sxm\r\
application/vnd.sun.xml.writer			sxw\r\
application/vnd.sun.xml.writer.global		sxg\r\
application/vnd.sun.xml.writer.template		stw\r\
application/vnd.svd\r\
application/vnd.swiftview-ics\r\
application/vnd.symbian.install			sis\r\
application/vnd.triscape.mxs\r\
application/vnd.trueapp\r\
application/vnd.truedoc\r\
application/vnd.tve-trigger\r\
application/vnd.ufdl\r\
application/vnd.uplanet.alert\r\
application/vnd.uplanet.alert-wbxml\r\
application/vnd.uplanet.bearer-choice\r\
application/vnd.uplanet.bearer-choice-wbxml\r\
application/vnd.uplanet.cacheop\r\
application/vnd.uplanet.cacheop-wbxml\r\
application/vnd.uplanet.channel\r\
application/vnd.uplanet.channel-wbxml\r\
application/vnd.uplanet.list\r\
application/vnd.uplanet.list-wbxml\r\
application/vnd.uplanet.listcmd\r\
application/vnd.uplanet.listcmd-wbxml\r\
application/vnd.uplanet.signal\r\
application/vnd.vcx\r\
application/vnd.vectorworks\r\
application/vnd.vidsoft.vidconference\r\
application/vnd.visio				vsd\r\
application/vnd.vividence.scriptfile\r\
application/vnd.wap.sic\r\
application/vnd.wap.slc\r\
application/vnd.wap.wbxml			wbxml\r\
application/vnd.wap.wmlc			wmlc\r\
application/vnd.wap.wmlscriptc			wmlsc\r\
application/vnd.webturbo\r\
application/vnd.wrq-hp3000-labelled\r\
application/vnd.wt.stf\r\
application/vnd.xara\r\
application/vnd.xfdl\r\
application/vnd.yellowriver-custom-menu\r\
application/x-123				wk\r\
application/x-abiword				abw\r\
application/x-apple-diskimage			dmg\r\
application/x-bcpio				bcpio\r\
application/x-bittorrent			torrent\r\
application/x-cdf				cdf\r\
application/x-cdlink				vcd\r\
application/x-chess-pgn				pgn\r\
application/x-core\r\
application/x-cpio				cpio\r\
application/x-csh				csh\r\
application/x-debian-package			deb udeb\r\
application/x-director				dcr dir dxr\r\
application/x-dms				dms\r\
application/x-doom				wad\r\
application/x-dvi				dvi\r\
application/x-executable\r\
application/x-flac				flac\r\
application/x-font				pfa pfb gsf pcf pcf.Z\r\
application/x-freemind				mm\r\
application/x-futuresplash			spl\r\
application/x-gnumeric				gnumeric\r\
application/x-go-sgf				sgf\r\
application/x-graphing-calculator		gcf\r\
application/x-gtar				gtar tgz taz\r\
application/x-hdf				hdf\r\
application/x-httpd-php				phtml pht php\r\
application/x-httpd-php-source			phps\r\
application/x-httpd-php3			php3\r\
application/x-httpd-php3-preprocessed		php3p\r\
application/x-httpd-php4			php4\r\
application/x-httpd-eruby			rhtml\r\
application/x-ica				ica\r\
application/x-internet-signup			ins isp\r\
application/x-iphone				iii\r\
application/x-iso9660-image			iso\r\
application/x-java-applet\r\
application/x-java-bean\r\
application/x-java-jnlp-file			jnlp\r\
application/x-javascript			js\r\
application/x-jmol				jmz\r\
application/x-kchart				chrt\r\
application/x-kdelnk\r\
application/x-killustrator			kil\r\
application/x-koan				skp skd skt skm\r\
application/x-kpresenter			kpr kpt\r\
application/x-kspread				ksp\r\
application/x-kword				kwd kwt\r\
application/x-latex				latex\r\
application/x-lha				lha\r\
application/x-lzh				lzh\r\
application/x-lzx				lzx\r\
application/x-maker				frm maker frame fm fb book fbdoc\r\
application/x-mif				mif\r\
application/x-ms-wmd				wmd\r\
application/x-ms-wmz				wmz\r\
application/x-msdos-program			com exe bat dll\r\
application/x-msi				msi\r\
application/x-netcdf				nc\r\
application/x-ns-proxy-autoconfig		pac\r\
application/x-nwc				nwc\r\
application/x-object				o\r\
application/x-oz-application			oza\r\
application/x-pkcs7-certreqresp			p7r\r\
application/x-pkcs7-crl				crl\r\
application/x-python-code			pyc pyo\r\
application/x-quicktimeplayer			qtl\r\
application/x-redhat-package-manager		rpm\r\
application/x-rx\r\
application/x-sh				sh\r\
application/x-shar				shar\r\
application/x-shellscript\r\
application/x-shockwave-flash			swf swfl\r\
application/x-stuffit				sit\r\
application/x-sv4cpio				sv4cpio\r\
application/x-sv4crc				sv4crc\r\
application/x-tar				tar\r\
application/x-tcl				tcl\r\
application/x-tex-gf				gf\r\
application/x-tex-pk				pk\r\
application/x-texinfo				texinfo texi\r\
application/x-trash				~ % bak old sik\r\
application/x-troff				t tr roff\r\
application/x-troff-man				man\r\
application/x-troff-me				me\r\
application/x-troff-ms				ms\r\
application/x-ustar				ustar\r\
application/x-videolan\r\
application/x-wais-source			src\r\
application/x-wingz				wz\r\
application/x-x509-ca-cert			crt\r\
application/x-xcf				xcf\r\
application/x-xfig				fig\r\
application/x-xpinstall				xpi\r\
\r\
audio/32kadpcm\r\
audio/basic					au snd\r\
audio/g.722.1\r\
audio/l16\r\
audio/midi					mid midi kar\r\
audio/mp4a-latm\r\
audio/mpa-robust\r\
audio/mpeg					mpga mpega mp2 mp3 m4a\r\
audio/mpegurl					m3u\r\
audio/parityfec\r\
audio/prs.sid					sid\r\
audio/telephone-event\r\
audio/tone\r\
audio/vnd.cisco.nse\r\
audio/vnd.cns.anp1\r\
audio/vnd.cns.inf1\r\
audio/vnd.digital-winds\r\
audio/vnd.everad.plj\r\
audio/vnd.lucent.voice\r\
audio/vnd.nortel.vbk\r\
audio/vnd.nuera.ecelp4800\r\
audio/vnd.nuera.ecelp7470\r\
audio/vnd.nuera.ecelp9600\r\
audio/vnd.octel.sbc\r\
audio/vnd.qcelp\r\
audio/vnd.rhetorex.32kadpcm\r\
audio/vnd.vmx.cvsd\r\
audio/x-aiff					aif aiff aifc\r\
audio/x-gsm					gsm\r\
audio/x-mpegurl					m3u\r\
audio/x-ms-wma					wma\r\
audio/x-ms-wax					wax\r\
audio/x-pn-realaudio-plugin\r\
audio/x-pn-realaudio				ra rm ram\r\
audio/x-realaudio				ra\r\
audio/x-scpls					pls\r\
audio/x-sd2					sd2\r\
audio/x-wav					wav\r\
\r\
chemical/x-alchemy				alc\r\
chemical/x-cache				cac cache\r\
chemical/x-cache-csf				csf\r\
chemical/x-cactvs-binary			cbin cascii ctab\r\
chemical/x-cdx					cdx\r\
chemical/x-cerius				cer\r\
chemical/x-chem3d				c3d\r\
chemical/x-chemdraw				chm\r\
chemical/x-cif					cif\r\
chemical/x-cmdf					cmdf\r\
chemical/x-cml					cml\r\
chemical/x-compass				cpa\r\
chemical/x-crossfire				bsd\r\
chemical/x-csml					csml csm\r\
chemical/x-ctx					ctx\r\
chemical/x-cxf					cxf cef\r\
#chemical/x-daylight-smiles			smi\r\
chemical/x-embl-dl-nucleotide			emb embl\r\
chemical/x-galactic-spc				spc\r\
chemical/x-gamess-input				inp gam gamin\r\
chemical/x-gaussian-checkpoint			fch fchk\r\
chemical/x-gaussian-cube			cub\r\
chemical/x-gaussian-input			gau gjc gjf\r\
chemical/x-gaussian-log				gal\r\
chemical/x-gcg8-sequence			gcg\r\
chemical/x-genbank				gen\r\
chemical/x-hin					hin\r\
chemical/x-isostar				istr ist\r\
chemical/x-jcamp-dx				jdx dx\r\
chemical/x-kinemage				kin\r\
chemical/x-macmolecule				mcm\r\
chemical/x-macromodel-input			mmd mmod\r\
chemical/x-mdl-molfile				mol\r\
chemical/x-mdl-rdfile				rd\r\
chemical/x-mdl-rxnfile				rxn\r\
chemical/x-mdl-sdfile				sd sdf\r\
chemical/x-mdl-tgf				tgf\r\
#chemical/x-mif					mif\r\
chemical/x-mmcif				mcif\r\
chemical/x-mol2					mol2\r\
chemical/x-molconn-Z				b\r\
chemical/x-mopac-graph				gpt\r\
chemical/x-mopac-input				mop mopcrt mpc dat zmt\r\
chemical/x-mopac-out				moo\r\
chemical/x-mopac-vib				mvb\r\
chemical/x-ncbi-asn1				asn\r\
chemical/x-ncbi-asn1-ascii			prt ent\r\
chemical/x-ncbi-asn1-binary			val aso\r\
chemical/x-ncbi-asn1-spec			asn\r\
chemical/x-pdb					pdb ent\r\
chemical/x-rosdal				ros\r\
chemical/x-swissprot				sw\r\
chemical/x-vamas-iso14976			vms\r\
chemical/x-vmd					vmd\r\
chemical/x-xtel					xtel\r\
chemical/x-xyz					xyz\r\
\r\
image/cgm\r\
image/g3fax\r\
image/gif					gif\r\
image/ief					ief\r\
image/jpeg					jpeg jpg jpe\r\
image/naplps\r\
image/pcx					pcx\r\
image/png					png\r\
image/prs.btif\r\
image/prs.pti\r\
image/svg+xml					svg svgz\r\
image/tiff					tiff tif\r\
image/vnd.cns.inf2\r\
image/vnd.djvu					djvu djv\r\
image/vnd.dwg\r\
image/vnd.dxf\r\
image/vnd.fastbidsheet\r\
image/vnd.fpx\r\
image/vnd.fst\r\
image/vnd.fujixerox.edmics-mmr\r\
image/vnd.fujixerox.edmics-rlc\r\
image/vnd.mix\r\
image/vnd.net-fpx\r\
image/vnd.svf\r\
image/vnd.wap.wbmp				wbmp\r\
image/vnd.xiff\r\
image/x-cmu-raster				ras\r\
image/x-coreldraw				cdr\r\
image/x-coreldrawpattern			pat\r\
image/x-coreldrawtemplate			cdt\r\
image/x-corelphotopaint				cpt\r\
image/x-icon					ico\r\
image/x-jg					art\r\
image/x-jng					jng\r\
image/x-ms-bmp					bmp\r\
image/x-photoshop				psd\r\
image/x-portable-anymap				pnm\r\
image/x-portable-bitmap				pbm\r\
image/x-portable-graymap			pgm\r\
image/x-portable-pixmap				ppm\r\
image/x-rgb					rgb\r\
image/x-xbitmap					xbm\r\
image/x-xpixmap					xpm\r\
image/x-xwindowdump				xwd\r\
\r\
inode/chardevice\r\
inode/blockdevice\r\
inode/directory-locked\r\
inode/directory\r\
inode/fifo\r\
inode/socket\r\
\r\
message/delivery-status\r\
message/disposition-notification\r\
message/external-body\r\
message/http\r\
message/s-http\r\
message/news\r\
message/partial\r\
message/rfc822\r\
\r\
model/iges					igs iges\r\
model/mesh					msh mesh silo\r\
model/vnd.dwf\r\
model/vnd.flatland.3dml\r\
model/vnd.gdl\r\
model/vnd.gs-gdl\r\
model/vnd.gtw\r\
model/vnd.mts\r\
model/vnd.vtu\r\
model/vrml					wrl vrml\r\
\r\
multipart/alternative\r\
multipart/appledouble\r\
multipart/byteranges\r\
multipart/digest\r\
multipart/encrypted\r\
multipart/form-data\r\
multipart/header-set\r\
multipart/mixed\r\
multipart/parallel\r\
multipart/related\r\
multipart/report\r\
multipart/signed\r\
multipart/voice-message\r\
\r\
text/calendar					ics icz\r\
text/comma-separated-values			csv\r\
text/css					css\r\
text/directory\r\
text/english\r\
text/enriched\r\
text/h323					323\r\
text/html					html htm shtml\r\
text/iuls					uls\r\
text/mathml					mml\r\
text/parityfec\r\
text/plain					asc txt text diff pot\r\
text/prs.lines.tag\r\
text/rfc822-headers\r\
text/richtext					rtx\r\
text/rtf					rtf\r\
text/scriptlet					sct wsc\r\
text/t140\r\
text/texmacs					tm ts\r\
text/tab-separated-values			tsv\r\
text/uri-list\r\
text/vnd.abc\r\
text/vnd.curl\r\
text/vnd.DMClientScript\r\
text/vnd.flatland.3dml\r\
text/vnd.fly\r\
text/vnd.fmi.flexstor\r\
text/vnd.in3d.3dml\r\
text/vnd.in3d.spot\r\
text/vnd.IPTC.NewsML\r\
text/vnd.IPTC.NITF\r\
text/vnd.latex-z\r\
text/vnd.motorola.reflex\r\
text/vnd.ms-mediapackage\r\
text/vnd.sun.j2me.app-descriptor		jad\r\
text/vnd.wap.si\r\
text/vnd.wap.sl\r\
text/vnd.wap.wml				wml\r\
text/vnd.wap.wmlscript				wmls\r\
text/x-bibtex					bib\r\
text/x-boo					boo\r\
text/x-c++hdr					h++ hpp hxx hh\r\
text/x-c++src					c++ cpp cxx cc\r\
text/x-chdr					h\r\
text/x-component				htc\r\
text/x-crontab\r\
text/x-csh					csh\r\
text/x-csrc					c\r\
text/x-dsrc					d\r\
text/x-haskell					hs\r\
text/x-java					java\r\
text/x-literate-haskell				lhs\r\
text/x-makefile\r\
text/x-moc					moc\r\
text/x-pascal					p pas\r\
text/x-pcs-gcd					gcd\r\
text/x-perl					pl pm\r\
text/x-python					py\r\
text/x-server-parsed-html\r\
text/x-setext					etx\r\
text/x-sh					sh\r\
text/x-tcl					tcl tk\r\
text/x-tex					tex ltx sty cls\r\
text/x-vcalendar				vcs\r\
text/x-vcard					vcf\r\
\r\
video/dl					dl\r\
video/dv					dif dv\r\
video/fli					fli\r\
video/gl					gl\r\
video/mpeg					mpeg mpg mpe\r\
video/mp4					mp4\r\
video/quicktime					qt mov\r\
video/mp4v-es\r\
video/parityfec\r\
video/pointer\r\
video/vnd.fvt\r\
video/vnd.motorola.video\r\
video/vnd.motorola.videop\r\
video/vnd.mpegurl				mxu\r\
video/vnd.mts\r\
video/vnd.nokia.interleaved-multimedia\r\
video/vnd.vivo\r\
video/x-la-asf					lsf lsx\r\
video/x-mng					mng\r\
video/x-ms-asf					asf asx\r\
video/x-ms-wm					wm\r\
video/x-ms-wmv					wmv\r\
video/x-ms-wmx					wmx\r\
video/x-ms-wvx					wvx\r\
video/x-msvideo					avi\r\
video/x-sgi-movie				movie\r\
\r\
x-conference/x-cooltalk				ice\r\
\r\
x-world/x-vrml					vrm vrml wrl\r\
"
