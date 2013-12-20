/*
    Copyright © 2011, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package stdlib.apis.ldap

/**
 * Types and functions to handle the inetOrgPerson schema data.
 *
 * @destination public
 * @stabilization work in progress
 **/

type Ldap.inetOrgPerson = {

  // top
  option(string) dn, // always in search replies

  // person
  list(string) sn, // MUST
  list(string) cn, // MUST
  option(string) userPassword,
  option(string) seeAlso,
  option(string) description,

  //organizationalPerson
  list(string) title,
  list(string) x121Address,
  list(string) registeredAddress,
  list(string) destinationIndicator,
  option(string) preferredDeliveryMethod,
  list(string) telexNumber,
  list(string) teletexTerminalIdentifier,
  list(string) telephoneNumber,
  list(string) internationaliSDNNumber,
  list(string) facsimileTelephoneNumber,
  list(string) street,
  list(string) postOfficeBox,
  list(string) postalCode,
  list(string) postalAddress,
  list(string) physicalDeliveryOfficeName,
  list(string) ou,
  list(string) st,
  list(string) l,

  // inetOrgPerson
  list(string) audio,
  list(string) businessCategory,
  list(string) carLicense,
  list(string) departmentNumber,
  option(string) displayName,
  option(string) employeeNumber,
  list(string) employeeType,
  list(string) givenName,
  list(string) homePhone,
  list(string) homePostalAddress,
  list(string) initials,
  list(string) jpegPhoto,
  list(string) labeledURI,
  list(string) mail,
  list(string) manager,
  list(string) mobile,
  list(string) o,
  list(string) pager,
  list(string) photo,
  list(string) roomNumber,
  list(string) secretary,
  list(string) uid,
  list(string) userCertificate,
  list(string) x500uniqueIdentifier,
  option(string) preferredLanguage,
  list(string) userSMIMECertificate,
  list(string) userPKCS12

}

module InetOrgPerson {

  Ldap.inetOrgPerson default_inetOrgPerson = {

    // top
    dn:{none},

    // person
    sn:[],
    cn:[],
    userPassword:{none},
    seeAlso:{none},
    description:{none},

    //organizationalPerson
    title:[],
    x121Address:[],
    registeredAddress:[],
    destinationIndicator:[],
    preferredDeliveryMethod:{none},
    telexNumber:[],
    teletexTerminalIdentifier:[],
    telephoneNumber:[],
    internationaliSDNNumber:[],
    facsimileTelephoneNumber:[],
    street:[],
    postOfficeBox:[],
    postalCode:[],
    postalAddress:[],
    physicalDeliveryOfficeName:[],
    ou:[],
    st:[],
    l:[],

    // inetOrgPerson
    audio:[],
    businessCategory:[],
    carLicense:[],
    departmentNumber:[],
    displayName:{none},
    employeeNumber:{none},
    employeeType:[],
    givenName:[],
    homePhone:[],
    homePostalAddress:[],
    initials:[],
    jpegPhoto:[],
    labeledURI:[],
    mail:[],
    manager:[],
    mobile:[],
    o:[],
    pager:[],
    photo:[],
    roomNumber:[],
    secretary:[],
    uid:[],
    userCertificate:[],
    x500uniqueIdentifier:[],
    preferredLanguage:{none},
    userSMIMECertificate:[],
    userPKCS12:[]

  }

  private function lst(_name, json, rcrd, cur, set) {
    //Ansi.jlog("lst: name=%y{name}%d cur=%m{cur}%d json=%g{json}%d");
    match (json) {
    case {~String}:
      //Ansi.jlog("{name}: %m{String}%d")
      set(rcrd, [String|cur]);
    case {List:l}:
      new =
        List.fold(function (el, new) {
                    match (el) {
                    case {~String}:
                      //Ansi.jlog("{name}: %m{String}%d")
                      [String|new];
                    default: new;
                    }
                  },l,cur)
      set(rcrd,new);
    default:
      //Ansi.jlog("{name}: %rnot string or list%d")
      rcrd;
    }
  }

  private function opt(_name, json, rcrd, cur, set) { // ie. SINGLE-VALUE
    //Ansi.jlog("opt: name=%y{name}%d cur=%m{cur}%d json=%g{json}%d");
    function chk(String) {
      match (cur) {
      case {none}:
        //Ansi.jlog("{name}: %m{String}%d")
        {some:set(rcrd, {some:String})};
      case {some:_}:
        {none};
      }
    }
    match (json) {
    case {~String}: chk(String);
    case {List:[]}: {some:rcrd};
    case {List:[{~String}]}: chk(String);
    default: {none};
    }
  }

  private function chkMultiple(aux, flds, res) {
    match (res) {
    case {none}: {failure:"Multiple SINGLE-VALUE entries"};
    case {some:rcrd}: aux(flds, rcrd);
    }
  }

  function parseJson(RPC.Json.json json) {
    match (json) {
    case {~Record}:
      recursive function aux(flds, iop) {
        match (flds) {
        case []: {success:iop};
        case [fld|flds]:
          //Ansi.jlog("fld: %b{fld}%d")
          match (String.lowercase(fld.f1)) {

          // top
          case "dn":
            chkMultiple(aux, flds, opt("dn", fld.f2, iop, iop.dn,
                        function (iop, dn) { {iop with ~dn} }));
          case "controls": aux(flds, iop);  // ignore this, it's rarely used

          // person
          case "sn": aux(flds,lst("sn", fld.f2, iop, iop.sn, function (iop, sn) { {iop with ~sn} }));
          case "surname": aux(flds,lst("surname", fld.f2, iop, iop.sn, function (iop, sn) { {iop with ~sn} }));
          case "cn": aux(flds,lst("cn", fld.f2, iop, iop.cn, function (iop, cn) { {iop with ~cn} }));
          case "commonname": aux(flds,lst("commonName", fld.f2, iop, iop.cn, function (iop, cn) { {iop with ~cn} }));
          case "userpassword":
            chkMultiple(aux, flds, opt("userPassword", fld.f2, iop, iop.userPassword,
                        function (iop, userPassword) { {iop with ~userPassword} }));
          case "seealso":
            chkMultiple(aux, flds, opt("seeAlso", fld.f2, iop, iop.seeAlso,
                        function (iop, seeAlso) { {iop with ~seeAlso} }));
          case "description":
            chkMultiple(aux, flds, opt("description", fld.f2, iop, iop.description,
                        function (iop, description) { {iop with ~description} }));

          //organizationalPerson
          case "title":
            aux(flds,lst("title", fld.f2, iop, iop.title,
                function (iop, title) { {iop with ~title} }));
          case "x121address":
            aux(flds,lst("x121Address", fld.f2, iop, iop.x121Address,
                function (iop, x121Address) { {iop with ~x121Address} }));
          case "registeredaddress":
            aux(flds,lst("registeredAddress", fld.f2, iop, iop.registeredAddress,
                function (iop, registeredAddress) { {iop with ~registeredAddress} }));
          case "destinationindicator":
            aux(flds,lst("destinationIndicator", fld.f2, iop, iop.destinationIndicator,
                function (iop, destinationIndicator) { {iop with ~destinationIndicator} }));
          case "preferreddeliverymethod":
            chkMultiple(aux, flds, opt("description", fld.f2, iop, iop.description,
              function (iop, description) { {iop with ~description} }));
          case "telexnumber":
            aux(flds,lst("telexNumber", fld.f2, iop, iop.telexNumber,
                function (iop, telexNumber) { {iop with ~telexNumber} }));
          case "teletexterminalidentifier":
            aux(flds,lst("teletexTerminalIdentifier", fld.f2, iop, iop.teletexTerminalIdentifier,
                function (iop, teletexTerminalIdentifier) { {iop with ~teletexTerminalIdentifier} }));
          case "telephonenumber":
            aux(flds,lst("telephoneNumber", fld.f2, iop, iop.telephoneNumber,
                function (iop, telephoneNumber) { {iop with ~telephoneNumber} }));
          case "internationalisdnnumber":
            aux(flds,lst("internationaliSDNNumber", fld.f2, iop, iop.internationaliSDNNumber,
                function (iop, internationaliSDNNumber) { {iop with ~internationaliSDNNumber} }));
          case "facsimiletelephonenumber":
            aux(flds,lst("facsimileTelephoneNumber", fld.f2, iop, iop.facsimileTelephoneNumber,
                function (iop, facsimileTelephoneNumber) { {iop with ~facsimileTelephoneNumber} }));
          case "street":
            aux(flds,lst("street", fld.f2, iop, iop.street,
                function (iop, street) { {iop with ~street} }));
          case "postofficebox":
            aux(flds,lst("postOfficeBox", fld.f2, iop, iop.postOfficeBox,
                function (iop, postOfficeBox) { {iop with ~postOfficeBox} }));
          case "postalcode":
            aux(flds,lst("postalCode", fld.f2, iop, iop.postalCode,
                function (iop, postalCode) { {iop with ~postalCode} }));
          case "postaladdress":
            aux(flds,lst("postalAddress", fld.f2, iop, iop.postalAddress,
                function (iop, postalAddress) { {iop with ~postalAddress} }));
          case "physicaldeliveryofficename":
            aux(flds,lst("physicalDeliveryOfficeName", fld.f2, iop, iop.physicalDeliveryOfficeName,
                function (iop, physicalDeliveryOfficeName) { {iop with ~physicalDeliveryOfficeName} }));
          case "ou":
            aux(flds,lst("ou", fld.f2, iop, iop.ou,
                function (iop, ou) { {iop with ~ou} }));
          case "st":
            aux(flds,lst("st", fld.f2, iop, iop.st,
                function (iop, st) { {iop with ~st} }));
          case "l":
            aux(flds,lst("l", fld.f2, iop, iop.l,
                function (iop, l) { {iop with ~l} }));

          // inetOrgPerson
          case "audio":
            aux(flds,lst("audio", fld.f2, iop, iop.audio,
                function (iop, audio) { {iop with ~audio} }));
          case "businesscategory":
            aux(flds,lst("businessCategory", fld.f2, iop, iop.businessCategory,
                function (iop, businessCategory) { {iop with ~businessCategory} }));
          case "carlicense":
            aux(flds,lst("carLicense", fld.f2, iop, iop.carLicense,
                function (iop, carLicense) { {iop with ~carLicense} }));
          case "departmentnumber":
            aux(flds,lst("departmentNumber", fld.f2, iop, iop.departmentNumber,
                function (iop, departmentNumber) { {iop with ~departmentNumber} }));
          case "displayname":
            chkMultiple(aux, flds, opt("description", fld.f2, iop, iop.description,
                        function (iop, description) { {iop with ~description} }));
          case "employeenumber":
            chkMultiple(aux, flds, opt("description", fld.f2, iop, iop.description,
                        function (iop, description) { {iop with ~description} }));
          case "employeetype":
            aux(flds,lst("employeeType", fld.f2, iop, iop.employeeType,
                function (iop, employeeType) { {iop with ~employeeType} }));
          case "givenname":
            aux(flds,lst("givenName", fld.f2, iop, iop.givenName,
                function (iop, givenName) { {iop with ~givenName} }));
          case "homephone":
            aux(flds,lst("homePhone", fld.f2, iop, iop.homePhone,
                function (iop, homePhone) { {iop with ~homePhone} }));
          case "homepostaladdress":
            aux(flds,lst("homePostalAddress", fld.f2, iop, iop.homePostalAddress,
                function (iop, homePostalAddress) { {iop with ~homePostalAddress} }));
          case "initials":
            aux(flds,lst("initials", fld.f2, iop, iop.initials,
                function (iop, initials) { {iop with ~initials} }));
          case "jpegphoto":
            aux(flds,lst("jpegPhoto", fld.f2, iop, iop.jpegPhoto,
                function (iop, jpegPhoto) { {iop with ~jpegPhoto} }));
          case "labeleduri":
            aux(flds,lst("labeledURI", fld.f2, iop, iop.labeledURI,
                function (iop, labeledURI) { {iop with ~labeledURI} }));
          case "mail":
            aux(flds,lst("mail", fld.f2, iop, iop.mail,
                function (iop, mail) { {iop with ~mail} }));
          case "manager":
            aux(flds,lst("manager", fld.f2, iop, iop.manager,
                function (iop, manager) { {iop with ~manager} }));
          case "mobile":
            aux(flds,lst("mobile", fld.f2, iop, iop.mobile,
                function (iop, mobile) { {iop with ~mobile} }));
          case "o":
            aux(flds,lst("o", fld.f2, iop, iop.o,
                function (iop, o) { {iop with ~o} }));
          case "pager":
            aux(flds,lst("pager", fld.f2, iop, iop.pager,
                function (iop, pager) { {iop with ~pager} }));
          case "photo":
            aux(flds,lst("photo", fld.f2, iop, iop.photo,
                function (iop, photo) { {iop with ~photo} }));
          case "roomnumber":
            aux(flds,lst("roomNumber", fld.f2, iop, iop.roomNumber,
                function (iop, roomNumber) { {iop with ~roomNumber} }));
          case "secretary":
            aux(flds,lst("secretary", fld.f2, iop, iop.secretary,
                function (iop, secretary) { {iop with ~secretary} }));
          case "uid":
            aux(flds,lst("uid", fld.f2, iop, iop.uid,
                function (iop, uid) { {iop with ~uid} }));
          case "usercertificate":
            aux(flds,lst("userCertificate", fld.f2, iop, iop.userCertificate,
                function (iop, userCertificate) { {iop with ~userCertificate} }));
          case "x500uniqueidentifier":
            aux(flds,lst("x500uniqueIdentifier", fld.f2, iop, iop.x500uniqueIdentifier,
                function (iop, x500uniqueIdentifier) { {iop with ~x500uniqueIdentifier} }));
          case "preferredlanguage":
            chkMultiple(aux, flds, opt("description", fld.f2, iop, iop.description,
                        function (iop, description) { {iop with ~description} }));
          case "usersmimecertificate":
            aux(flds,lst("userSMIMECertificate", fld.f2, iop, iop.userSMIMECertificate,
                function (iop, userSMIMECertificate) { {iop with ~userSMIMECertificate} }));
          case "userpkcs12":
            aux(flds,lst("userPKCS12", fld.f2, iop, iop.userPKCS12,
                function (iop, userPKCS12) { {iop with ~userPKCS12} }));
          default:
            {failure:"Bad inetOrgPerson field '{fld.f1}'"};
          }
        }
      }
      aux(Record, default_inetOrgPerson);
    default: {failure:"JSON value is not Record"};
    }
  }

  function outcome(string,string) toJsonString(Ldap.inetOrgPerson iop) {
    missing = List.flatten([if (List.is_empty(iop.cn)) ["cn"] else [],
                            if (List.is_empty(iop.sn)) ["sn"] else []])
    if (not(List.is_empty(missing)))
      {failure:"Missing MUST fields {String.concat(",",missing)}"}
    else {
      function mkstr(string String) { {~String} }
      function lst(name, l) {
        match (l) {
        case [String]: [(name,{~String})];
        case [_|_]: [(name,{List:List.map(mkstr,l)})];
        case []: [];
        }
      }
      function opt(name, o) {
        match (o) {
        case {some:String}: [(name,{~String})];
        case {none}: [];
        }
      }
      {success:
       Json.serialize({Record:List.flatten([

        [// top
         ("objectclass",{List:[{String:"inetOrgPerson"}]})],
         opt("dn",iop.dn),

         // person
         lst("sn",iop.sn),
         lst("cn",iop.cn),
         opt("userPassword",iop.userPassword),
         opt("seeAlso",iop.seeAlso),
         opt("description",iop.description),

         //organizationalPerson
         lst("title",iop.title),
         lst("x121Address",iop.x121Address),
         lst("registeredAddress",iop.registeredAddress),
         lst("destinationIndicator",iop.destinationIndicator),
         opt("preferreddeliverymethod",iop.preferredDeliveryMethod),
         lst("telexNumber",iop.telexNumber),
         lst("teletexTerminalIdentifier",iop.teletexTerminalIdentifier),
         lst("telephoneNumber",iop.telephoneNumber),
         lst("internationaliSDNNumber",iop.internationaliSDNNumber),
         lst("facsimileTelephoneNumber",iop.facsimileTelephoneNumber),
         lst("street",iop.street),
         lst("postOfficeBox",iop.postOfficeBox),
         lst("postalCode",iop.postalCode),
         lst("postalAddress",iop.postalAddress),
         lst("physicalDeliveryOfficeName",iop.physicalDeliveryOfficeName),
         lst("ou",iop.ou),
         lst("st",iop.st),
         lst("l",iop.l),

         // inetOrgPerson
         lst("audio",iop.audio),
         lst("businessCategory",iop.businessCategory),
         lst("carLicense",iop.carLicense),
         lst("departmentNumber",iop.departmentNumber),
         opt("displayName",iop.displayName),
         opt("employeeNumber",iop.employeeNumber),
         lst("employeeType",iop.employeeType),
         lst("givenName",iop.givenName),
         lst("homePhone",iop.homePhone),
         lst("homePostalAddress",iop.homePostalAddress),
         lst("initials",iop.initials),
         lst("jpegPhoto",iop.jpegPhoto),
         lst("labeledURI",iop.labeledURI),
         lst("mail",iop.mail),
         lst("manager",iop.manager),
         lst("mobile",iop.mobile),
         lst("o",iop.o),
         lst("pager",iop.pager),
         lst("photo",iop.photo),
         lst("roomNumber",iop.roomNumber),
         lst("secretary",iop.secretary),
         lst("uid",iop.uid),
         lst("userCertificate",iop.userCertificate),
         lst("x500uniqueIdentifier",iop.x500uniqueIdentifier),
         opt("preferredLanguage",iop.preferredLanguage),
         lst("userSMIMECertificate",iop.userSMIMECertificate),
         lst("userPKCS12",iop.userPKCS12)
        ])})}
    }
  }

}

