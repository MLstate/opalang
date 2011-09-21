package hello_i18n.translation
import stdlib.core.i18n

// Template for hello_i18n.opa
// "server"
// string, 6
__i18n_cf1e8c14e54505f60aa10ceb8d5d8ab3()= match I18n.lang()
"fr"  -> "serveur"
"en"  -> "server"
  _   -> "server"


// Template for hello_i18n.opa
// "{p1} say hello"
// string, 6
__i18n_51ceade11ee0ae73e2b41ed0feeafd1a(p1:string)= match I18n.lang()
"fr"  -> "{p1} dit bonjour"
"en"  -> "Hello from {p1}"
  _   -> "{p1} say hello"


// Template for hello_i18n.opa
// "client say _hello"
// string, 9
__i18n_7e3b0dc086f943b0c49c7a69f66f1cc1()= match I18n.lang()
"fr"  -> "bonjour (côté client)"
"en"  -> "hello (client side)"
  _   -> "client say hello"


// Template for hello_i18n.opa
// "Hello World"
// string, 16
__i18n_b10a8db164e0754105b7a99be72e3fe5()= match I18n.lang()
"fr"  -> "Bonjour Monde"
"en"  -> "Good morning World"
  _   -> "Hello World"


// Template for hello_i18n.opa
// "Hello"
// string, 19
__i18n_8b1a9953c4611296a827abf8c47804d7()= match I18n.lang()
"fr"  -> "Bonjour"
"en"  -> "Good morning"
  _   -> "Hello"

