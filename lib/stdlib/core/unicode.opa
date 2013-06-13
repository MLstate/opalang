/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * type character :
 * Represent a single character.
 * Since we want Unicode encoding, the traditional `char' of one byte will not be enough.
 * This is only an input/output type ! The inner type is UTF-8 (one to four bytes).
 *
 * FIXME: Can it be abstract
**/
@opacapi
type Unicode.character = int

/**
 * Definition of the unicode char primitive
 */
module Unicode{

  /**
   * Convert a character to lowercase
   */
  function lowercase(Unicode.character c){
    if(
      ((c >= 65) && (c <= 91))                                    // US-ASCII
       || ((c >= 192) && (c <= 214))                               // ISO-8859-1 (latin-1)  v
       || ((c >= 216) && (c <= 222))                               // ISO-8859-1 (latin-1)  ^
       || ((c >= 65313) && (c <= 65338))                           // caracteres demi/pleine chasse
       || ((c >= 912) && (c <= 942))                               // grec
        || ((c >= 1040) && (c <= 1071))                            // cyrillique
    ){
      c + 32
    } else if (
      ((c >= 256) && (c <= 319) && ((mod(c, 2)) == 0))         // latin étendu A v
        || ((c >= 313) && (c <= 328) && ((mod(c, 2)) == 1))
        || ((c >= 330) && (c <= 375) && ((mod(c, 2)) == 0))
        || (c == 377) || (c == 379) || (c == 381)                      // latin étendu A ^
        || (c == 388) || (c == 391) || (c == 395) || (c == 401)         // latin étendu B v
        || (c == 408) || (c == 416) || (c == 418) || (c == 420)
        || (c == 423) || (c == 428) || (c == 431) || (c == 433)
        || (c == 435) || (c == 437) || (c == 453) || (c == 456)
        || (c == 459)
        || ((c >= 461) && (c <= 476) && ((mod(c, 2)) == 1))
        || ((c >= 478) && (c <= 495) && ((mod(c, 2)) == 0))
        || (c == 498)
        || ((c >= 500) && (c <= 563) && ((mod(c, 2)) == 0))
        || (c == 571) || (c == 577) || (c == 584) || (c == 586)
        || (c == 588) || (c == 590)                                   // latin étendu B ^
        || ((c >= 7680) && (c <= 7935) && ((mod(c, 2)) == 0))          // latin étendu additionnel
        || (c == 8579)                                               // nombre latin : facteur 10
        || ((c >= 976) && (c <= 1007) && ((mod(c, 2)) == 0))           // grec avec accents
        || ((c >= 1120) && (c <= 1153) && ((mod(c, 2)) == 0))          // cyrillique v
        || ((c >= 1162) && (c <= 1215) && ((mod(c, 2)) == 0))
        || ((c >= 1217) && (c <= 1230) && ((mod(c, 2)) == 1))
        || ((c >= 1232) && (c <= 1279) && ((mod(c, 2)) == 0))          // cyrillique ^
        || ((c >= 1280) && (c <= 1299) && ((mod(c, 2)) == 0))
    ){     // cyrillique additionnel
      c + 1
    } else if ((c == 452) || (c == 455) || (c == 458) || (c == 497)){ // latin étendu B doubles lettres
      c + 2
    } else if (c == 376){                                        // special case : ÿ. Latin 1&A.
      255
    } else if ((c >= 9398) && (c <= 9423)){                     // lettres pastilles
      c + 26
    } else if ((c >= 1024) && (c <= 1039)){                     // cyrillique
      c + 80
    } else if (((c >= 7936) && (c <= 8047) && ((mod(c, 16)) > 7))      // grec polytonique v
               || ((c >= 8064) && (c <= 8111) && ((mod(c, 16)) > 7))){    // grev polytonique ^
        c - 8
    } else if ((c >= 1329) && (c <= 1366)){                      // arménien
      c + 48
    } else if ((c >= 8544) && (c <= 8559)){                      // nombres latins
      c + 16
    } else c
  }

  /**
   * Convert a character to uppercase
   */
  function uppercase(Unicode.character c){
    if(
      ((c >= 97) && (c <= 123))                                   // US-ASCII
        || ((c >= 224) && (c <= 246))                             // ISO-8859-1 (latin-1)  v
        || ((c >= 248) && (c <= 254))                             // ISO-8859-1 (latin-1)  ^
        || ((c >= 65345) && (c <= 65370))                         // caracteres demi/pleine chasse
        || ((c >= 944) && (c <= 974))                             // grec
        || ((c >= 1072) && (c <= 1103))                           // cyrillique
    ){
        c - 32
    } else if (
      ((c >= 257) && (c <= 319) && ((mod(c, 2)) == 1))         // latin étendu A v
        || ((c >= 314) && (c <= 328) && ((mod(c, 2)) == 0))
        || ((c >= 331) && (c <= 375) && ((mod(c, 2)) == 1))
        || (c == 378) || (c == 380) || (c == 382)                      // latin étendu A ^
        || (c == 389) || (c == 392) || (c == 396) || (c == 402)         // latin étendu B v
        || (c == 409) || (c == 417) || (c == 419) || (c == 421)
        || (c == 424) || (c == 429) || (c == 432) || (c == 434)
        || (c == 436) || (c == 438) || (c == 453) || (c == 456)
        || (c == 459)
        || ((c >= 462) && (c <= 476) && ((mod(c, 2)) == 0))
        || ((c >= 479) && (c <= 495) && ((mod(c, 2)) == 1))
        || (c == 498)
        || ((c >= 501) && (c <= 563) && ((mod(c, 2)) == 1))
        || (c == 572) || (c == 578) || (c == 585) || (c == 587)
        || (c == 589) || (c == 591)                                   // latin étendu B ^
        || ((c >= 7680) && (c <= 7935) && ((mod(c, 2)) == 1))          // latin étendu additionnel
        || (c == 8580)                                               // nombre latin : facteur 10
        || ((c >= 977) && (c <= 1007) && ((mod(c, 2)) == 1))           // grec avec accents
        || ((c >= 1120) && (c <= 1153) && ((mod(c, 2)) == 1))          // cyrillique v
        || ((c >= 1163) && (c <= 1215) && ((mod(c, 2)) == 1))
        || ((c >= 1217) && (c <= 1230) && ((mod(c, 2)) == 0))
        || ((c >= 1232) && (c <= 1279) && ((mod(c, 2)) == 1))          // cyrillique ^
        || ((c >= 1280) && (c <= 1299) && ((mod(c, 2)) == 1))          // cyrillique additionnel
    ){
      c - 1
    } else if((c == 454) || (c == 457) || (c == 460) || (c == 499)){ // latin étendu B doubles lettres
      c - 2
    } else if (c == 255){                                        // special case : ÿ. Latin 1&A.
      376
    } else if ((c >= 9424) && (c <= 9449)){                     // lettres pastilles
      c - 26
    } else if ((c >= 1104) && (c <= 1119)){                     // cyrillique
      c - 80
    } else if (((c >= 7936) && (c <= 8047) && ((mod(c, 16)) <= 7))      // grec polytonique v
               || ((c >= 8064) && (c <= 8111) && ((mod(c, 16)) <= 7))){    // grev polytonique ^
        c + 8
    } else if ((c >= 1377) && (c <= 1414)){                     // arménien
      c - 48
    } else if ((c >= 8560) && (c <= 8575)){                     // nombres latins
      c - 16
    } else c
  }
}
