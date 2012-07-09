/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Types for rdf modules
 *
 * @author Anthonin Bonnefoy, 2011
 * @category api
 * @destination public
 */


/**
 *  A single row of rdf information
 */
type Rdf.element = (string,string,string)

/**
 * The representation of a rdf database
 */
type Rdf.base = list(Rdf.element)

/**
 *  A select element in a query
 */
type Rdf.select_element = 
  { all } 
  / { vars : list(string) } 

/**
 *  A where clause of a rdf query
 */
type Rdf.where_element = 
  { var : string }
  / { value : string }

/**
 *  A rdf query 
 */
type Rdf.query = 
  { 
    prefix : list((string,string))
    ; select : Rdf.select_element
    ; where :list( list(Rdf.where_element) )
  }

