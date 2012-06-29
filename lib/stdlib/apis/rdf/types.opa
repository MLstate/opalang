/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
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

