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
/** This file is meant to contains all bsl preprocess formats rules for opa files */

##format functions "#n = %%#k%% : #t\nformodule_#n = %%#k%% : #t"
##format bind-module "#m = #m"
##format bind "#n = (formodule_#n) ; "

##format in-module "#{functions}" 
##format sub-module "#m = \n#{#rec, functions}\n { #{bind-module bind} } in" 
##format module "#m = \n#{ sub-module, functions }\n  { #{bind-module bind} }"
##format plot_hierarchy "module #m #{#rec}"
