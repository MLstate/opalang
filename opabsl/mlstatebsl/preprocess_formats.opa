/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
/** This file is meant to contains all bsl preprocess formats rules for opa files */

##format functions "#n = %%#k%% : #t\nformodule_#n = %%#k%% : #t"
##format bind-module "#m = #m"
##format bind "#n = (formodule_#n) ; "

##format in-module "#{functions}" 
##format sub-module "#m = \n#{#rec, functions}\n { #{bind-module bind} } in" 
##format module "#m = \n#{ sub-module, functions }\n  { #{bind-module bind} }"
##format plot_hierarchy "module #m #{#rec}"
