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

/**
 * Elvis Binding of the Datepicker Widget
 *
 * @category UI
 * @author François-Régis Sinot, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Datepicker}
 *
 * Demonstrating how to wrap a widget into an elvis.
 * No support for the "menu-style" datepicker.
 */

import stdlib.widgets.datepicker

/**
 * {1 Theming}
 *
 * The input always has classes "mlstate", "elvis", "datepicker".
 */

/**
 * {1 Sources for this elvis}
 */
type EDatepicker.sources =
{
  changed_date:   Event.source(Date.date)
}

type EDatepicker.options =
{
  initial_date: Date.date
  /* TODO: all options of WDatepicker.config except stylers */
}

@abstract type EDatepicker.implementation =
{
  /**
   * Parameters for WDatepicker
   */
  wdatepicker_id: string
  wdatepicker_config: WDatepicker.config
}

type EDatepicker.elvis = Elvis.elvis(EDatepicker.sources, EDatepicker.implementation)

@client EDatepicker = {{
  /**
   * {2 Constructors}
   */
  simple(initial_date : Date.date): EDatepicker.elvis =
  (
     make({ default_options with ~initial_date})
  )

  make(options: EDatepicker.options): EDatepicker.elvis =
  (
     id_datepicker  = "edatepicker_{Random.string(32)}"

     //Setup sources
     changed_date_net = Network.empty()

     //Translate to WDatepicker
     wdatepicker_config = WDatepicker.default_config

     display(theme) =
     (
         _theme_name = Elvis.Theme.get_classes(theme) /* TODO: use it */

         on_change_date = Network.broadcast(_, changed_date_net)

         xhtml = WDatepicker.edit(wdatepicker_config, on_change_date, id_datepicker, options.initial_date)

         dom = Dom.of_xhtml(xhtml)

         ~{dom xhtml}
     )
     sources : EDatepicker.sources =
        {
           changed_date  = (changed_date_net)
        }
     implem : EDatepicker.implementation =
        ({
           wdatepicker_id = id_datepicker
           ~wdatepicker_config
        })
     Elvis.make(sources, implem, display)
  )

  /**
   * A default set of options
   */
  default_options: EDatepicker.options =
          {initial_date = Date.epoch
         }

}}
