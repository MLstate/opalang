/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
