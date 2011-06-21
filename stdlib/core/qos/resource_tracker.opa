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
 * Manager of collectable resources, binding with QOS (appserver)
 *
 * @category WEB
 * @author Mathieu Barbin, 2011
 * @destination PUBLIC
 * @stability UNTESTED
**/

type ResourceTracker.manager('message, 'result) = external
type ResourceTracker.signal = external

ResourceTracker = {{

  /**
   * Some Signal
  **/
  Signal = {{
    EXPIRATION = %%BslResourceTracker.Signal.expiration%% : ResourceTracker.signal
  }}

  /**
   * Creating a new manager.
  **/
  create = %%BslResourceTracker.create%% :
    /*state :*/       'state,
    /*on_message :*/  ('state, 'message -> ('state, 'result)),
    /*expire :*/      ('state -> option(ResourceTracker.signal)),
    /*collect :*/     ('state, ResourceTracker.signal -> void)
    -> ResourceTracker.manager('message, 'result)

  /**
   * Trying to access to the resource associated to its manager.
  **/
  call = %%BslResourceTracker.call%% :
    /*manager*/ ResourceTracker.manager('message, 'result),
    /*message*/ 'message
    /*result*/ -> 'result

  /**
   * Send a terminaison signal to a manager.
  **/
  term = %%BslResourceTracker.term%% : ResourceTracker.manager, ResourceTracker.signal -> void

  /**
   * Execute manually a step of garbage collector.
  **/
  garbage_collector = %%BslResourceTracker.garbage_collector%% : -> void
}}
