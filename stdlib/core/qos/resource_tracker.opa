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
 * Manager of collectible resources, binding with QOS (appserver)
 *
 * @category WEB
 * @author Mathieu Barbin, 2011
 * @destination PUBLIC
 * @stability UNTESTED
**/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type ResourceTracker.manager('message, 'result) = external
type ResourceTracker.signal = external

/**
 * {1 Interface}
 */

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
   * Send a termination signal to a manager.
  **/
  term = %%BslResourceTracker.term%% : ResourceTracker.manager, ResourceTracker.signal -> void

  /**
   * Execute manually a step of garbage collector.
  **/
  garbage_collector = %%BslResourceTracker.garbage_collector%% : -> void
}}
