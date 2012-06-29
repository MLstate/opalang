/*
    Copyright © 2011, 2012 MLstate

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

type Themes.state = stringmap(stringmap(finite_single_thread_lazy(string)))

Themes = {{

  @private
  dir = "stdlib/themes/"

  @private
  default_theme_name = "default"

  @private @server
  themes = Mutable.make(Map.empty : Themes.state)

  @private
  user_theme = UserContext.make(default_theme_name)

  /**
   * Used to declare a new theme
   */
  load_theme(theme_name,map_resource) =
    gen_dyn_res(resource)=
      FiniteSingleThreadLazy.make(->
        parameters = {expiration={none}
                      consumption={unlimited}
                      visibility={shared}}
        config = { prefix=some(theme_name) sufix=none onaccess=none randomize=true }
        DynamicResource.publish_extend("", resource, parameters, config)
      )
    map_resource = Map.fold(k,v,map ->
      name = String.replace_first("{dir}{theme_name}/", "", k)
      Map.add(name,gen_dyn_res(v),map)
    ,map_resource,Map.empty)
    map = themes.get()
    themes.set(Map.add(theme_name, map_resource, map) )


  /**
   * Get the list of loaded theme
   */
  @server
  get_theme_list() : list(string) =
    Map.To.key_list(themes.get())

  /**
   * Change the theme for the current user only
   */
  @server
  set(name) =
    map = themes.get()
    if Map.mem(name, map)
    then UserContext.change(_ -> name, user_theme)
    else Log.info("THEME","theme '{name}' it not loaded yet, cannot be default")

  /**
   * Set the default themes
   */
  @server
  set_default(name) =
    do Log.info("THEME", "set_default_theme to {name}" )
    map = themes.get()
    if Map.mem(name, map)
    then UserContext.set_default(name, user_theme)
    else Log.info("THEME","theme '{name}' it not loaded yet, cannot be default")


  /**
   * Get the url of a resource with the right theme
   */
  get_resource_url(resource_name)=
    theme_name = UserContext.execute(x->x,user_theme)
    do Log.info("THEME", "current_theme: {theme_name}" )
    rec with_theme(theme_name) =
      map = themes.get()
      match Map.get(theme_name,map) with
        | {some=theme} ->
          match Map.get(resource_name, theme) with
            | {some=lazy} -> FiniteSingleThreadLazy.force(lazy)
            | {none} ->
              if theme_name == default_theme_name
              then bad_resource_url(resource_name,"resource not found for theme {theme_name}")
              else with_theme(default_theme_name)
          end
        | {none} ->
          if theme_name == default_theme_name
          then bad_resource_url(resource_name, "theme not found")
          else with_theme(default_theme_name)
      end
    with_theme(theme_name)

  @private
  bad_resource_url(resource_name, err) =
    do Log.warning("THEME", "{resource_name} - {err}")
    "/_internal_/null"
}}
