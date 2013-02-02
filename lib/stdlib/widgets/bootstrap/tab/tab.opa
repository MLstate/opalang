import-plugin bootstrap-tab

/**
 * {2 Example:}
 *
 * {[
 *
 * import stdlib.themes.bootstrap
 * import stdlib.widgets.bootstrap.tab
 *
 * function page() {
 *
 *   <ul class="nav nav-tabs" id="myTab">
 *     <li class="active"><a id=#home-tab href="#home" onclick={function (_) Tab.show(#home-tab)}>Home</a></li>
 *     <li><a id=#profile-tab href="#profile" onclick={function (_) Tab.show(#profile-tab)}>Profile</a></li>
 *     <li><a id=#messages-tab href="#messages" onclick={function (_) Tab.show(#messages-tab)}>Messages</a></li>
 *     <li><a id=#settings-tab href="#settings" onclick={function (_) Tab.show(#settings-tab)}>Settings</a></li>
 *   </ul>
 *   <div class="tab-content">
 *     <div class="tab-pane active" id="home">... Home ...</div>
 *     <div class="tab-pane" id="profile">... Profile ...</div>
 *     <div class="tab-pane" id="messages">... Messages ...</div>
 *     <div class="tab-pane" id="settings">... Settings ...</div>
 *   </div>
 *
 * }
 *
 * Server.start(Server.http, { title: "Tab", ~page })
 *
 * }
 *
 */
@client
Tab = {{

  show(dom:dom) =
    (%%tab.show%%)(Dom.to_string(dom))

}}
