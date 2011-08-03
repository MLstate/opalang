/**
* Javascript for opadoc static pages
**/

String.prototype.endsWith = function(str){return (this.match(str+"$")==str)}

// FIXME: do not use global vars !!!
var global_config = "", global_id = "", global_plugins = "";

function switchTab(tabName) {
    //window.location.search = "?tab="+tabName;
    $("#"+global_id).jstree("destroy");
    make_tree(global_config, global_id, global_plugins, tabName);
}

function switchFile(fileName, nodeId) {
    var sep = "/!/";
    if (nodeId === undefined) nodeId = "";
    window.location.hash = "#"+fileName+sep+nodeId;
    //open_parent_node(global_id);
}

function getUrlVars() {
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++) {
        hash = hashes[i].split('=');
        vars.push(hash[0]);
        try {
            vars[hash[0]] = hash[1].split('#')[0];
        } catch(e) {
            vars[hash[0]] = hash[1];
        }
    }
    return vars;
}

/**
* {1 Tree elements}
*
* Requirements: jQuery, jsTree, scrollTo
*/

function open_node(tree, node) {
    tree.jstree('open_node', node, false, false);
}

function open_parent_node(id, scroll) {
    var anchor = window.location.hash;
    if (anchor === "") return;
    var last_part = anchor.lastIndexOf("/!/");
    var file = anchor.slice(0, last_part).substr(1);
    var node_id = anchor.slice(last_part+3, anchor.length);
    if (node_id === "" && file.endsWith(".html"))
        node_id = file.substr(0, file.length-5);
    var node_anchor = "#node_"+node_id.replace(/\./g, "\\.");
    $(node_anchor).parents().each(function(i, e) {
        open_node($('#' + id), $(e));
    });
    $("#"+id).jstree("deselect_all").jstree("select_node", node_anchor);
    //$(node_anchor + " > a").addClass("selected");
    if (!scroll) return;
    if (file.endsWith(".html"))
        $("#doc").attr("src", file+"#"+node_id);
    try {
        $("#"+id).scrollTo(node_anchor);
    } catch(e) {}
}

function doSearch(id) {
    var val = $("#search-tree").val();
    if (val === "") {
        $("#" + id).jstree("clear_search");
        $("#" + id).jstree("close_all", $("#" + id), true);
    } else {
        $("#"+id).jstree("search", val);
    }
}

function init_tree(id) {

    open_parent_node(id, true);

    $("#" + id + " ul li").bind("dblclick", function(e){
        $("#" + id).jstree("toggle_node", this);
        e.stopImmediatePropagation();
    });

    $(document).keypress(function(e) {
        if (e.which == 13) {
            $(".jstree-hovered").click();
        }
    });

    $("#" + id + " ul li a").each(function(i, e) {
        var href = $(e).attr("href");
        var file = href.split("#")[0];
        var anchor = href.split("#")[1];
        if ($(e).filter("[href^='\#']").length == 0) {
            $(e).bind("click", function(e) {
                switchFile(file, anchor);
                $("#doc").attr("src", href);
            });
            $(e).attr("href", "javascript:void(0)");
        }
    });

}

function receiveMessage(e) {
    var file = e.data.split("#")[0];
    var node_id = e.data.split("#")[1];
    switchFile(file, node_id);
    open_parent_node(global_id, true);
}

function make_tree(config, id, plugins, tab) {

    jQuery(document).ready(function($){
        $.history.init(function(hash){
            if(hash == "") {
                // initialize your app
            } else {
                // restore the state from hash
                open_parent_node(global_id, false);
            }
        },
        { unescape: ",/" });
    });

    if (global_id === "") {
        $("<form action=\"javascript:doSearch('"+id+"')\"><input placeholder='Filter' id='search-tree'/><input type='submit' style='display: none;' value='Filter'/></form>").insertBefore("#"+id);

        $("#toggler").bind("click", function(e) {
            $(".sidebar").toggle();
            var ml = "0px";
            if ($(".sidebar:visible").length > 0) {
                ml = "250px";
            }
            $("#toggler").css("margin-left", ml);
            $(".main").css("margin-left", ml);
        });

        $("#" + id).jstree._themes = "resources/themes/";

        window.addEventListener('message', receiveMessage, false);

    }

    global_config = config;
    global_id = id;
    global_plugins = plugins;

    $(function() {

        //tab = getUrlVars()["tab"];
        if (tab === "" || tab == undefined)
            tab = "packages";
        var content = {};
        if (tab === "values") {
            content = values_tree_json();
        } else if (tab === "types") {
            content = types_tree_json();
        } else if (tab === "files") {
            content = files_tree_json();
        } else {
            content = packages_tree_json();
        }

        var current_class = "current";
        $("ul.menu li").removeClass(current_class);
        $("ul.menu li."+tab+"_tab").addClass(current_class);

        $("#" + id).bind("loaded.jstree", function (event, data) {
            init_tree(id);
        }).jstree({
            core      : config,
            json_data : {
                data : content,
                progressive_render : false
            },
            search    : {
                show_only_matches : true,
                search_method : "jstree_title_contains"
            },
            ui        : {
                select_limit : 1
            },
            themes    : {
                theme: 'classic',
                url: 'resources/themes/classic/style.css'
            },
            types     : {
                "types" : {
                    "package" : {
                        "max_depth" : 1,
                        "icon" : {
                            "image" : "resources/package_icon.png"
                        }
                    },
                    "module" : {
                        "icon" : {
                            "image" : "resources/module_icon.png"
                        }
                    },
                    "value" : {
                        "icon" : {
                            "image" : "resources/value_icon.png"
                        }
                    },
                    "type" : {
                        "icon" : {
                            "image" : "resources/type_icon.png"
                        }
                    },
                    "file" : {
                        "icon" : {
                            "image" : "resources/file_icon.png"
                        }
                    },
                    "default" : {
                        "hover_node" : true
                    }
                }
            },
            plugins   : plugins
        });
    });
}

function init() {
    jQuery(document).ready(function() {

        $("h1").each(function(i, e) {
            $(e).addClass("collapsible").bind("click", function(e) {
                $(this).nextUntil("h1").toggle();
                if ($(this).nextAll("h1").length == 0) {
                    $(this).parent().nextUntil("div.block:has(h1)").toggle();
                }
            });
        });

        var global_source;
        window.addEventListener('message', function(event) {
            global_source = event.source;
        }, false);

        $("a").each(function(i, e) {
            var href = $(e).attr("href");
            $(e).bind("click", function(event) {
                global_source.postMessage(href, "*");
            });
        });

    });
}

function initMessage() {
    var iframe = document.getElementById('doc').contentWindow;
    iframe.postMessage("Hello", "*");
}

/**
* A hack for avoiding an error, when opa generates static pages
* Opa generates javascript code like this :
*
* $('#drorwaflpuhsioqawokdsealauvjlact').css({ 'color': 'rgb(0,0,0)'})
*
**/
function $(){ return({ css : function() {} }); }
