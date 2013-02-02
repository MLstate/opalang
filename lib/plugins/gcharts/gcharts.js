function genericDynamicLoad(id, pack, str_data, str_opts, gfun) {
    var options = {
	packages:pack,
	callback:function(){
	    var data = new google.visualization.DataTable(str_data);
	    var opts = JSON.parse(str_opts);
	    var chart = gfun(document.getElementById(id));
	    chart.draw(data, opts);
	}
    }
    google.load("visualization", "1", options);
}

/**
 * @register {string, string, string -> void}
 */
function draw_area_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["corechart"], str_data, str_opts,
	function(v){return new google.visualization.AreaChart(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_bar_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["corechart"], str_data, str_opts,
	function(v){return new google.visualization.BarChart(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_column_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["corechart"], str_data, str_opts,
	function(v){return new google.visualization.ColumnChart(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_combo_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["corechart"], str_data, str_opts,
	function(v){return new google.visualization.ComboChart(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_gauge_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["gauge"], str_data, str_opts,
	function(v){return new google.visualization.Gauge(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_geo_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["geochart"], str_data, str_opts,
	function(v){return new google.visualization.GeoChart(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_intensity_map(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["intensitymap"], str_data, str_opts,
	function(v){return new google.visualization.IntensityMap(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_line_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["corechart"], str_data, str_opts,
	function(v){return new google.visualization.LineChart(v)});
}

/**
 * @register {string, string, string -> void}
 */
function draw_pie_chart(id, str_data, str_opts) {
    genericDynamicLoad(
	id, ["corechart"], str_data, str_opts,
	function(v){return new google.visualization.PieChart(v)});
}

// Doest not work, quite strangely
//
// function testTable(id) {
// 	packages:['table'],
// 	callback:function(){
// 	    var data = new google.visualization.DataTable('{"cols":[{"id":"cedhntbyzrlqmaeammmntubbnfhkiqzk","label":"Activity","type":"string"},{"id":"osbvrfcabsvfbjhomcaqwucrglhfqape","label":"Workday","type":"number"}],"rows":[{"c":[{"v":"working"},{"v":8}]},{"c":[{"v":"sleeping"},{"v":5}]},{"c":[{"v":"eating"},{"v":2}]},{"c":[{"v":"commuting"},{"v":1}]},{"c":[{"v":"other"},{"v":8}]}]}');
// 	    var opts = {};
// 	    var chart = new google.visualization.Table(document.getElementById(id));
// 	    chart.draw(data, opts);
// 	}
//     };
//     google.load("visualization", "1", options);
// }

// ##register draw_table : string, string, string -> void
// ##args(id, str_data, str_opts)
// {
//     testTable(id);
//     // genericDynamicLoad(
//     // 	id, ["table"], str_data, str_opts,
//     // 	function(v){return new google.visualization.Table(v)});

/**
 * @register {string, string, string -> void}
 */
function draw_tree_map(id, str_data, str_opts) {
    genericDynamicLoad(
    	id, ["treemap"], str_data, str_opts,
    	function(v){return new google.visualization.TreeMap(v)});
}
