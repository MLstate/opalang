type page = {
	string path,
	string content,
	int counter
}

database application_name {
	page /page[{path}]
}

module Model {
  
	function get_content(path) {
		/application_name/page[{~path}]/counter++;
		/application_name/page[{~path}]/content
	}

	function set_content(path, content) {
		/application_name/page[{~path}]/content <- content
	}

	function statistics() {
		DbSet.iterator(/application_name/page)
	}

}

