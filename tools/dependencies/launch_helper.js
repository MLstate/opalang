var min_node_version = 'v0.6.0',
    max_node_version = 'v0.8.2';

if (process.version < min_node_version) {
    console.error('Your version of node seems to be too old. Please upgrade to a more recent version of node (>= '+min_node_version+')');
    process.exit(1);
} else if (process.version > max_node_version) {
    console.warn('This version of node ('+process.version+') has not been tested with Opa. Use it at your own risks.');
}

dependencies = dependencies.filter(function(dependency, index, array) {
    // console.log('Checking', dependency, '...');
    try {
        module.require(dependency);
	return false;
    } catch(e) {
        return (e.code === 'MODULE_NOT_FOUND');
    }
})

if (dependencies.length > 0) {
    console.error(
	dependencies.length+' modules are missing.',
	'Please run: npm install -g '+dependencies.join(' ')
    );
    process.exit(1);
}
