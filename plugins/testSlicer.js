
##register only_on_client : void -> string
function only_on_client(_void)
{
    return "[only_on_client] hello from CLIENT";
}

##register on_both_sides : void -> string
function on_both_sides(_void)
{
    return "[on_both_sides] hello from CLIENT";
}
