// This is a JavaScript file, containing opa preprocessing directives

##register stammer : string -> string
##args(s)
{
    // this is a standard javascript code,
    // we are there in the body of a function, with an argument named 's'
    return "client is stammering: " + s + s + s ;
}
