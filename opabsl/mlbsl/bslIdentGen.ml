##register create : string -> (-> string)
let create prefix = IdentGenerator.alphanum_generator ~prefix
