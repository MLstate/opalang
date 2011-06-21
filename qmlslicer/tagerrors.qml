val f = @client(@server(4));;

val f x y z = (x + y) / z;;
val g x y = @client((@server(f 1)) x y);;

val g x = @client(!/db/x);;

val f = (@client( (@server( (+) 0 )) 1));;

val g x = @client( x + 1);;
val rec pipe x0 f = @server(f x0) ;;
val x = @client(pipe 1 g);;


val add_client y z = @client(1)
val foo = @server(add_client 0 1)


(* TODO do not slice this kind of function !
without typing, I think it will everytime pass
*)
val add_client y z = @client(1)
val foo = let f = add_client 0 in @server(f 1)


