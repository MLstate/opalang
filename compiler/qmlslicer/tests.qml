val f = 0 + 1;;

val f = (@client(0)) + (@server(1));;
val f = (@server(0)) + (@client(1));;

val f = let x = @client(0) in @server(x + 1);;

val f x = let x2 = @client(x) in @server( {foo=x; bar=x2} );;

val f x = {foo=@client(0); bar=@server(2)};;

val f x = {foo=@client(0); bar=@server(x + 2)};;

val f x = @server({foo=0; bar=2});;
val g = @client(f)

val f x = @server({foo=@client(0); bar=2});;

val g x = x + 1;;
val f x = @client(1 + @server(g x));; (* todo optimiser ca*)

val f = @server(4)
val g = @client(f)

val /db : int intmap
val g x = @client( let out = !/db/x in out);;

val str_concat a b = a;;
val md5 x = x;;
val insert_password = (* todo : faire en sorte que password ne se balade pas chez le client...*)
  @server(
    let current_client = "current_user"
    and password = str_concat "ma graine " @client("toto_dans_champ_input")
    in (* /db/current_client/password = *) md5 password
  )


val f =
  @server(
  let a = 1
  and b = 1 + @client(2)
  in a + b
  )


val f z = match @client(z) with
  | {x; y} -> (@server(x)) + (@client(x + y))
  | {x_y} -> x_y

val f x y z = (@client(x)) + (@server(y)) + z


val x = (* todo optimiser ca*)
  @server({
    foo={ a = 1; b = 2; c = @client(3); d = @server(4)};
    bar={a=1}
  })

val f = @server( {x=1} :: {x=@client(1)} );;

val /db : int intmap
val g x = @server(
  let x2 = 4 in
  let /db/x = @client(4) in
  let /db/x2 = @client(5) in
  1155
);;

val f = @client(
  let x = @server(
      {
        foo = @client(0);
        bar = 4;
        blah = let x2 = 1 in @client(x2)
      }.blah
    ) in x + 1
  );;

val /db : int intmap
val f =
  let x0 = @client(0) in
  let x1 = x0 + 1 in
  @server(let v0 = /db/x0
  and v1 = /db/x1 in
  @client(v0 + v1));;

val rec fact x =
  @client(match x with
  | 0 -> @client(1)
  | n -> @server( n * @client((fact (n - 1) ) )))
;;

val rec g x = f (@client(1) + x)
and f x0 = g (@server(1) + x0);;

val rec f x = g (@server(1) + x)
and g x0 = f (@client(1) + x0);;

val g x = x + 1;;
val pipe x0 f = @server(f x0) ;;
val x = @client( pipe 1 g );;

val f x = @server(x)
val g x0 = x0 + 1
val h x1 = @client(x1 + 2)
val i = [f;g;h]

val g x = @server( x + 1);;
val pipe x0 f = @client(f x0) ;;
val x = pipe 1 g;;

type 'a list = {hd : 'a ; tl : 'a list} / {nil : unit};;
val g x0 = @server( x0 + 1);;
val rec map f li = @client(
  match li with
  | { hd; tl } -> {hd = f hd; tl = map f tl}
  | { nil } -> li
);;
val li = [1;2;3;4;5;6;7;8;9;10];;
val x = map g li;;

val g x y z = @client( 1 + @server( z - (x + y) * x - y))

val /db : int
val jquery_get_val_truc = @client(4)
val f =
   @server(
    let _ = @/db <- 1
    and titi = 2 in
    let toto = @client(let out = @server(let /db = jquery_get_val_truc in ()) in out ) in
    let _ = @/db <- 5 in ()
  )

val add y z = 1
val foo =
  let x = @client(add)
  in @server(x 0 1)


val add_client y z = 1
val foo = let f = add_client 0 in @server(f 1)


val f =
@server({
  a = @client(1 + 0);
  b = @server(1 + 1);
  c = @client(1 + 2);
  d = @server(1 + 3);
  e = @server(1 + 4);
  f = @client(1 + 5);
  g = @server(1 + 6)
})


val rec f x = g (x + 1)
and h x2 = f x2
and g x3 = h x3;;

val x =
  { value=@side_specific( {f1=5; f2=9} ) };;

val x =
  @client({ value=@side_specific( {f1=5; f2=9} ) });;

val x =
  @server({ value=@side_specific( {f1=5; f2=9} ) });;

val do_not_slice = 5
val x = do_not_slice


val plus x y = x;;

val f x = @client( plus 1 x);;

val g f x = plus x @server(1);;

val h = g f 0;;



val f x y = @server(0)
val k = @server(1)
val g x =
  let y = @client(x) in
  f k y




val /db : int intmap
val add x y = y
val g x = @/db/x <- @client( add x 1 ) ;;
