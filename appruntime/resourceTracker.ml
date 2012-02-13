(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

#<Debugvar:RESOURCE_TRACKER_DEBUG>

type ste_private
type msg_private
type res_private

type key = int

type signal =
    [ `Expired
    | `Killed
    | `Closed
    | `Collected]

type ('args,'res) sync =
  | Sync of ('args -> 'res)
  | ASync of ('args -> ('res -> unit) -> unit)

let proj_sync f1 f2 = function
  | Sync f -> Sync (fun x -> (f2 (f (f1 x))))
  | ASync f -> ASync (fun x k -> f (f1 x) (fun r -> k(f2 r)))

type cbs =
  | CbNormal of (key ref * (b * signal ,unit) sync)
  | CbLight of (signal,unit) sync
  | Nop

and ('m,'r,'s) h = {
  name:string;
  on_delete: ('s*signal,unit) sync;
  on_message: (('m,'r)t * 's * 'm, 's * 'r) sync;
  expire: ('s,signal option) sync;
  decide: (('m,'r)t * 's * signal, unit) sync;
}

and ('m,'r,'s) t_private = {
  id : key ref;
  rh : ('m,'r,'s) h;
  mutable cbs : cbs ref list;
  mutable cbs_ref : cbs ref list;
  mutable gp : g;
  mutable state : 's;
  msgs : ('m*('r->unit)*((exn -> unit) option))  Queue.t;
  mutable rsize : int;
  mutable active: bool;
}

and  ('m,'r)t = ('m,'r,ste_private) t_private

and b = (msg_private,res_private,ste_private) t_private

and g' = {
  mutable mem: key ref list;
  mutable size: int;
}

and g = g' ref

exception Not_Alive
exception Async_Call

external black : (_,_) t -> b = "%identity"
external grey : ('a,'b,_) t_private -> ('a,'b) t= "%identity"
external unblack : b -> (_,_) t = "%identity"

external unblack_state : ste_private -> _ = "%identity"

module Manager : sig

  type m

  val make : unit -> m

  val default : m

  val add : m -> (_,_) t -> unit

  val get : m -> key -> b option

  val remove : m -> (_,_) t -> unit

  val fold : m -> ('a -> b -> 'a) -> 'a -> 'a

end
  =
struct

  module W = WeakResArray
  module C = Counter
  type m =
      {
        table:b W.t;
        counter: C.t;
      }

  let make () =
    {
      table=W.create 1024;
      counter=C.make 1024;
    }

  (* Default Resource manager *)
  let default = make ()

  let add vrm r =
    let key = C.get_key vrm.counter in
    W.set vrm.table key (Some (black r));
    r.id:=key

  let get vrm i = W.get vrm.table i

  let remove vrm r = W.remove vrm.table !(r.id); C.release_key vrm.counter !(r.id)

  let fold vrm f a = W.fold_left f a vrm.table

end

module type M = sig

  (* module Group : sig *)

  (*   val make : unit -> g *)

  (*   val empty : g *)

  (*   val merge : g -> g -> g *)

  (*   val register : (_,_) t -> g -> unit *)

  (*   val iter : (b -> unit) -> g -> unit *)

  (* end *)

  module Expire : sig
    type 'a t = 'a state -> signal option * Time.t option
    and context =
        [ `Date of Time.t
        | `Timeout of Time.t
        | `Limit of int
        | `And of context list
        | `Or of context list ]
    and 'a state =
        {
          mutable limit: int;
          mutable last_use: Time.t;
          mutable cancel: Scheduler.async_key option;
          mutable state: 'a;
        }
    val init : 'a -> 'a state
    val make : context -> 'a t
  end

  (* val handler' : *)
  (*   string -> *)
  (*   ( 's * signal , unit ) sync -> *)
  (*   ( 's , signal option ) sync -> *)
  (*   (('m,'r)t*'s*'m , 's*'r ) sync -> *)
  (*   (('m,'r)t*'s*signal , unit) sync -> *)
  (*   ('m,'r,'s) h *)

  val handler :
    string ->
    ( 's-> signal -> unit ) ->
    ( 's -> signal option ) ->
    (('m,'r)t -> 's -> 'm -> 's*'r ) ->
    (('m,'r)t -> 's -> signal -> unit) ->
    ('m,'r,'s) h

  val handler_cps :
    string ->
    ( 's -> signal -> (unit -> unit) -> unit ) ->
    ( 's -> (signal option -> unit ) -> unit ) ->
    (('m,'r)t -> 's -> 'm -> ( ('s*'r) -> unit ) -> unit ) ->
    (('m,'r)t -> 's -> signal -> (unit -> unit) -> unit) ->
    ('m,'r,'s) h

  val handler_timer :
    string ->
    ('s -> signal -> unit) ->
    's Expire.t ->
    (('m,'r) t -> 's -> 'm -> 's * 'r) ->
    (('m, 'r) t -> 's -> signal -> unit) ->
    ('m, 'r, 's Expire.state) h

  val resource : ('m,'r,'s) h -> 's -> ?depends: b list -> unit -> ('m,'r) t

  val resource_timer : ('m,'r,'s Expire.state) h -> 's -> ?depends: b list -> unit -> ('m,'r)t

  val expire : (_,_)t -> signal option

  val expire_cps : (_,_)t -> ?err_cont:(exn -> unit) -> (signal option -> unit)  -> unit

  val call : ('m,'r)t -> 'm -> 'r

  val call_cps : ('m,'r)t -> 'm -> ?err_cont:(exn -> unit) -> ('r -> unit) -> unit

  val kill : (_,_)t -> signal ->  unit

  val alive : (_,_)t -> bool

  val collect : unit -> unit

  val register : (_,_)t -> (signal -> unit) -> unit

  val register_cps : (_,_)t -> (signal -> unit) -> unit

end

let string_of_signal = function
  | `Expired -> "Expired"
  | `Killed -> "Killed"
  | `Closed -> "Closed"
  | `Collected -> "Collected"

let make sched manager =
  let module Implem =
      struct
        module Group =
        struct

          let make () = ref {mem=[];size=0}

          let empty = make ()

          let merge c1 c2 =
            if c1 == empty then (c1:=!c2;c2)
            else if c2 == empty then (c2:=!c1;c1)
            else
              let mem = !(c1).mem@(!(c2).mem) in
              let size = !(c1).size + !(c2).size in
              ((!c1).mem <- mem;
              (!c1).size <- size;
              c2:=!c1;
              c1)

          let register r c =
            (!c).mem <- r.id::(!c).mem;
            (!c).size <- (!c).size +1


          let iter f g =
            let k id =
              if !id >= 0
              then match Manager.get manager !id with
              | Some r -> f r
              | None -> ()

            in List.iter k (!g).mem

        end

        module Expire = struct
          type 'a t = 'a state -> signal option * Time.t option
          and context =
              [ `Date of Time.t
              | `Timeout of Time.t
              | `Limit of int
              | `And of context list
              | `Or of context list ]
          and 'a state =
              {
                mutable limit: int;
                mutable last_use: Time.t;
                mutable cancel:Scheduler.async_key option;
                mutable state: 'a;
              }

          let init x =
            {limit=0;last_use=Time.now ();cancel=None;state=x}

          let rec make (c:context) : ('a state -> signal option * Time.t option) = match c with
            | `Date d -> (fun _ ->
                let t = Time.difference (Time.now()) d in
                if Time.is_positive t
                then None,Some t
                else (Some `Expired),None)
            | `Timeout t -> (fun i ->
                let dur = Time.difference i.last_use (Time.now ()) in
                let d = Time.difference dur t in
                if Time.is_positive d
                then None,Some d
                else (Some `Expired),None)
            | `Limit l -> (fun i -> if i.limit >= l then (Some `Expired),None else None,None)
            | `And [] -> (fun _ -> None,None)
            | `And (x::[]) -> make x
            | `And (x::xs) ->
                let f1 = make x in
                let f2 = make (`And xs) in
                (fun i -> match f1 i with
                | Some _,_ -> f2 i
                | None,t -> None,t)
            | `Or [] -> (fun _ -> None,None)
            | `Or (x::[]) -> make x
            | `Or (x::xs) ->
                let f1 = make x in
                let f2 = make (`Or xs) in
                (fun i -> match f1 i with
                | Some x,t -> Some x,t
                | None,None -> f2 i
                | None,Some t1 -> match f2 i with
                  | a,Some t2 -> a,Some(max t1 t2)
                  | a,None -> a,Some t1)
        end

        let alive r = !(r.id) <> -1



        let kill r s =
          if alive r
          then
            begin
              #<If> Logger.debug "[ResourceTracker] Remove '%s %d' with signal '%s'" r.rh.name !(r.id) (string_of_signal s) #<End>;
              (* prevent from calling kill function twice *)
              Manager.remove manager r;
              r.id:=-1;
              Queue.iter (fun (_,_,k) -> Option.iter (fun f -> f Not_Alive) k) r.msgs;
              Queue.clear r.msgs;
              (* first cancel all registered callbacks *)
              let _ = List.iter (fun cb -> cb:=Nop) r.cbs_ref in
              (* then inform the connected resources *)
              let _ = List.iter (fun r -> match !r with
                | Nop -> ()
                | CbLight f ->
                    begin
                      match f with
                      | Sync f -> f s
                      | ASync f -> Scheduler.push sched (fun () -> f s (fun _ -> ()))
                    end
                | CbNormal(id,f) ->
                    if !id >= 0
                    then
                      let r = Manager.get manager !id in
                      begin
                        match r,f with
                        | None,_ -> ()
                        | Some r,Sync f -> f (r,s)
                        | Some r,ASync f ->
                            Scheduler.push sched (fun () -> f (r,s) (fun _->()) )
                      end
              ) r.cbs in
              (* kill the current resource *)
                match r.rh.on_delete with
                | Sync f -> f (r.state,s)
                | ASync f -> Scheduler.push sched (fun () -> f (r.state,s) (fun _ -> ()))
        end
          else
            raise Not_Alive


        let call r m =
          if alive r
          then
            match r.rh.on_message with
            | Sync f ->
                let state,result = f (r,r.state,m) in
                r.state <- state;
                result
            | ASync f ->
                let b = ref None in
                f (r,r.state,m) (fun res -> b:=Some res);
                match !b with
                | None -> raise Async_Call
                | Some (state,result)-> r.state <- state;result
          else
            raise Not_Alive

        let call_cps r m ?err_cont cont =
          if alive r
          then
            match r.rh.on_message with
            | Sync f ->
                let state,result = f (r,r.state,m) in
                r.state <- state;
                Scheduler.push sched (fun () -> cont result)
            | ASync f ->
                Queue.add (m, cont, err_cont) r.msgs;
                if r.active
                then r.active <- false;
                let rec process () =
                  if Queue.is_empty r.msgs
                  then r.active <- true
                  else
                    let message,cont,_ = Queue.pop r.msgs in
                    f (r,r.state,message) (fun (state,result) ->
                      r.state <- state;
                      Scheduler.push sched process;
                      Scheduler.push sched (fun _ -> cont result)
                    )
                in process ()

          else Option.iter (fun f -> f Not_Alive) err_cont

        (* let project_handler (proj_msg:'a-> 'c) (proj_res:'b->'d) (h:('a,'b,'s)h) : ('c,'d,'s) h = *)
        (*   let on_message = *)
        (*     proj_sync *)
        (*       (fun (r,s,m) -> (r,s,proj_msg m)) *)
        (*       (fun (s,r) -> (s,proj_res r)) *)
        (*       (fun (r,s,m) -> call r (proj_msg m) h.on_message) *)
        (*   in { h with on_message} *)

        (* let project_resource (proj_msg:'a-> 'c) (proj_res:'b->'d) (r:('a,'b,'s)t) : ('c,'d,'s) t = *)
        (*   let rh = project_handler proj_msg proj_res r.rh *)
        (*   in { r with rh } *)


        let register r cb =
          if alive r
          then
            (r.cbs <- (ref (CbLight (Sync cb)))::r.cbs;
            r.rsize<-succ r.rsize)
          else
            cb `Closed

        let register_cps r cb =
          let cb s _ = cb s in
          if alive r
          then
            (r.cbs <- (ref (CbLight (ASync cb)))::r.cbs;
            r.rsize <-succ r.rsize)
          else
            Scheduler.push sched (fun _ -> cb `Closed (fun () -> ()))

        let expire r =
          if alive r
          then
            match r.rh.expire with
            | Sync f -> f r.state
            | ASync f ->
                let b = ref None in
                f r.state (fun res -> b:=Some res);
                match !b with
                | None -> raise Async_Call
                | Some (exp)-> exp
          else
            raise Not_Alive

        let expire_cps r ?err_cont cont =
          if alive r
          then
            match r.rh.expire with
            | Sync f ->
                let res = f r.state in
                Scheduler.push sched (fun () -> cont res)
            | ASync f -> f r.state cont
          else Option.iter (fun f -> f Not_Alive) err_cont

        let collect () =
          let _exp,_nexp,_rem = Manager.fold manager
            (fun  (exp,nexp,rem) r ->
              (match expire r with
                None -> (exp,nexp+1,rem)
              | Some e -> ignore(kill r e); (exp+1,nexp,r::rem)))
            (0,0,[])
          in ()


        (* Create a resource and add it to Manager *)
        (*     You must provide ... *)
        let resource rh state ?depends () : ('a,'b) t =
          let depends = Option.default [] depends in
          let depends_group = depends in
          let gp = List.fold_left
            (fun group parent -> Group.merge group parent.gp) Group.empty depends_group in
          let resource = {
            id= ref (-1);
            rh;
            cbs=[];
            cbs_ref=[];
            gp;
            state;
            msgs=Queue.create();
            rsize=0;
            active=true;
          } in
          let blackresource = grey resource in
          (* if register then ignore(Group.register blackresource group); *)
          Manager.add manager blackresource;
          #<If> Logger.debug "[ResourceTracker] Create %s [%d]" rh.name
            !(resource.id) #<End>;
          let _ =
            let f = match rh.decide with
              | Sync f ->
                  (Sync(fun ((r:b),(s:signal)) ->
                    #<If> Logger.debug "[ResourceTracker] Remove dependent resource [%d]" !(r.id) #<End>;
                    f (unblack r,unblack_state r.state,s)))
              | ASync f ->
                  (ASync(fun ((r:b),(s:signal)) _ ->
                    Scheduler.push sched (fun _ -> f (unblack r,unblack_state r.state,s) (fun _ -> ()) )))
            in
            let cb = ref (CbNormal(resource.id,f)) in
            List.iter
              (fun (p) ->
                p.cbs <- cb::p.cbs;
                resource.cbs_ref <- cb::resource.cbs_ref)
              depends
          in
          Scheduler.finalise sched (fun r -> kill r `Collected) blackresource;
          blackresource

        let handler' name on_delete expire on_message decide = {
          name;
          on_delete;
          on_message;
          expire;
          decide;
        }

        let c2 f (a,b)= f a b
        let c3 f (a,b,c)=f a b c

        let handler name on_delete expire on_message decide =
          handler' name (Sync (c2 on_delete)) (Sync expire) (Sync (c3 on_message)) (Sync (c3 decide))

        let handler_cps name on_delete expire on_message decide =
          handler' name (ASync (c2 on_delete)) (ASync expire) (ASync (c3 on_message)) (ASync (c3 decide))


        let resource_timer rh state ?depends () =
          resource rh (Expire.init state) ?depends ()

        let handler_timer name kill_fun expire_fun on_message_fun decide_fun =

          let cancel_timeout s = match s.Expire.cancel with
              Some key -> Scheduler.abort sched key; s.Expire.cancel <- None
            | None -> () in

          let create_timeout s r = function
            | Some next ->
                let key = Scheduler.sleep sched next (fun _ -> kill r `Expired) in
                s.Expire.cancel<- Some key
            | None -> () in

          let update s =
            s.Expire.last_use <- Time.now ();
            s.Expire.limit <- s.Expire.limit+1 in

          let set_timeout r exp s =
            match exp with
            | Some signal,_->
                cancel_timeout s;
                kill r signal
            | None,next ->
                cancel_timeout s;
                create_timeout s r next;
                update s
          in
          let on_message_fun r s m =
            let state,res = on_message_fun r s.Expire.state m in
            s.Expire.state <- state;
            set_timeout r (expire_fun s) s;
            s,res
          in
          let f = (fun r s signal -> decide_fun r s.Expire.state signal) in
          handler name
            (fun s signal -> cancel_timeout s; kill_fun s.Expire.state signal)
            (fun s -> fst (expire_fun s))
            on_message_fun
            f

      end
  in (module Implem: M)


module Default = (val make Scheduler.default Manager.default : M)


(* let ht = Default.handler_timer *)
(*   "Test" *)
(*   (fun () _ -> ()) *)
(*   (Default.Expire.create (`Timeout 5.)) *)
(*   (fun _ k () -> k,()) *)
(*   (fun _ _ _ -> ()) *)

(* let r = Default.resource_timer ht () () *)

(* let _ = Default.call r () *)
