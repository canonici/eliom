open Lwt

module type S = sig
  type identity
  type key
  type notification
  val equal                      : identity -> identity -> bool
  val get_identity               : unit -> identity
  val max_ressource              : int
  val max_identity_per_ressource : int
end

module Make (A : S) = struct

  type notification_data = A.key * A.notification

  type notification_react =
    notification_data Eliom_react.Down.t
    * notification_data React.event
    * (?step: React.step -> notification_data -> unit)

  module Notif_hashtbl = Hashtbl.Make(struct
    type t    = A.key
    let equal = ( = )
    let hash  = Hashtbl.hash
  end)

  module Weak_tbl = Weak.Make (struct
    type t = (A.identity * notification_react) option
    let equal a b = match a, b with
      | None, None ->
	true
      | Some (a, b), Some (c, d) ->
	A.equal a c && b == d
      | _ -> false
    let hash = Hashtbl.hash
  end)

  module I = struct

    let tbl = Notif_hashtbl.create A.max_ressource
      
    let lock = Lwt_mutex.create ()

    let async_locked f = Lwt.async (fun () ->
      Lwt_mutex.lock lock >>= fun () ->
      f ();
      Lwt.return (Lwt_mutex.unlock lock)
    )

    let remove_if_empty wt key = async_locked (fun () ->
      if Weak_tbl.count wt = 0
      then Notif_hashtbl.remove tbl key
    )

    let remove v key = async_locked (fun () ->
      let () =
	try
	  let wt = Notif_hashtbl.find tbl key in
          Weak_tbl.remove wt v;
	  remove_if_empty wt key
	with Not_found -> ()
      in
      Lwt.return ()
    )

    let add v key = async_locked (fun () ->
      let wt =
	try
	  Notif_hashtbl.find tbl key
        with Not_found ->
	  let wt = Weak_tbl.create A.max_identity_per_ressource in
          Notif_hashtbl.add tbl key wt;
          wt
      in
      if not (Weak_tbl.mem wt v)
      then Weak_tbl.add wt v;
      Lwt.return ()
    )

    let iter =
      let iter (f : Weak_tbl.data -> unit Lwt.t) wt : unit =
	Weak_tbl.iter
	  (fun data -> Lwt.async (fun () -> f data))
	  wt
      in
      fun f key -> async_locked (fun () ->
	let () =
	  try
	    let wt = Notif_hashtbl.find tbl key in
	    let g data = match data with
              | None ->
		Weak_tbl.remove wt data;
		remove_if_empty wt key;
		Lwt.return ()
              | Some v ->
		f v;
		Lwt.return ()
	    in
            iter g wt;
	  with Not_found -> ()
	in
	Lwt.return ()
      )
  end

  let identity_r = Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope
    None

  (* notif_e consists in a server side react event,
     its client side counterpart,
     and the server side function to trigger it. *)
  let notif_e : notification_react Eliom_reference.eref =
    Eliom_reference.eref_from_fun
      ~scope:Eliom_common.default_process_scope
      (fun () ->
         let e, send_e = React.E.create () in
         let client_ev = Eliom_react.Down.of_react
             (*VVV If we add throttling, some events may be lost
               even if buffer size is not 1 :O *)
             ~size: 100 (*VVV ? *)
             ~scope:Eliom_common.default_process_scope e in
         (client_ev, e, send_e)
         (* I don't really need e, but I need to keep a reference on it during
            the session to avoid it beeing garbage collected. *))

  let set_identity identity =
    (* For each tab connected to the app,
       we keep a pointer to (identity, notif_ev) option in process state,
       because the table resourceid -> (identity, notif_ev) option
       is weak.
    *)
    Eliom_reference.get notif_e >>= fun notif ->
    Eliom_reference.set identity_r (Some (identity, notif)) >>= fun () ->
    Lwt.return ()

  let set_current_identity () =
    let identity = A.get_identity () in
    set_identity identity

  let listen (key : A.key) = Lwt.async (fun () ->
    set_current_identity () >>= fun () ->
    Eliom_reference.get identity_r >>= fun identity ->
    I.add identity key;
    Lwt.return ()
  )

  let unlisten (id : A.key) = Lwt.async (fun () ->
    Eliom_reference.get identity_r >>= fun identity ->
    I.remove identity id;
    Lwt.return ()
  )

  let notify ?(notforme = false) key content_gen =
    let f = fun (identity, ((_, _, send_e) as notif)) ->
      Eliom_reference.get notif_e >>= fun notif_e ->
      if notforme && notif == notif_e then
	Lwt.return ()
      else
        content_gen identity >>= fun content -> match content with
        | Some content -> send_e (key, content); Lwt.return ()
        | None -> Lwt.return ()
    in
    (* on all tabs registered on this data *)
    I.iter f key

  let client_ev () =
    let (ev, _, _) = Eliom_reference.get notif_e |> Lwt_main.run in
    ev

  let unlisten_wrapper ~key ~handler =
    unlisten key;
    handler

  let listen_wrapper ~key ~handler =
    listen key;
    handler

end
