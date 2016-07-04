
module type S = sig
  type identity
  type key
  type notification
  val equal                      : identity -> identity -> bool
  val get_identity               : unit -> identity
  val max_ressource              : int
  val max_identity_per_ressource : int
end

module Make(A : S) :
sig

  (** Make client process listen on data whose index is [key] *)
  val listen : A.key -> unit

  (** Stop listening on data [key] *)
  val unlisten : A.key -> unit

  (** Call [notify key f] to send a notification to all clients currently
      listening on data referenced by [key].
      The notification is build using function [f],
      that takes the identity of the client as parameter,
      if a client is identified for this client process.

      If you do not want to send the notification for this identity,
      for example because it is not allowed to see this data,
      make function [f] return [None].

      If [~notforme] is [true], notification will not be sent to the tab
      currently doing the request (the one which caused the notification to
      happen). Default is [false].
  *)
  val notify : ?notforme:bool -> A.key ->
    (A.identity -> A.notification option Lwt.t) -> unit

  (** Returns the client react event. Map a function on this event to react
      to notifications from the server.
      For example:

      let%client handle_notification some_stuff ev =
         let (_, msgid) = ev in
         ...

      let%server something some_stuff =
         ignore
           [%client
              (ignore (React.E.map
		        (handle_notification ~%some_stuff)
		        ~%(Notif_module.client_ev ())
	      ) : unit)
           ]
  
  *)

  val client_ev : unit -> (A.key * A.notification) Eliom_react.Down.t

  val unlisten_wrapper : key:A.key -> handler:'a  -> 'a

  val listen_wrapper : key:A.key -> handler:'a  -> 'a

end
