
(** Input signature of the functor [Eliom_notif.Make]
    [identity] is the type of values used to differentiate
    one client from another.
    [key] is the type of values designating a given resource.
    [notification] is the type of values to notifiy clients with.
    [equal_key] is a function testing the equality between two values
    of type [key].
    [equal_identity] is the same as [equal_key] but for values of type
    [identity].
    [max_resource] is the initial size for the hash table storing the data of
    clients listenning on resources, for best results it should be on the order
    of the expected number of different resources one may want to be able to
    listen to.
    [max_identity_per_resource] is the initial size for the tables storing the
    data of clients listenning on one given resource, fo best results it
    should be on the order of the expected number of clients that may listen
    on a given resource.
*)

module type S = sig
  type identity
  type key
  type notification
  val equal_key                  : key -> key -> bool
  val equal_identity             : identity -> identity -> bool
  val get_identity               : unit -> identity Lwt.t
  val max_resource               : int
  val max_identity_per_resource  : int
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
         ...

      let%server something some_stuff =
         ignore
           [%client
              (ignore (React.E.map
		        (handle_notification ~%some_stuff)
		        ~%(Notif_module.client_ev ())
	      ) : unit)
           ]
  
  **)

  val client_ev : unit -> (A.key * A.notification) Eliom_react.Down.t Lwt.t


  (** Call [clean freq] to launch an asynchronous thread clearing the tables
      from empty data every [freq] seconds
  *)
  val clean : float -> unit Lwt.t

end
