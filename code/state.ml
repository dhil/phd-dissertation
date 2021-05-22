(* Companion for "State of effectful programming"
   Tested with OCaml 4.10.0+multicore. *)

(* Generic direct-style incr_even *)
let even : int -> bool
  = fun n -> n mod 2 = 0

let incr_even : (unit -> int) * (int -> unit) -> unit -> bool
  = fun (get, put) () ->
  let st = get () in
  put (1 + st);
  even st

(* Delimited control *)
module Prompt : sig
  type 'a t
  val make : unit -> 'a t
  val reify : 'a t -> (('b -> 'a) -> 'a) -> 'b
  val install : 'a t -> (unit -> 'a) -> 'a
end = struct
  type 'a t = {
      install : (unit -> 'a) -> 'a;
      reify : 'b. (('b -> 'a) -> 'a) -> 'b
    }

  let make (type a) () =
    let module M = struct
        effect Prompt : (('b -> a) -> a) -> 'b
      end
    in
    let reify f = perform (M.Prompt f) in
    let install f =
      match f () with
      | x -> x
      | effect (M.Prompt f) k -> f (continue k)
    in
    { install; reify }

  let install { install; _ } = install
  let reify { reify; _ } = reify
  let resume k v = continue k v
end

module type CTRL = sig
  type ans
  val reset : (unit -> ans) -> ans
  val shift : (('a -> ans) -> ans) -> 'a
end

module Ctrl(R : sig type ans end) : sig
  include CTRL with type ans = R.ans
end = struct
  type ans = R.ans

  let p : ans Prompt.t = Prompt.make ()

  let reset m =
    Prompt.install p m

  let shift f =
    Prompt.reify p
      (fun k ->
        Prompt.install p
          (fun () ->
            f (fun x ->
                Prompt.install p
                  (fun () -> k x))))
end

module CtrlState
         (S : sig type s end)
         (R : sig type ans end): sig
  type s = S.s
  type ans = s -> R.ans * s

  val get : unit -> s
  val put : s -> unit

  val run : (unit -> R.ans) -> ans
end = struct
  type s = S.s
  type ans = s -> R.ans * s
  module Ctrl = Ctrl(struct type nonrec ans = ans end)

  let get ()  = Ctrl.shift (fun k -> fun st -> k st st)
  let put st' = Ctrl.shift (fun k -> fun st -> k () st')

  let run m =
    Ctrl.reset
      (fun () ->
        let x = m () in
        fun st -> (x, st))
end

module CtrlIntState = CtrlState(struct type s = int end)(struct type ans = bool end)

(* Monadic programming *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(** State monad **)
module type STATE_MONAD = sig
  type ans
  type s
  include MONAD

  val get : unit -> s t
  val put : s -> unit t
  val run : (unit -> ans t) -> s -> ans * s
end

module StateMonad(S : sig type s end)(R : sig type ans end): sig
  include STATE_MONAD with type s = S.s
                       and type ans = R.ans
end = struct
  type ans = R.ans
  type s = S.s
  type 'a t = s -> 'a * s

  let return : 'a -> 'a t
    = fun x -> fun st -> (x, st)

  let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    = fun m k -> fun st ->
      let (x, st') = m st in
      k x st'

  let get : unit -> s t
    = fun () st -> (st, st)

  let put : s -> unit t
    = fun st st' -> ((), st)

  let run : (unit -> ans t) -> s -> ans * s
    = fun m st -> m () st
end

module IntStateMonad = StateMonad(struct type s = int end)(struct type ans = bool end)

(** Continuation monad **)
module type CONTINUATION_MONAD = sig
  type r
  include MONAD with type 'a t = ('a -> r) -> r
end

module ContinuationMonad(R : sig type ans end): sig
  include CONTINUATION_MONAD with type r = R.ans
end = struct
  type r = R.ans
  type 'a t = ('a -> r) -> r

  let return : 'a -> 'a t
    = fun x -> fun k -> k x

  let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    = fun m k -> fun c ->
      m (fun x -> k x c)
end

module ContinuationStateMonad
         (S : sig type s end)
         (R : sig type ans end): sig
  type s = S.s
  type ans = R.ans
  include CONTINUATION_MONAD with type r = s -> ans * s

  val get : unit -> s t
  val put : s -> unit t
  val run : (unit -> ans t) -> s -> ans * s
end = struct
  type s = S.s
  type ans = R.ans
  module ContinuationMonad : CONTINUATION_MONAD with type r = s -> ans * s
    = ContinuationMonad(struct type nonrec ans = s -> ans * s end)
  include ContinuationMonad

  let get : unit -> s t
    = fun () -> fun k -> fun st -> k st st

  let put : s -> unit t
    = fun st' -> fun k -> fun st -> k () st'

  let run : (unit -> R.ans t) -> s -> R.ans * s =
    fun m st -> m () (fun x -> fun st -> (x, st)) st
end

module ContinuationIntStateMonad
  = ContinuationStateMonad(struct type s = int end)(struct type ans = bool end)

(** Free monad **)
module type FUNCTOR = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type FREE_MONAD = sig
  type 'a op
  type 'a free = Return of 'a
               | Op of 'a free op

  include MONAD with type 'a t = 'a free

  val do' : 'a op -> 'a free
end

module FreeMonad(F : FUNCTOR) : sig
  include FREE_MONAD with type 'a op = 'a F.t
end = struct
  type 'a op = 'a F.t
  type 'a free = Return of 'a
               | Op of 'a free F.t

  type 'a t = 'a free

  let return : 'a -> 'a t
    = fun x -> Return x

  let rec (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    = fun m k ->
    match m with
    | Return x -> k x
    | Op y -> Op (F.fmap (fun m' -> m' >>= k) y)

  let do' : 'a F.t -> 'a free
    = fun op -> Op (F.fmap (fun x -> Return x) op)
end

module type FREE_STATE = sig
  type s
  type 'r opsig = Get of (s -> 'r)
                | Put of s * (unit -> 'r)
  include FUNCTOR with type 'r t = 'r opsig
end

module FreeState(S : sig type s end) = struct
  type s = S.s
  type 'r opsig = Get of (s -> 'r)
                | Put of s * (unit -> 'r)
  type 'r t = 'r opsig

  let fmap : ('a -> 'b) -> 'a t -> 'b t
    = fun f op ->
    match op with
    | Get k -> Get (fun st -> f (k st))
    | Put (st', k) -> Put (st', fun st -> f (k ()))
end

module FreeIntStateMonad: sig
  include STATE_MONAD with type s = int
                       and type ans = bool
end = struct

  module rec FreeIntState : FREE_STATE with type s = int
    = FreeState(struct type s = int end)
  and FreeIntStateMonad : FREE_MONAD with type 'r op = 'r FreeIntState.opsig
    = FreeMonad(FreeIntState)

  open FreeIntState
  include FreeIntStateMonad

  type s = int
  type ans = bool

  let get : unit -> s t
    = fun () -> do' (Get (fun st -> st))

  let put : s -> unit t
    = fun st -> do' (Put (st, fun () -> ()))

  let rec run : (unit -> ans t) -> s -> ans * s
    = fun m st ->
    match m () with
    | Return x          -> (x, st)
    | Op (Get k)        -> run (fun () -> k st) st
    | Op (Put (st', k)) -> run k st'
end

(** Monadic reflection **)
module Reflect
         (M : MONAD)
         (R : sig type ans end): sig
  type ans = R.ans

  val reify : (unit -> ans) -> ans M.t
  val reflect : 'a M.t -> 'a

end = struct
  type ans = R.ans
  effect Reflect : 'a M.t -> 'a

  let reify : (unit -> ans) -> ans M.t
    = fun f ->
    let open M in
    match f () with
    | x -> return x
    | effect (Reflect m) k -> m >>= (continue k)

  let reflect : 'a M.t -> 'a
    = fun m ->
    perform (Reflect m)
end

module ReflectIntStateMonad
  = Reflect(IntStateMonad)(struct type ans = bool end)

module ReflectIntState = struct
  open ReflectIntStateMonad

  let get : unit -> int
    = fun () -> reflect (IntStateMonad.get ())

  let put : int -> unit
    = fun st -> reflect (IntStateMonad.put st)

  let run : (unit -> bool) -> int -> bool * int
    = fun m st -> IntStateMonad.run (fun () -> reify m) st
end

(* Generic monadic incr_even *)
module MonadExample(T : STATE_MONAD with type s = int) = struct
  let incr_even : unit -> bool T.t
    = fun () ->
    let open T in
    (get ()) >>= (fun st -> put (1 + st)
             >>= (fun () -> return (even st)))
end

(** Effect handlers **)
module type STATE_HANDLER = sig
  type s

  val get : unit -> s
  val put : s -> unit
  val run : (unit -> 'a) -> s -> 'a * s
end

module StateHandler(S : sig type s end) : STATE_HANDLER with type s = S.s = struct
  type s = S.s

  effect Put : s -> unit
  let put st = perform (Put st)

  effect Get : unit -> s
  let get () = perform (Get ())

  let run
    = fun m st ->
    let f = match m () with
      | x -> (fun st -> (x, st))
      | effect (Put st') k -> (fun st -> continue k () st')
      | effect (Get ()) k -> (fun st -> continue k st st)
    in f st
end

module IntStateHandler = StateHandler(struct type s = int end)

let run_examples () =
  let examples = [
      "builtin", (fun st ->
        let st = ref st in let v = !st in st := 1 + v; (even v, !st));
      "pure state passing", (fun st -> (even st, 1 + st));
      "shift/reset", (fun st ->
        CtrlIntState.run (incr_even CtrlIntState.(get, put)) st);
      "state monad", (fun st ->
        let module MonadStateExample = MonadExample(IntStateMonad) in
        IntStateMonad.run MonadStateExample.incr_even st);
      "continuation monad", (fun st ->
        let module ContinuationMonadExample = MonadExample(ContinuationIntStateMonad) in
        ContinuationIntStateMonad.run ContinuationMonadExample.incr_even st);
      "free monad", (fun st ->
        let module FreeMonadExample = MonadExample(FreeIntStateMonad) in
        FreeIntStateMonad.run FreeMonadExample.incr_even st);
      "monadic reflection", (fun st ->
        ReflectIntState.run (incr_even ReflectIntState.(get, put)) st);
      "state handler", (fun st ->
        IntStateHandler.run (incr_even IntStateHandler.(get, put)) st) ]
  in
  List.map (fun (s, f) -> (s, f 4)) examples
(* module IntStateMRefl : MREFL with type ans := bool and type 'a t = 'a IntState.t
 *   = MRefl(struct type ans = bool end)(IntState)
 * 
 * let get () = IntStateMRefl.reflect (IntState.get ())
 * let put st = IntStateMRefl.reflect (IntState.put st)
 * let run m st = IntState.run (IntStateMRefl.reify m) st
 * 
 * let even : int -> bool
 *   = fun n -> n mod 2 = 0
 * 
 * let incr_even : unit -> bool
 *   = fun () ->
 *   let st = get () in
 *   put (1 + st);
 *   even st *)
