(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* We represent futures by an object { fut : <promise> } with a single
   [fut] JavaScript Promise object which, by construction, *never*
   rejects. The promise is wrapped in an object because JavaScript's
   [resolve] which should be monadic [return] unfortunately also
   monadically [join]s. This JavaScript expression:

     Promise.resolve (Promise.resolve ("Noooooo!"))

   evaluates to:  Promise {<fulfilled>: "Noooooo!"}
     instead of:  Promise {<fulfilled>: Promise {<fulfilled>: ""Noooooo!""}}

   This makes it impossible to type [resolve] correctly in OCaml since it
   would need to have these two types:

     val resolve : 'a -> 'a Promise.t
     val resolve : 'a Promise.t -> 'a Promise.t

   In general this breaks type safety for example [bind]ing a ['a
   Fut.t Fut.t] value your function could end up with a ground value
   of type ['a] not the expected ['a Fut.t] value as argument. By
   wrapping the promise in an object we can control that. *)

type 'a t = Jv.t (* a JavaScript object of the form: { fut : <promise> } *)

let fut p = Jv.obj [| "fut", p |]
let promise f = Jv.get f "fut"
let promise' f = Jv.get f "fut"
let create () = (* Ugly as shit but that's what new Promise gives us.  *)
  let not_set = fun _ -> assert false in
  let is_set = fun _ -> Jv.throw (Jstr.v "The future is already set") in
  let setter = ref not_set in
  let set_setter resolve _reject = setter := resolve in
  let p = Jv.Promise.create set_setter in
  let set v = !setter v; setter := is_set in
  fut p, set

let await f k = Jv.Promise.await (promise f) k
let return v = fut @@ Jv.Promise.resolve v
let bind f fn = fut @@ Jv.Promise.bind (promise f) (fun v -> promise (fn v))
let map fn f = bind f (fun v -> return (fn v))
let pair f0 f1 =
  fut @@
  Jv.Promise.bind (promise f0) @@ fun v0 ->
  Jv.Promise.bind (promise f1) @@ fun v1 ->
  Jv.Promise.resolve (v0, v1)

let of_list fs =
  let arr = Jv.of_list promise' fs in
  let all = Jv.Promise.all arr in
  let to_list l = Jv.Promise.resolve (Jv.to_list Obj.magic l) in
  fut @@ Jv.Promise.bind all to_list

(* NB: this function contains prr-only bug-fix suggested in
   https://github.com/ocsigen/js_of_ocaml/issues/1589#issuecomment-2014892340
   which is not (yet) in brr yet *)
let tick ~ms =
  fut @@ Jv.Promise.create @@ fun res _rej ->
  ignore (Jv.apply (Jv.get Jv.global "setTimeout") Jv.[| callback ~arity:1 res; of_int ms |])

(* Converting with JavaScript promises *)

type nonrec ('a, 'b) result = ('a, 'b) result t
type 'a or_error = ('a, Jv.Error.t) result

let ok v = return (Ok v)
let error e = return (Error e)

let of_promise' ~ok ~error p =
  let ok v = Jv.Promise.resolve (Ok (ok v)) in
  let error e = Jv.Promise.resolve (Error (error e)) in
  fut @@ Jv.Promise.then' p ok error

let to_promise' ~ok ~error f =
  Jv.Promise.create @@ fun res rej ->
  await f @@ function
  | Ok v -> res (ok v)
  | Error e -> rej (error e)

let of_promise ~ok v = of_promise' ~ok ~error:Jv.to_error v
let to_promise ~ok v = to_promise' ~ok ~error:Jv.of_error v

(* Future syntaxes *)

module Syntax = struct
  let ( let* ) = bind
  let ( and* ) = pair
  let ( let+ ) f fn = map fn f
  let ( and+ ) = ( and* )
end

module Result_syntax = struct
  let result_pair r0 r1 = match r0, r1 with
  | (Error _ as r), _ | _, (Error _ as r) -> r
  | Ok v0, Ok v1 -> Ok (v0, v1)

  let ( let* ) f fn = bind f @@ function
  | Ok v -> fn v
  | Error _ as e -> return e

  let ( and* ) f0 f1 = map result_pair (pair f0 f1)
  let ( let+ ) f fn = map (Result.map fn) f
  let ( and+ ) = ( and* )
end

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
