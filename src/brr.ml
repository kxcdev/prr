(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------
   Adopted as  github:kxcdev/prr  from  github:dbuenzli/brr
   Copyright (c) 2022 KXC Members and Contributors. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

[@@@warning "-27-32-33-34"]

(* Data containers and encodings *)

module Tarray = struct

  (* Array buffers *)

  module Buffer = struct
    type t = Jv.t
    include (Jv.Id : Jv.CONV with type t := t)

    let array_buffer = Jv.get Jv.global "ArrayBuffer"
    let create n = Jv.new' array_buffer Jv.[| of_int n |]
    let byte_length a = Jv.Int.get a "byteLength"
    let slice ?(start = 0) ?stop  a =
      let stop = match stop with None -> byte_length a | Some stop -> stop in
      Jv.call a "slice" Jv.[| of_int start; of_int stop |]
  end

  (* Common to typed arrays and DataViews *)

  let buffer o = Buffer.of_jv @@ Jv.get o "buffer"
  let byte_offset o = Jv.Int.get o "byteOffset"
  let byte_length o = Jv.Int.get o "byteLength"

  (* Byte-level data access *)

  module Data_view = struct
    type t = Jv.t (* DataView object *)
    include (Jv.Id : Jv.CONV with type t := t)

    let dataview = Jv.get Jv.global "DataView"
    let of_buffer ?(byte_offset = 0) ?byte_length b =
      let byte_length = match byte_length with
      | None -> Buffer.byte_length b
      | Some l -> l
      in
      Jv.new' dataview
        Jv.[|Buffer.to_jv b; of_int byte_offset; of_int byte_length|]

    let buffer = buffer
    let byte_offset = byte_offset
    let byte_length = byte_length

    let get_int8 b i =
      Jv.to_int @@ Jv.call b "getInt8" Jv.[| of_int i |]

    let get_int16_be b i =
      Jv.to_int @@ Jv.call b "getInt16" Jv.[| of_int i |]

    let get_int16_le b i =
      Jv.to_int @@ Jv.call b "getInt16" Jv.[| of_int i; Jv.true' |]

    let get_int32_be b i =
      Obj.magic @@ Jv.call b "getInt32" Jv.[| of_int i |]

    let get_int32_le b i =
      Obj.magic @@ Jv.call b "getInt32" Jv.[| of_int i; Jv.true' |]

    let get_uint8 b i =
      Jv.to_int @@ Jv.call b "getUint8" Jv.[| of_int i |]

    let get_uint16_be b i =
      Jv.to_int @@ Jv.call b "getUint16" Jv.[| of_int i |]

    let get_uint16_le b i =
      Jv.to_int @@ Jv.call b "getUint16" Jv.[| of_int i; Jv.true' |]

    let get_uint32_be b i =
      Obj.magic @@ Jv.call b "getUint32" Jv.[| of_int i |]

    let get_uint32_le b i =
      Obj.magic @@ Jv.call b "getUint32" Jv.[| of_int i; Jv.true' |]

    let get_float32_be b i =
      Jv.to_float @@ Jv.call b "getFloat32" Jv.[| of_int i |]

    let get_float32_le b i =
      Jv.to_float @@ Jv.call b "getFloat32" Jv.[| of_int i; Jv.true' |]

    let get_float64_be b i =
      Jv.to_float @@ Jv.call b "getFloat64" Jv.[| of_int i |]

    let get_float64_le b i =
      Jv.to_float @@ Jv.call b "getFloat64" Jv.[| of_int i; Jv.true' |]

    let set_int8 b i v =
      ignore @@ Jv.call b "setInt8" Jv.[| of_int i; of_int v |]

    let set_int16_be b i v =
      ignore @@ Jv.call b "setInt16" Jv.[| of_int i; of_int v |]

    let set_int16_le b i v =
      ignore @@ Jv.call b "setInt16" Jv.[| of_int i; of_int v; Jv.true' |]

    let set_int32_be b i v =
      ignore @@ Jv.call b "setInt32" Jv.[| of_int i; Obj.magic v |]

    let set_int32_le b i v =
      ignore @@ Jv.call b "setInt32" Jv.[| of_int i; Obj.magic v; Jv.true' |]

    let set_uint8 b i v =
      ignore @@ Jv.call b "setUint8" Jv.[| of_int i; of_int v |]

    let set_uint16_be b i v =
      ignore @@ Jv.call b "setUint16" Jv.[| of_int i; of_int v |]

    let set_uint16_le b i v =
      ignore @@ Jv.call b "setUint16" Jv.[| of_int i; of_int v; Jv.true' |]

    let set_uint32_be b i v =
      ignore @@ Jv.call b "setUint32" Jv.[| of_int i; Obj.magic v |]

    let set_uint32_le b i v =
      ignore @@ Jv.call b "setUint32" Jv.[| of_int i; Obj.magic v; Jv.true' |]

    let set_float32_be b i v =
      ignore @@ Jv.call b "setFloat32" Jv.[| of_int i; of_float v |]

    let set_float32_le b i v =
      ignore @@ Jv.call b "setFloat32" Jv.[| of_int i; of_float v; Jv.true' |]

    let set_float64_be b i v =
      ignore @@ Jv.call b "setFloat64" Jv.[| of_int i; of_float v |]

    let set_float64_le b i v =
      ignore @@ Jv.call b "setFloat64" Jv.[| of_int i; of_float v; Jv.true' |]
  end

  (* Array types *)

  type ('a, 'b) type' =
  | Int8 : (int, Bigarray.int8_signed_elt) type'
  | Int16 : (int, Bigarray.int16_signed_elt) type'
  | Int32 : (int32, Bigarray.int32_elt) type'
  | Uint8 : (int, Bigarray.int8_unsigned_elt) type'
  | Uint8_clamped : (int, Bigarray.int8_unsigned_elt) type'
  | Uint16 : (int, Bigarray.int16_unsigned_elt) type'
  | Uint32 : (int32, Bigarray.int32_elt) type'
  | Float32 : (float, Bigarray.float32_elt) type'
  | Float64 : (float, Bigarray.float64_elt) type'

  let type_size_in_bytes : type a b. (a, b) type' -> int = function
  | Int8 | Uint8 | Uint8_clamped -> 1 | Int16 | Uint16 -> 2
  | Int32 | Uint32 | Float32 -> 4 | Float64  -> 8

  (* Typed arrays *)

  type ('a, 'b) t = Jv.t
  external to_jv : ('a, 'b) t -> Jv.t = "%identity"
  external of_jv : Jv.t -> ('a, 'b) t = "%identity"

  let cons_of_type : type a b. (a, b) type' -> Jv.t = function
  | Int8 -> Jv.get Jv.global "Int8Array"
  | Int16 -> Jv.get Jv.global "Int16Array"
  | Int32 -> Jv.get Jv.global "Int32Array"
  | Uint8 -> Jv.get Jv.global "Uint8Array"
  | Uint8_clamped -> Jv.get Jv.global "Uint8ClampedArray"
  | Uint16 -> Jv.get Jv.global "Uint16Array"
  | Uint32 -> Jv.get Jv.global "Uint32Array"
  | Float32 -> Jv.get Jv.global "Float32Array"
  | Float64 -> Jv.get Jv.global "Float64Array"

  let create t n = Jv.new' (cons_of_type t) Jv.[| of_int n |]
  let of_buffer t ?(byte_offset = 0) ?length b =
    let args = match length with
    | None -> Jv.[| b; of_int byte_offset |]
    | Some l -> Jv.[| b; of_int byte_offset; of_int l |]
    in
    Jv.new' (cons_of_type t) args

  let length a = Jv.Int.get a "length"
  let type' : type a b. (a, b) t -> (a, b) type' = fun a ->
    let m = Obj.magic in
    match Jstr.to_string (Jv.Jstr.get (Jv.get a "constructor") "name") with
    | "Int8Array" -> m Int8
    | "Int16Array" -> m Int16
    | "Int32Array" -> m Int32
    | "Uint8Array" -> m Uint8
    | "Uint8ClampedArray" -> m Uint8_clamped
    | "Uint16Array" -> m Uint16
    | "Uint32Array" -> m Uint32
    | "Float32Array" -> m Float32
    | "Float64Array" -> m Float64
    | s ->
        let t = Jstr.of_string s in
        Jv.throw (Jstr.append (Jstr.v "Unknown typed array: ") t)

  (* Setting, copying and slicing *)

  external get : ('a, 'b) t -> int -> 'a = "caml_js_get"
  external set : ('a, 'b) t -> int -> 'a -> unit = "caml_js_set"
  let set_tarray a ~dst b = ignore @@ Jv.call a "set" Jv.[| b; of_int dst |]

  let fill ?(start = 0) ?stop v a =
    let stop = match stop with None -> length a | Some stop -> stop in
    ignore @@ Jv.call a "fill" Jv.[| Jv.repr v; of_int start; of_int stop |]

  let copy_within ?(start = 0) ?stop ~dst a =
    let stop = match stop with None -> length a | Some stop -> stop in
    ignore @@
    Jv.call a "copyWithin" Jv.[| of_int dst; of_int start; of_int stop |]

  let slice ?(start = 0) ?stop  a =
    let stop = match stop with None -> byte_length a | Some stop -> stop in
    Jv.call a "slice" Jv.[| of_int start; of_int stop |]

  let sub ?(start = 0) ?stop  a =
    let stop = match stop with None -> byte_length a | Some stop -> stop in
    Jv.call a "subArray" Jv.[| of_int start; of_int stop |]

  (* Predicates *)

  let find sat a =
    let sat v i = Jv.of_bool (sat i v) in
    Jv.to_option Obj.magic (Jv.call a "find" Jv.[| callback ~arity:2 sat |])

  let find_index sat a =
    let sat v i = Jv.of_bool (sat i v) in
    let i = Jv.to_int (Jv.call a "findIndex" Jv.[| callback ~arity:2 sat |]) in
    if i = -1 then None else Some i

  let for_all sat a =
    let sat v i = Jv.of_bool (sat i v) in
    Jv.to_bool @@ Jv.call a "every" Jv.[| callback ~arity:2 sat |]

  let exists sat a =
    let sat v i = Jv.of_bool (sat i v) in
    Jv.to_bool @@ Jv.call a "every" Jv.[| callback ~arity:2 sat |]

  (* Traversals *)

  let filter sat a =
    let sat v i = Jv.of_bool (sat i v) in
    Jv.call a "filter" Jv.[| callback ~arity:2 sat |]

  let iter f a =
    let f v i = f i v in
    ignore @@ Jv.call a "forEach" Jv.[| callback ~arity:2 f |]

  let map f a = Jv.call a "map" Jv.[| callback ~arity:1 f |]

  let fold_left f acc a =
    Obj.magic @@ Jv.call a "reduce" [|Jv.callback ~arity:2 f; Jv.repr acc|]

  let fold_right f a acc =
    let f acc v = f v acc in
    Obj.magic @@ Jv.call a "reduceRight" [|Jv.callback ~arity:2 f; Jv.repr acc|]

  let reverse a = Jv.call a "reverse" Jv.[||]

  (* Type aliases *)

  type int8 = (int, Bigarray.int8_signed_elt) t
  type int16 = (int, Bigarray.int16_signed_elt) t
  type int32 = (int32, Bigarray.int32_elt) t
  type uint8 = (int, Bigarray.int8_unsigned_elt) t
  type uint8_clamped = (int, Bigarray.int8_unsigned_elt) t
  type uint16 = (int, Bigarray.int16_unsigned_elt) t
  type uint32 = (int32, Bigarray.int32_elt) t
  type float32 = (float, Bigarray.float32_elt) t
  type float64 = (float, Bigarray.float64_elt) t

  (* Converting *)

  let of_tarray t a = Jv.new' (cons_of_type t) [| a |]
  let of_int_array t a = Jv.new' (cons_of_type t) Jv.[| of_array Jv.of_int a |]
  let of_float_array t a = Jv.new' (cons_of_type t) Jv.[| of_array of_float a|]
  let to_int_jstr ?(sep = Jstr.sp) b =
    Jv.to_jstr @@ Jv.call b "join" Jv.[| of_jstr sep |]

  let to_hex_jstr ?(sep = Jstr.empty) a =
    let hex = Jstr.v "0123456789abcdef" in
    let d = Data_view.of_buffer (buffer a) in
    let s = ref Jstr.empty in
    for i = 0 to Data_view.byte_length d - 1 do
      let b = Data_view.get_uint8 d i in
      let sep = if i = 0 then Jstr.empty else sep in
      s := Jstr.(!s + sep + get_jstr hex (b lsr 4) + get_jstr hex (b land 0xF))
    done;
    !s

  let uint8_of_buffer b = of_buffer Uint8 b

  external to_string : uint8 -> string = "caml_string_of_array"

  let of_jstr s =
    let enc = Jv.new' (Jv.get Jv.global "TextEncoder") [||] in
    Jv.call enc "encode" [| Jv.of_jstr s |]

  let to_jstr a =
    let args = [| Jv.of_string "utf-8"; Jv.obj [| "fatal", Jv.true' |]|] in
    let dec = Jv.new' (Jv.get Jv.global "TextDecoder") args in
    match Jv.call dec "decode" [| a |] with
    | exception Jv.Error e -> Error e | s -> Ok (Jv.to_jstr s)

  let of_binary_jstr s =
    let code s i =
      let c = Jv.to_int @@ Jv.call (Jv.of_jstr s) "charCodeAt" [|Jv.of_int i|]in
      if c <= 255 then c else
      Jv.throw Jstr.(of_int i + v ": char code " + of_int c + v "exceeds 255")
    in
    try
      let b = Buffer.create (Jstr.length s) in
      let d = Data_view.of_buffer b in
      for i = 0 to Jstr.length s - 1 do Data_view.set_int8 d i (code s i) done;
      Ok (of_buffer Int8 b)
    with
    | Jv.Error e -> Error e

  let to_binary_jstr a =
    let chr b =
      Jv.to_jstr @@
      Jv.call (Jv.get Jv.global "String") "fromCharCode" [| Jv.of_int b |]
    in
    let d = Data_view.of_buffer (buffer a) in
    let s = ref Jstr.empty in
    for i = 0 to Data_view.byte_length d - 1 do
      let b = Data_view.get_uint8 d i in
      s := Jstr.(!s + chr b);
    done;
    !s

  (* Bigarray *)

  let type_to_bigarray_kind : type a b. (a, b) type' -> (a, b) Bigarray.kind =
    function
    | Int8 -> Bigarray.int8_signed
    | Int16 -> Bigarray.int16_signed
    | Int32 -> Bigarray.int32
    | Uint8 -> Bigarray.int8_unsigned
    | Uint8_clamped -> Bigarray.int8_unsigned
    | Uint16 -> Bigarray.int16_unsigned
    | Uint32 -> Bigarray.int32
    | Float32 -> Bigarray.float32
    | Float64 -> Bigarray.float64

  let type_of_bigarray_kind :
    type a b. (a, b) Bigarray.kind -> (a, b) type' option =
    function
    | Bigarray.Int8_signed -> Some Int8
    | Bigarray.Int16_signed -> Some Int16
    | Bigarray.Int32 -> Some Int32
    | Bigarray.Int8_unsigned -> Some Uint8
    | Bigarray.Int16_unsigned -> Some Uint16
    | Bigarray.Float32 -> Some Float32
    | Bigarray.Float64 -> Some Float64
    | _ -> None

  external bigarray_kind : ('a, 'b) t -> ('a,'b) Bigarray.kind =
    "caml_ba_kind_of_typed_array"

  external of_bigarray1 :
    ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> ('a, 'b) t =
    "caml_ba_to_typed_array"

  external to_bigarray1 :
    ('a, 'b) t -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t =
    "caml_ba_from_typed_array"

  external of_bigarray :
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> ('a, 'b) t =
    "caml_ba_to_typed_array"
end

module Blob = struct

  (* Enumerations *)

  module Ending_type = struct
    type t = Jstr.t
    let transparent = Jstr.v "transparent"
    let native = Jstr.v "native"
  end

  (* Initialisation objects *)

  type init = Jv.t
  let init ?type' ?endings () =
    let o = Jv.obj [||] in
    Jv.Jstr.set_if_some o "type" type';
    Jv.Jstr.set_if_some o "endings" endings;
    o

  (* Blobs *)

  type t = Jv.t
  include (Jv.Id : Jv.CONV with type t := t)
  let blob = Jv.get Jv.global "Blob"
  let of_jstr ?(init = Jv.undefined) s =
    let a = Jv.Jarray.create 1 in
    Jv.Jarray.set a 0 (Jv.of_jstr s);
    Jv.new' blob [| a; init |]

  let of_jarray ?(init = Jv.undefined) a = Jv.new' blob [| a; init |]
  let of_array_buffer ?(init = Jv.undefined) b =
    Jv.new' blob [| Jv.of_jv_array [| Tarray.Buffer.to_jv b |]; init |]

  let byte_length b = Jv.Int.get b "size"
  let type' b = Jv.Jstr.get b "type"
  let slice ?(start = 0) ?stop ?(type' = Jstr.empty) b =
    let stop = match stop with None -> byte_length b | Some stop -> stop in
    Jv.call b "slice" Jv.[| of_int start; of_int stop; of_jstr type' |]

  let array_buffer b =
    Fut.of_promise ~ok:Tarray.Buffer.of_jv (Jv.call b "arrayBuffer" [||])

  let stream b = Jv.get b "stream"
  let text b = Fut.of_promise ~ok:Jv.to_jstr (Jv.call b "text" [||])
end

module Base64 = struct

  type data = Jstr.t

  let data_utf_8_of_jstr s = Tarray.to_binary_jstr (Tarray.of_jstr s)
  let data_utf_8_to_jstr d = match Tarray.of_binary_jstr d with
  | Error _ as e -> e | Ok t -> Tarray.to_jstr t

  let data_of_binary_jstr = Fun.id
  let data_to_binary_jstr = Fun.id

  let encode bs =
    match Jv.apply (Jv.get Jv.global "btoa") Jv.[| of_jstr bs |] with
    | exception Jv.Error e -> Error e
    | v -> Ok (Jv.to_jstr v)

  let decode s =
    match Jv.apply (Jv.get Jv.global "atob") Jv.[| of_jstr s |] with
    | exception Jv.Error e -> Error e
    | v -> Ok (Jv.to_jstr v)
end

module Json = struct
  type t = Jv.t
  let json = Jv.get Jv.global "JSON"
  let encode v = Jv.to_jstr (Jv.call json "stringify" [| v |])
  let decode s = match Jv.call json "parse" [|Jv.of_jstr s|] with
  | exception Jv.Error e -> Error e | v -> Ok v
end

module Uri = struct
  type t = Jv.t
  include (Jv.Id : Jv.CONV with type t := t)

  let encode = Jv.get Jv.global "encodeURI"
  let decode = Jv.get Jv.global "decodeURI"

  let url = Jv.get Jv.global "URL"

  let v ?base s = match base with
  | None -> Jv.new' url [| Jv.of_jstr s |]
  | Some b -> Jv.new' url [| Jv.of_jstr s; Jv.of_jstr b |]

  let with_uri ?scheme ?host ?port ?path ?query ?fragment u =
    let u = Jv.new' url [| u |] in
    let pct_enc v = Jv.apply encode [| Jv.of_jstr v |] in
    try
      Jv.set_if_some u "protocol" (Option.map pct_enc scheme);
      Jv.set_if_some u "hostname" (Option.map pct_enc host);
      begin match port with
      | None -> ()
      | Some p -> Jv.Jstr.set_if_some u "port" (Option.map Jstr.of_int p)
      end;
      Jv.set_if_some u "pathname" (Option.map pct_enc path);
      Jv.set_if_some u "search" (Option.map pct_enc query);
      Jv.set_if_some u "hash" (Option.map pct_enc fragment);
      Ok u
    with Jv.Error e -> Error e

  let pct_dec v = Jv.to_jstr @@ Jv.apply decode [| v |]

  let scheme u =
    let p = pct_dec (Jv.get u "protocol") in
    if Jstr.length p <> 0 then Jstr.slice p ~stop:(-1) (* remove ':' *) else p

  let host u = pct_dec (Jv.get u "hostname")
  let port u =
    let p = Jv.Jstr.get u "port" in
    if Jstr.is_empty p then None else Jstr.to_int p

  let query u =
    let q = pct_dec (Jv.get u "search") in
    if Jstr.is_empty q then q else Jstr.slice q ~start:1 (* remove '?' *)

  let path u = pct_dec (Jv.get u "pathname")
  let fragment u =
    let f = Jv.to_jstr @@ Jv.apply decode [| Jv.get u "hash" |] in
    if Jstr.is_empty f then f else Jstr.slice f ~start:1 (* remove '#' *)

  (* Params *)

  module Params = struct
    type t = Jv.t
    include (Jv.Id : Jv.CONV with type t := t)

    let usp = Jv.get Jv.global "URLSearchParams"
    let is_empty p = Jv.It.result_done (Jv.It.next (Jv.call p "entries" [||]))
    let mem k p = Jv.to_bool (Jv.call p "has" [|Jv.of_jstr k|])
    let find k p = Jv.to_option Jv.to_jstr (Jv.call p "get" [|Jv.of_jstr k|])
    let find_all k p = Jv.to_jstr_list (Jv.call p "getAll" [|Jv.of_jstr k|])
    let fold f p acc =
      let key = Jv.to_jstr in
      let value = Jv.to_jstr in
      Jv.It.fold_bindings ~key ~value f (Jv.call p "entries" [||]) acc

    let of_jstr s = Jv.new' usp [| Jv.of_jstr s|] (* No errors ? *)
    let to_jstr p = Jv.to_jstr (Jv.call p "toString" [||])
    let of_assoc l =
      let p = of_jstr Jstr.empty in
      let app p (k, v) =
        ignore (Jv.call p "append" Jv.[|of_jstr k; of_jstr v|])
      in
      List.iter (app p) l; p

    let to_assoc p = List.rev (fold (fun k v acc -> (k, v) :: acc) p [])
    let of_obj o = Jv.new' usp [| o |]
  end

  (* URI encoding *)

  let code f s = match Jv.apply f [|Jv.of_jstr s|] with
  | exception Jv.Error e -> Error e
  | v -> Ok (Jv.to_jstr v)

  let encode_component = Jv.get Jv.global "encodeURIComponent"
  let decode_component = Jv.get Jv.global "decodeURIComponent"
  let encode s = code encode s
  let decode s = code decode s
  let encode_component s = code encode_component s
  let decode_component s = code decode_component s

  (* Converting *)

  let to_jstr u = Jv.to_jstr (Jv.call u "toString" [||])
  let of_jstr ?base s = match v ?base s with
  | exception Jv.Error e -> Error e | v -> Ok v
end

module Console = struct
  type t = Jv.t
  include (Jv.Id : Jv.CONV with type t := t)
  let call c meth args = ignore (Jv.call c meth args)

  let c = ref (Jv.get Jv.global "console")
  let get () = !c
  let set n = c := n
  let clear () = call !c "clear" [||]

  (* Log functions *)

  type msg = [] : msg | ( :: ) : 'a * msg -> msg
  type 'a msgr = 'a -> msg
  let msg v = [v]
  let str v =
    let v = Jv.repr v in
    if Jv.is_null v then Jstr.v "null" else
    if Jv.is_undefined v then Jstr.v "undefined" else
    Jv.to_jstr @@ Jv.call v "toString" [||]

  let msg_to_jv_array msg =
    let rec loop a i = function
    | [] -> a
    | v :: vs -> Jv.Jarray.set a i (Jv.repr v); loop a (i + 1) vs
    in
    Jv.to_jv_array @@ loop (Jv.Jarray.create 0) 0 msg

  type log = msg -> unit

  (* Levelled logging *)

  let log msg = call !c "log" (msg_to_jv_array msg)
  let trace msg = call !c "trace" (msg_to_jv_array msg)
  let error msg = call !c "error" (msg_to_jv_array msg)
  let warn msg = call !c "warn" (msg_to_jv_array msg)
  let info msg = call !c "info" (msg_to_jv_array msg)
  let debug msg = call !c "debug" (msg_to_jv_array msg)

  (* Asserting and dumping *)

  let assert' b msg = call !c "assert" (msg_to_jv_array (Jv.of_bool b :: msg))
  let dir o = call !c "dir" [|Jv.repr o|]
  let table ?cols v =
    let msg = match cols with
    | None -> [|Jv.repr v|] | Some l -> [|Jv.repr v; Jv.of_jstr_list l|]
    in
    call !c "table" msg

  (* Grouping *)

  let group_end () = call !c "groupEnd" [||]
  let group ?(closed = false) msg = match closed with
  | false -> call !c "group" (msg_to_jv_array msg)
  | true -> call !c "groupCollapsed" (msg_to_jv_array msg)

  (* Counting *)

  let count label = call !c "count" [|Jv.of_jstr label|]
  let count_reset label = call !c "countReset" [|Jv.of_jstr label|]

  (* Timing *)

  let time label = call !c "time" [| Jv.of_jstr label |]
  let time_log label msg = call !c "timeLog" (msg_to_jv_array (label :: msg))
  let time_end label = call !c "timeEnd" [| Jv.of_jstr label |]

  (* Profiling *)

  let profile label = call !c "profile" [|Jv.repr label|]
  let profile_end label = call !c "profileEnd" [|Jv.repr label|]
  let time_stamp label = call !c "timeStamp" [| Jv.of_jstr label |]

  (* Result logging *)

  let log_result ?(ok = fun v -> [v]) ?error:(err = fun e -> [str e]) r =
    (match r with Ok v -> log (ok v) | Error e -> error (err e));
    r

  let log_if_error ?(l = error) ?(error_msg = fun e -> [str e]) ~use = function
  | Ok v -> v | Error e -> l (error_msg e); use

  let log_if_error' ?l ?error_msg ~use r =
    Ok (log_if_error ?l ?error_msg ~use r)
end

module G = struct

  (* Global objects *)

  let console = Jv.get Jv.global "console"

  (* Timers *)

  type timer_id = int

  let set_timeout ~ms f =
    Jv.to_int @@
    Jv.call Jv.global "setTimeout" [| Jv.callback ~arity:1 f; Jv.of_int ms |]

  let set_interval ~ms f =
    Jv.to_int @@
    Jv.call Jv.global "setInterval" [| Jv.callback ~arity:1 f; Jv.of_int ms |]

  let stop_timer tid =
    (* according to spec interval and timeout share the same ints *)
    ignore @@ Jv.call Jv.global "clearTimeout" [| Jv.of_int tid |]
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
  ---------------------------------------------------------------------------
   Copyright (c) 2022 The KXC Members and Contributors

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
