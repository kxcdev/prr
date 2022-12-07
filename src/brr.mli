(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

[@@@warning "-32-34"]

(** Browser APIs.

    Open this module to use it. It defines only modules in your scope. *)

(** {1:data Data containers and encodings} *)

(** Typed arrays. *)
module Tarray : sig

  (** {1:buffer Buffers} *)

  (** [ArrayBuffer] objects (byte buffers).  *)
  module Buffer : sig

    type t
    (** The type for
        {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer}[ArrayBuffer]}
      objects. They hold the bytes of typed arrays. *)

    val create : int -> t
    (** [create n] is a new buffer with [n] bytes. *)

    val byte_length : t -> int
    (** [byte_length b] is the byte length of [b]. *)

    val slice : ?start:int -> ?stop:int -> t -> t
    (** [slice ~start ~stop b] is a new buffer holding the bytes of
        [b] in range \[[start];[stop-1]\]. This is {b a copy}.
        [start] defaults to [0] and [stop] to [byte_length b].

        If [start] or [stop] are negative they are subtracted from
        [byte_length b]. This means that [-1] denotes the last byte of
        the buffer. *)

    (**/**)
    include Jv.CONV with type t := t
    (**/**)
  end

  (** [DataView objects] (byte-level typed data access on [ArrayBuffer]s).

      This module allows to read and write buffers with any data element
      at any byte offset. *)
  module Data_view : sig

    type t
    (** The type for {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView}DataView} objects. *)

    val of_buffer : ?byte_offset:int -> ?byte_length:int -> Buffer.t -> t
    (** [of_buffer ~byte_offset ~length b k] provides access to
        [byte_length] (defaults to [Buffer.byte_length b]) bytes of [b]
        starting at byte offset [byte_offset]. *)

    val buffer : t -> Buffer.t
    (** [buffer d] is the untyped buffer of [d]. *)

    val byte_offset : t -> int
    (** [byte_offset d] is the byte index where [d] starts in [buffer d]. *)

    val byte_length : t -> int
    (** [byte_length d] is the byte length of [d]. *)

    (** {1:reads Reads}

        {b Suffixes.} [_be] stands for big endian, [_le] for little endian. *)

    val get_int8 : t -> int -> int
    val get_int16_be : t -> int -> int
    val get_int16_le : t -> int -> int
    val get_int32_be : t -> int -> int32
    val get_int32_le : t -> int -> int32

    val get_uint8 : t -> int -> int
    val get_uint16_be : t -> int -> int
    val get_uint16_le : t -> int -> int
    val get_uint32_be : t -> int -> int32
    val get_uint32_le : t -> int -> int32

    val get_float32_be : t -> int -> float
    val get_float32_le : t -> int -> float
    val get_float64_be : t -> int -> float
    val get_float64_le : t -> int -> float

    (** {1:writes Writes}

        {b Suffixes.} [_be] stands for big endian, [_le] for little endian. *)

    val set_int8 : t -> int -> int -> unit
    val set_int16_be : t -> int -> int -> unit
    val set_int16_le : t -> int -> int -> unit
    val set_int32_be : t -> int -> int32 -> unit
    val set_int32_le : t -> int -> int32 -> unit

    val set_uint8 : t -> int -> int -> unit
    val set_uint16_be : t -> int -> int -> unit
    val set_uint16_le : t -> int -> int -> unit
    val set_uint32_be : t -> int -> int32 -> unit
    val set_uint32_le : t -> int -> int32 -> unit

    val set_float32_be : t -> int -> float -> unit
    val set_float32_le : t -> int -> float -> unit
    val set_float64_be : t -> int -> float -> unit
    val set_float64_le : t -> int -> float -> unit

    (**/**)
    include Jv.CONV with type t := t
    (**/**)
  end

  (** {1:types Array types} *)

  type ('a, 'b) type' =
  | Int8 : (int, Bigarray.int8_signed_elt) type'
  | Int16 : (int, Bigarray.int16_signed_elt) type'
  | Int32 : (int32, Bigarray.int32_elt) type'
  | Uint8 : (int, Bigarray.int8_unsigned_elt) type'
  | Uint8_clamped : (int, Bigarray.int8_unsigned_elt) type'
  | Uint16 : (int, Bigarray.int16_unsigned_elt) type'
  | Uint32 : (int32, Bigarray.int32_elt) type'
  | Float32 : (float, Bigarray.float32_elt) type'
  | Float64 : (float, Bigarray.float64_elt) type' (** *)
  (** The type for typed array whose elements are of type ['b] and
      are accessed with type ['a]. *)

  val type_size_in_bytes : ('a, 'b) type' -> int
  (** [type_size_in_bytes t] is the number of bytes used to store
      an element of type ['b]. *)

  (** {1:typed Typed arrays}

      {b Note.} In the functions below.
      {ul
      {- Indices can always be negative in which case they are subtracted
         from {!length}. This means that [-1] denotes the last element of
         the buffer.}
      {- If unspecified [start] defaults to [0].}
      {- If unspecified [stop] defaults to [length b].}} *)

  type ('a, 'b) t
  (** The type for
      {{:https://developer.mozilla.org/en-US/docs/Web/API/ArrayBufferView}
      [ArrayBufferView]} objects (typed access to [ArrayBuffer] objects)
      whose elements are of type ['b] and accessed with type ['a]. See
      the {{!type_aliases}type aliases}. *)

  val create : ('a, 'b) type' -> int -> ('a, 'b) t
  (** [create n t] is an array of type [t] with [n] elements of type ['b]
      initialised to their zero. See also {{!converting} converting}. *)

  val of_buffer :
    ('a, 'b) type' -> ?byte_offset:int -> ?length:int -> Buffer.t -> ('a, 'b) t
  (** [of_buffer t ~byte_offset ~length b] is an array of type [t] with
      [length] elements of type ['b] starting at the byte offset [byte_offset]
      of [b]. [byte_offset] defaults to [0] and length so as to get to
      the end of the buffer. *)

  val buffer : ('a, 'b) t -> Buffer.t
  (** [buffer a] is the untyped buffer of [a]. *)

  val byte_offset : ('a, 'b) t -> int
  (** [byte_offset a] is the byte index where [a] starts in [buffer a]. *)

  val byte_length : ('a, 'b) t -> int
  (** [byte_length a] is the byte length of [a]. *)

  val length : ('a, 'b) t -> int
  (** [length a] are the number of elements in [a]. *)

  val type' : ('a, 'b) t -> ('a, 'b) type'
  (** [type' a] is the type of [a]. *)

  (** {1:set Setting, copying and slicing} *)

  external get : ('a, 'b) t -> int -> 'a = "caml_js_get"
  (** [get a i] is the element of [a] at [i]. *)

  external set : ('a, 'b) t -> int -> 'a -> unit = "caml_js_set"
  (** [set a i v] sets the element of [a] at [i] to [v]. *)

  val set_tarray : ('a, 'b) t -> dst:int -> ('c, 'd) t -> unit
  (** [set_tarray a ~dst b] sets the values of [a] starting
      at index [dst] with those of [b] which are converted to match
      the type of [a] ({{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/set}unclear} how exactly). *)

  val fill : ?start:int -> ?stop:int -> 'a -> ('a, 'b) t -> unit
  (** [fill ~start ~stop v a] sets the elements in range [[start];[stop-1]]
      to [v]. *)

  val copy_within : ?start:int -> ?stop:int -> dst:int -> ('a, 'b) t -> unit
  (** [copy_within ~start ~stop ~dst a] copies at at [dst] the elements in
      range [[start];[stop-1]]. *)

  val slice : ?start:int -> ?stop:int -> ('a, 'b) t -> ('a, 'b) t
  (** [slice ~start ~stop a] is a new array holding a copy of the
      bytes of [a] in range \[[start];[stop-1]\]. This is {b a copy},
      use {!sub} to share the data. *)

  val sub : ?start:int -> ?stop:int -> ('a, 'b) t -> ('a, 'b) t
  (** [sub ~start ~stop a] is an array that spans the bytes of [b] in
      range \[[start];[stop-1]\]. This is {b not a copy}, use {!slice}
      to make a copy. *)

  (** {1:predicates Predicates} *)

  val find : (int -> 'a -> bool) -> ('a, 'b) t -> 'a option
  (** [find sat a] is the first index a.[i] for which [sat i a.[i]] is true. *)

  val find_index : (int -> 'a -> bool) -> ('a, 'b) t -> int option
  (** [find sat a] is the first index i for which [sat i a.[i]] is true. *)

  val for_all : (int -> 'a -> bool) -> ('a, 'b) t -> bool
  (** [for_all sat a] is [true] iff all elements [a.[i]] of [b] satisfy
      [sat i a.[i]]. *)

  val exists : (int -> 'a -> bool) -> ('a, 'b) t -> bool
  (** [exists sat a] is [true] iff one elements [a.[i]] of [b] satisfies
      [sat i a.[i]]. *)

  (** {1:traversal Traversals} *)

  val filter : (int -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t
  (** [filter sat a] is an array with the elements [a.[i]] of [a] for which
      [sat i a.[i]] is [true]. *)

  val iter : (int -> 'a -> unit) -> ('a, 'b) t -> unit
  (** [iter f a] calls [f i a.[i]] on each element of [a]. *)

  val map : ('a -> 'a) -> ('a, 'b) t -> ('a, 'b) t
  (** [map f a] is a new typed array with elements of [a] mapped by [f]. *)

  val fold_left : ('c -> 'a -> 'c) -> 'c -> ('a, 'b) t -> 'c
  (** [fold_left f acc a] folds [f] over the elements of [a] starting with
      [acc]. *)

  val fold_right : ('a -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** [fold_right f acc a] folds [f] over the elements of [a] starting with
      [acc]. *)

  val reverse : ('a, 'b) t -> ('a, 'b) t
  (** [reverse a] is a new array with [a]'s elements reversed. *)

  (** {1:type_aliases Type aliases}

      Use these in interfaces. *)

  type int8 = (int, Bigarray.int8_signed_elt) t
  type int16 = (int, Bigarray.int16_signed_elt) t
  type int32 = (Int32.t, Bigarray.int32_elt) t
  type uint8 = (int, Bigarray.int8_unsigned_elt) t
  type uint8_clamped = (int, Bigarray.int8_unsigned_elt) t
  type uint16 = (int, Bigarray.int16_unsigned_elt) t
  type uint32 = (Int32.t, Bigarray.int32_elt) t
  type float32 = (float, Bigarray.float32_elt) t
  type float64 = (float, Bigarray.float64_elt) t

  (** {1:converting Converting} *)

  val of_tarray : ('c, 'd) type' -> ('a, 'b) t -> ('c, 'd) t
  (** [of_tarray t a] is an array of type [t] with the elements of
      [a] converted accordingly ({{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Constructor}unclear} how
      exactly). *)

  val uint8_of_buffer : Buffer.t -> uint8
  (** [uint8_of_buffer b] wraps [b] as an Uint8 typed array. *)

  val of_int_array : ('a, 'b) type' -> int array -> ('a, 'b) t
  (** [of_int_array t arr] is an array of type [t] whose elements
      are the values of [arr], values exceeding the range for the type
      are taken modulo the range bounds (except for [Uint8_clamped]). *)

  val of_float_array : ('a, 'b) type' -> float array -> ('a, 'b) t
  (** [of_int_array t arr] is an array of type [t] whose elements
      are the values of [arr], values exceeding the range for the type
      are taken modulo the range bounds (except for [Uint8_clamped]). *)

  (** {2:string With strings} *)

  val of_jstr : Jstr.t -> uint8
  (** [of_jstr s] is an unsigned byte array with [s] as UTF-8 encoded data. *)

  val to_jstr : uint8 -> (Jstr.t, Jv.Error.t) result
  (** [to_jstr a] is the UTF-8 encoded data [a] as a string. Errors
      if [a] holds invalid UTF-8. *)

  val of_binary_jstr : Jstr.t -> (uint8, Jv.Error.t) result
  (** [of_binary_jstr s] is an unsigned byte array with the bytes of the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/DOMString/Binary}
      JavaScript binary string} [s]. Errors if a code unit of [s] is greater
      than [255]. *)

  val to_binary_jstr : uint8 -> Jstr.t
  (** [to_binary_jstr a] is a
      {{:https://developer.mozilla.org/en-US/docs/Web/API/DOMString/Binary}
      JavaScript binary string} with the unsigned bytes of [a]. *)

  val to_int_jstr : ?sep:Jstr.t -> ('a, 'b) t -> Jstr.t
  (** [to_int_jstr ~sep a] is a string with the elements of [a] printed and
      separated by [sep] (defaults to {!Jstr.sp}). *)

  val to_hex_jstr : ?sep:Jstr.t -> ('a, 'b) t -> Jstr.t
  (** [to_hex_jstr ?sep a] is a string with the bytes of [a] printed in
      lowercase hex and separated by [sep] (defaults to {!Jstr.empty}). *)

  external to_string :  uint8 -> string = "caml_string_of_array"
  (** [to_string a] is an OCaml {e byte} string from the byte array. *)

  (** {1:bigarrays As bigarrays} *)

  val type_to_bigarray_kind : ('a, 'b) type' -> ('a, 'b) Bigarray.kind
  (** [type_to_bigarray_kind t] is [t] as a bigarray kind. [Uint32] is
      mapped on {!Bigarray.int32}. *)

  val type_of_bigarray_kind : ('a, 'b) Bigarray.kind -> ('a, 'b) type' option
  (** [type_of_bigarray_kind k] is [k] as a type array type or [None] if
      there is no corresponding one. *)

  external bigarray_kind : ('a, 'b) t -> ('a,'b) Bigarray.kind =
    "caml_ba_kind_of_typed_array"
  (** [bigarray_kind a] is the bigarray kind of [a]. *)

  external of_bigarray1 :
    ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> ('a, 'b) t
    = "caml_ba_to_typed_array"
  (** [of_bigarray1 b] is a typed array with the data of bigarray
      [b]. The data buffer is shared. *)

  external to_bigarray1 :
    ('a, 'b) t -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
    = "caml_ba_from_typed_array"
  (** [to_bigarray b] is a bigarray with the data of bigarray [b]. The
      data buffer is shared. *)

  external of_bigarray :
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> ('a, 'b) t
    = "caml_ba_to_typed_array"
  (** [of_bigarray b] is a typed array with the data of bigarray
      [b]. The data buffer is shared. {b XXX.} How is the data laid
      out ? *)

  (**/**)
  external to_jv : ('a, 'b) t -> Jv.t = "%identity"
  external of_jv : Jv.t -> ('a, 'b) t = "%identity"
  (**/**)
end

(** Blob objects.

    See the {{:https://w3c.github.io/FileAPI/#blob-section}Blob Interface}. *)
module Blob : sig

  (** {1:enums Enumerations} *)

  (** The line ending type enum. *)
  module Ending_type : sig
    type t = Jstr.t
    (** The type for line endings.
        {{:https://w3c.github.io/FileAPI/#dom-blobpropertybag-endings}
        [EndingType]} values. *)

    val transparent : Jstr.t
    val native : Jstr.t
  end

  (** {1:blobs Blobs} *)

  type init
  (** The type for blob initialisation objects. *)

  val init : ?type':Jstr.t -> ?endings:Ending_type.t -> unit -> init
  (** [init ()] is a blob initialisation object with given
      {{:https://w3c.github.io/FileAPI/#ref-for-dfn-BlobPropertyBag%E2%91%A0}
      properties}. *)

  type t
  (** The type for
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob}[Blob]}
      objects. *)

  val of_jstr : ?init:init -> Jstr.t -> t
  (** [of_jstr ~init s] is a blob containing the UTF-8 encoded data of [s]. *)

  val of_array_buffer : ?init:init -> Tarray.Buffer.t -> t
  (** [of_array_buffer ~init b] is a blob containing the bytes of [b]. *)

  val byte_length : t -> int
  (** [byte_length b] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/size}byte
      length} of the blob. *)

  val type' : t -> Jstr.t
  (** [type' b] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/type}MIME type}
      of [b] or {!Jstr.empty} if unknown. *)

  val slice : ?start:int -> ?stop:int -> ?type':Jstr.t -> t -> t
  (** [slice ~start ~stop ~type b] are the bytes in
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/slice}range}
      \[[start];[stop-1]\]
      as blob. [start] defaults to [0] and [stop] to [byte_length b].

      If [start] or [stop] are negative they are subtracted from
      [byte_length b]. This means that [-1] denotes the last byte of the
      blob.

      [type'] specifies the resulting type for the blob, defaults to
      the empty string. *)

  val array_buffer : t -> Tarray.Buffer.t Fut.or_error
  (** [array_buffer b] is an
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/arrayBuffer}
      array buffer} with the contents of [b]. *)

  val stream : t -> Jv.t
  (** [stream b] is a
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/stream}stream}
      to read the contents of [b]. *)

  val text : t -> Jstr.t Fut.or_error
  (** [text b] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/text}string}
      that results from UTF-8 decoding the contents of [b]. *)

  (**/**)
  include Jv.CONV with type t := t
  (**/**)
end

(** [base64] codec.

    As performed by {{:https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/btoa}[btoa]} and
{{:https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/atob}[atob]} functions.

    {b Warning.} These functions are slightly broken API-wise. They
    are easy to use incorrectly and involve a lot of data copies to
    use them correctly. Use only for quick hacks. The detour via the
    {!Base64.type-data} type is provided to hopefully prevent people
    from shooting themselves in the foot. *)
module Base64 : sig

  (** {1:data Binary data} *)

  type data
  (** The type for representing binary data to codec. *)

  val data_utf_8_of_jstr : Jstr.t -> data
  (** [data_utf_8_of_jstr s] is the UTF-16 encoded JavaScript string
      [s] as UTF-8 binary data. This is to be used with {!encode}
      which results in a [base64] encoding of the UTF-8 representation
      of [s]. *)

  val data_utf_8_to_jstr : data -> (Jstr.t, Jv.Error.t) result
  (** [data_utf_8_to_jstr d] decodes the UTF-8 binary data [d] to an UTF-16
      encoded JavaScript string. *)

  val data_of_binary_jstr : Jstr.t -> data
  (** [data_of_binary_jstr d] is the binary data represented
      by the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/DOMString/Binary}
      JavaScript binary string} [d]. Note that this does not check that
      [d] is a binary string, {!encode} will error if that's not the case.
      Use {!Tarray.to_binary_jstr} to convert typed arrays to binary
      strings. *)

  val data_to_binary_jstr : data -> Jstr.t
  (** [data_to_jstr d] is a
      {{:https://developer.mozilla.org/en-US/docs/Web/API/DOMString/Binary}
      JavaScript binary string} from [d]. Use {!Tarray.of_binary_jstr} to
      convert binary strings to typed arrays. *)

  (** {1:codec Codec} *)

  val encode : data -> (Jstr.t, Jv.Error.t) result
  (** [encode d] encodes the binary data [d] to [base64]. This errors if
      [d] was constructed with {!data_of_binary_jstr} from an invalid
      {{:https://developer.mozilla.org/en-US/docs/Web/API/DOMString/Binary}
      JavaScript binary string}. *)

  val decode : Jstr.t -> (data, Jv.Error.t) result
  (** [decode s] decodes the [base64] encoded string [s] to
      a {{:https://developer.mozilla.org/en-US/docs/Web/API/DOMString/Binary}
      binary string}. Errors if [s] is not only made of US-ASCII characters or
      is not well formed Base64. *)
end

(** JSON codec.

    As codec by the
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON}JSON object}.

    {b Warning.} This interface will change in the future. *)
module Json : sig

  type t = Jv.t
  (** The type for JSON values.
      {b FIXME} have something more abstract. *)

  val encode : t -> Jstr.t
  (** [encode v] encodes [v] to JSON using
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify}JSON.stringify}.

      {b Warning.} Do not expect an [encode] on a {!Jv.repr} of an OCaml
      value to be decoded back by [decoded]. *)


  val decode : Jstr.t -> (t, Jv.Error.t) result
  (** [decode s] decodes the JSON text [s] into a JavaScript value using
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse}JSON.parse}. *)
end

(** URIs and URI parameters.

    {!Uri.t} values are
    {{:https://developer.mozilla.org/en-US/docs/Web/API/URL}URL}
    objects but we tweak the API to return data according to
    {{:http://tools.ietf.org/html/rfc3986}RFC 3986} terminology:
    we don't return separators like [':'], ['?']  and ['#'] in
    the data and we use [host] for what is [hostname] in the URL API.
    Also the data we return is {{!Brr.Uri.decode}URL (percent) decoded}). *)
module Uri : sig

  (** {1:uris URIs} *)

  type t
  (** The type for {{:https://developer.mozilla.org/en-US/docs/Web/API/URL}
      [URL]} objects. *)

  val v : ?base:Jstr.t -> Jstr.t -> t
  (** [v ?base s] is an URI from [s] relative to [base] (if specified).
      Raises in in case of error, use {!of_jstr} if you need to deal
      with user input. *)

  val scheme : t -> Jstr.t
  (** [scheme u] is the scheme of [u]. *)

  val host : t -> Jstr.t
  (** [host u] is the host of [u]. {b Warning} this is what
      the URL API calls [hostname]. *)

  val port : t -> int option
  (** [port u] is the port of [u]. *)

  val path : t -> Jstr.t
  (** [path u] is the path of [u]. Note that this ["/"] if there is no
      path. *)

  val query : t -> Jstr.t
  (** [query u] is the query of [u] (without the leading ['?']). *)

  val fragment : t -> Jstr.t
  (** [fragment u] is fragment of [u] (withouth the leading ['#']). *)

  val with_uri :
    ?scheme:Jstr.t -> ?host:Jstr.t -> ?port:int option -> ?path:Jstr.t ->
    ?query:Jstr.t -> ?fragment:Jstr.t -> t -> (t, Jv.Error.t) result
  (** [with_uri u] is [u] with the specified components updated.
      The given parameters are {{!Brr.Uri.encode}URL (percent) encoded}
      by the function. *)

  (** {1:params Fragment or query parameters} *)

  (** URI fragment or query parameters.

      {!Params.t} values represent key-value parameters stored in
      strings as ["k0=v0&k1=v1..."]. They can be constructed from
      any {!Jstr.t}. In particular it means they can be used with an
      URI {!fragment} or {!query}. *)
  module Params : sig

    (** {1:params Parameters} *)

    type t
    (** The type for
        {{:https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams}
        [URLSearchParams]} objects. *)

    val is_empty : t -> bool
    (** [is_empty ps] is [true] if [ps] has no key value bindings. *)

    val mem : Jstr.t -> t -> bool
    (** [mem k ps] is [true] if key [k] is bound in [ps]. *)

    val find : Jstr.t -> t -> Jstr.t option
    (** [find k ps] is the value of the first binding of [k] in [ps]. *)

    val find_all : Jstr.t -> t -> Jstr.t list
    (** [find_all k ps] are the values of all bindings of [k] in [ps]. *)

    val fold : (Jstr.t -> Jstr.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f ps acc] folds {e all} the key value bindings. *)

    (** {1:conver Converting} *)

    val of_obj : Jv.t -> t
    (** [of_obj o] uses the keys of object [o] to define URL parameters. *)

    val of_jstr : Jstr.t -> t
    (** [of_jstr s] URL decodes and parses parameters from [s]. *)

    val to_jstr : t -> Jstr.t
    (** [to_jstr ps] URL encodes the parameters [ps] to a string. *)

    val of_assoc : (Jstr.t * Jstr.t) list -> t
    (** [of_assoc assoc] are parameters for the assoc [assoc]. *)

    val to_assoc : t -> (Jstr.t * Jstr.t) list
    (** [to_assoc ps] is [ps] as an assoc list. *)

    (**/**)
    include Jv.CONV with type t := t
    (**/**)
  end

  (** {1:encoding URI Encoding} *)

  val encode : Jstr.t -> (Jstr.t, Jv.Error.t) result
  (** [encode s] URL encodes [s] by percent-encoding an UTF-8 representation
      of [s]. See {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI}encodeURI}. *)

  val decode : Jstr.t -> (Jstr.t, Jv.Error.t) result
  (** [decode s] URL decodes [s] by percent-decoding an UTF-8 representation
      of [s]. See {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI}decodeURI}.
*)

  val encode_component : Jstr.t -> (Jstr.t, Jv.Error.t) result
  (** [encode s] URL encodes [s] by percent-encoding an UTF-8 representation
      of [s]. See {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent}encodeURIComponent}. *)

  val decode_component : Jstr.t -> (Jstr.t, Jv.Error.t) result
  (** [decode s] URL decodes [s] by precent-decoding an UTF-8 representation
      of [s]. See {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent}decodeURIComponent}. *)

  (** {1:conv Converting} *)

  val of_jstr : ?base:Jstr.t -> Jstr.t -> (t, Jv.Error.t) result
  (** [of_jstr ~base s] is an URL from [s] relative to [base] (if specified).
      Note that if [s] is relative and [base] is unspecified the function
      errors. *)

  val to_jstr : t -> Jstr.t
  (** [to_jstr u] is [u] as a JavaScript string. The result is URL
      encoded.*)

  (**/**)
  include Jv.CONV with type t := t
  (**/**)
end

(** Browser console.

    See {{:https://developer.mozilla.org/en-US/docs/Web/API/console}
    [Console]}. Take a few minutes to {{!Console.val-log}understand this}. *)
module Console : sig

  type t
  (** The type for
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console}[console]}
    objects. See {!G.console} for the global console object. *)

  val get : unit -> t
  (** [get ()] is the console object on which the functions below act.
      Initially this is {!G.console}. *)

  val set : t -> unit
  (** [set o] sets the console object to [o]. *)

  val clear : unit -> unit
  (** [clear ()]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/clear}clears}
      the console. *)

  (** {1:log_funs Log functions} *)

  type msg =
  | [] : msg
  | ( :: ) : 'a * msg -> msg (** *)
  (** The type for log messages. *)

  type 'a msgr = 'a -> msg
  (** The type for functions turning values of type ['a] into log
      messages. *)

  val msg : 'a msgr
  (** [msg v] is [[v]]. *)

  type log = msg -> unit
  (** The type for log functions.

      Log messages rebind OCaml's list syntax. This allows to
      write heterogeneous logging statements concisely.

{[
let () = Console.(log [1; 2.; true; Jv.true'; str "🐫"; G.navigator])
]}

      The console logs JavaScript values. For OCaml values this means
      that their [js_of_ocaml] representation is logged; see the
      {{!page-ffi_manual}FFI manual} for details.  Most OCaml values
      behind [Brr] types are however direct JavaScript values and
      logging them as is will be valuable. For other values you can
      use the {!str} function which invokes the JavaScript [toString]
      method on the value. It works on OCaml strings and is mostly
      equivalent and shorter than calling {!Jstr.v} before logging
      them.

      In the JavaScript [console] API, if the first argument is a JavaScript
      string it can have
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console#Using_string_substitutions}formatting
      specifications}.  Just remember this should be a JavaScript string, so
      wrap OCaml literals by {!str} or {!Jstr.v}:
{[
let () = Console.(log [str "This is:\n%o\n the navigator"; G.navigator])
]}
*)

  val str : 'a -> Jstr.t
  (** [str v] is the result of invoking the JavaScript [toString] method on
      the representation of [v]. If [v] is {!Jv.null} and {!Jv.undefined}
      a string representing them is directly returned. *)

  (** {1:result [Result] logging} *)

  val log_result :
    ?ok:'a msgr -> ?error:'b msgr -> ('a, 'b) result -> ('a, 'b) result

  (** [log_result ~ok ~error r] is [r] but logs [r] using {!val-log} and
      [ok] to format [Ok v] and {!error} and [error] for [Error
      e]. [ok] defaults to [[v]] and [error] to [[str e]]. *)

  val log_if_error :
    ?l:log -> ?error_msg:'b msgr -> use:'a -> ('a, 'b) result -> 'a
  (** [log_if_error ~l ~error_msg ~use r] is [v] if [r] is [Ok v]
      and [use] if [r] is [Error e]. In this case [e] is logged with [l]
      (defaults to {!error}) and [error_msg] (defaults to [str e]). *)

  val log_if_error' :
    ?l:log -> ?error_msg:'b msgr -> use:'a -> ('a, 'b) result ->
    ('a, 'b) result
  (** [log_if_error'] is {!log_if_error} wrapped by {!Result.ok}. *)

  (** {1:logging Levelled logging} *)

  val log : log
  (** [log m]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/log}logs}
      [m] with no specific level. *)

  val trace : log
  (** [trace m] logs [m] with no specific level but with a
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/trace}
      stack trace}, like {!error} and {!warn} do. *)

  val error : log
  (** [error m] logs [m] with level
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/error}
      error}. *)

  val warn : log
  (** [warn m] logs [m] with level
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/warn}
      warn}. *)

  val info : log
  (** [warn m] logs [m] with level
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/info}
      info}. *)

  val debug : log
  (** [debug m] logs [m] with level
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/debug}
      debug}. *)

  (** {1:assert_dump Asserting and dumping} *)

  val assert' : bool -> log
  (** [assert' c m]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/assert}
      asserts} [c] and logs [m] with a stack trace iff [c] is [false]. *)

  val dir : 'a -> unit
  (** [dir o] logs a
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/dir}
      listing} of the properties of the object [o] –
      {{:https://stackoverflow.com/a/11954537}this} explains
      the difference with [Console.(log [o])]. *)

  val table : ?cols:Jstr.t list -> 'a -> unit
  (** [table v] outputs [v] as
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/table}
      tabular data}. If [cols] is specified only the specified
      properties are printed. *)

  (** {1:grouping Grouping} *)

  val group : ?closed:bool -> log
  (** [group ~closed msg] logs [msg] and pushes a new inline
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/group}group}
      in the console. This indents messages until {!group_end} is called. If
      [closed] is [true] (defaults to [false]) the group's content is hidden
      behind a {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/groupCollapsed}disclosure button}. *)

  val group_end : unit -> unit
  (** [group_end ()]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/groupEnd}
      pops} the last inline group. *)

  (** {1:count Counting} *)

  val count : Jstr.t -> unit
  (** [count label] logs [label] with the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/count}
      number of times} [count label] was called. *)

  val count_reset : Jstr.t -> unit
  (** [count_reset label]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/countReset}
      resets} the counter for [count label]. *)

  (** {1:timing Timing} *)

  val time : Jstr.t -> unit
  (** [time label]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/time}starts}
      a timer named [label]. *)

  val time_log : Jstr.t -> log
  (** [time_log label msg]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/timeLog}
      reports} the timer value of [label] with [msg] appended to the
      report. *)

  val time_end : Jstr.t -> unit
  (** [time_end label]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/timeEnd}
      ends} the timer named [label]. *)

  (** {1:profiling Profiling} *)

  val profile : Jstr.t -> unit
  (** [profile label]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/profile}
      starts} a new profile labelled [label]. *)

  val profile_end : Jstr.t -> unit
  (** [profile_end label]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/profileEnd}
      ends} ends the new profile labelled [label]. *)

  val time_stamp : Jstr.t -> unit
  (** [time_stamp label]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/timeStamp}
      adds} a marker labeled by [label] in the waterfall view. *)

  (**/**)
  include Jv.CONV with type t := t
  (**/**)
end

(** The global object, its global objects and functions.

    If you are:
    {ul
    {- In Webworker context, see also {!Brr_webworkers.Worker.G}}
    {- In an audio worklet, see also {!Brr_webaudio.Audio.Worklet.G}}} *)
module G : sig

  (** {1:global_objects Global objects}

      Depending on the JavaScript environment theses values can be undefined.
      You should know what you are doing or use {!Jv.defined} to test
      the values.

      Because of type dependencies some global objects are defined
      in their dedicated modules:
      {ul
      {- {!Brr_io.Fetch.caches} is the global
      {{:https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/caches}[caches]} object.}
      {- {!Brr_webcrypto.Crypto.crypto} is the global
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Window/crypto}
      [crypto]} object.}} *)

  val console : Console.t
  (** [console] is the global
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Window/console}
      [console]} object (if available). This
      is what {!Console.get} returns initially. *)

  (** {1:timers Timers} *)

  type timer_id = int
  (** The type for timeout identifiers. *)

  val set_timeout : ms:int -> (unit -> unit) -> timer_id
  (** [set_timeout ~ms f] is a timer calling [f] in [ms] milliseconds unless
      {{!stop_timer}stopped} before. *)

  val set_interval : ms:int -> (unit -> unit) -> timer_id
  (** [set_interval ~ms f] is a timer calling [f] every [ms] milliseconds
      until it is {{!stop_timer}stopped}. *)

  val stop_timer : timer_id -> unit
  (** [stop_timer tid] stops timer [tid]. *)
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
