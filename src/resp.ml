module Socket_helper = struct
  let of_address address =
    let domain = Unix.domain_of_sockaddr address in
    let socket = Unix.socket domain Unix.SOCK_STREAM 0 in
    Unix.connect socket address;
    socket

  let discard length socket =
    let bytes = Bytes.create length in
    let _     = Unix.recv socket bytes 0 length [] in
    ()

  let discard_crlf =
    discard 2

  let discard_lf =
    discard 1

  let receive f socket length =
    let bytes = Bytes.create length in
    f bytes 0 (Unix.recv socket bytes 0 length [])

  let receive_bytes =
    receive Bytes.sub

  let receive_char socket =
    Bytes.get (receive_bytes socket 1) 0

  let receive_string socket length =
    let str = receive Bytes.sub_string socket length in
    discard_crlf socket;
    str

  let receive_string_until_crlf socket =
    let rec loop acc =
      match (receive_char socket) with
      | '\r' -> discard_lf socket; acc
      | c    -> loop (String.make 1 c :: acc)
    in

    String.concat "" (List.rev (loop []))

  let receive_string_until_crlf_to_int socket =
    int_of_string (receive_string_until_crlf socket)

  let send_string socket s =
    Unix.send_substring socket s 0 (String.length s) []
end

module Uri_helper = struct
  let default_host = Unix.inet_addr_of_string "127.0.0.1"

  let default_port = 6379

  let to_host uri =
    match (Uri.host uri) with
    | Some host -> Unix.inet_addr_of_string host
    | None      -> default_host

  let to_port uri =
    match (Uri.port uri) with
    | Some port -> port
    | None      -> default_port

  let to_address uri =
    Unix.ADDR_INET (to_host uri, to_port uri)
end

module Request = struct
  type t = string list

  let encode_element element =
    Printf.sprintf "$%d\r\n%s\r\n" (Bytes.length (Bytes.of_string element)) element

  let encode_elements elements =
    List.map encode_element elements

  let encode_length request =
    Printf.sprintf "*%d\r\n" (List.length request)

  let to_string request =
    String.concat "" (encode_length request :: encode_elements request)
end

module Response = struct
  type t =
    | Int    of int
    | List   of t list option
    | String of string option

  let none_list = List None

  let none_string = String None

  let get_int response =
    match response with
    | Int i    -> i
    | List _   -> invalid_arg "response is List _"
    | String _ -> invalid_arg "response is String _"

  let get_list response =
    match response with
    | Int _    -> invalid_arg "response is Int _"
    | List o   -> Option.get o
    | String _ -> invalid_arg "response is String _"

  let get_string response =
    match response with
    | Int _    -> invalid_arg "response is Int _"
    | List _   -> invalid_arg "response is List _"
    | String o -> Option.get o

  let of_int i = Int i

  let of_list l = List (Option.some l)

  let of_string s = String (Option.some s)

  let receive_simple_string socket =
    of_string (Socket_helper.receive_string_until_crlf socket)

  let receive_error socket =
    invalid_arg (Socket_helper.receive_string_until_crlf socket)

  let receive_int socket : t =
    of_int (Socket_helper.receive_string_until_crlf_to_int socket)

  let receive_bulk_string socket =
    match (Socket_helper.receive_string_until_crlf_to_int socket) with
    | -1     -> none_string
    | 0      -> of_string ""
    | length -> of_string (Socket_helper.receive_string socket length)

  let rec receive_array socket =
    let length = Socket_helper.receive_string_until_crlf_to_int socket in

    let rec loop i acc =
      if (i < length) then
        loop (i + 1) (receive socket :: acc)
      else
        List.rev acc
    in

    match length with
    | -1 -> none_list
    | 0  -> of_list []
    | _  -> of_list (loop 0 [])
  and receive socket =
    match (Socket_helper.receive_char socket) with
    | '+' -> receive_simple_string socket
    | '-' -> receive_error socket
    | ':' -> receive_int socket
    | '$' -> receive_bulk_string socket
    | '*' -> receive_array socket
    | c   -> invalid_arg ("received unexpected char " ^ String.make 1 c)
end

type t = {
  socket : Unix.file_descr
}

let call { socket; _ } request =
  let _ = Socket_helper.send_string socket (Request.to_string request) in
  Response.receive socket

let connect_to_address address =
  { socket = Socket_helper.of_address address }

let connect_to_uri uri =
  connect_to_address (Uri_helper.to_address uri)
