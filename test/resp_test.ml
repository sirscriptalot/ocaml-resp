open Resp

let check predicate =
  assert predicate;

  Printf.printf "."

let check_equal a b =
  check (a = b)

let make () =
  let resp = connect_to_uri (Uri.of_string @@ Sys.getenv "REDIS_URL") in
  let _    = call resp ["SELECT"; Sys.getenv "REDIS_INDEX"] in
  let _    = call resp ["FLUSHDB"] in
  resp

let test_call_and_receive_simple_string () =
  let resp = make () in
  check_equal (call resp ["PING"]) (Response.of_string "PONG")

let test_call_and_receive_error () =
  let resp = make () in
  try let _ = call resp ["INCRBY"; "foo"; "bar"] in check false
  with Invalid_argument _ -> check true

let test_call_and_receive_integer () =
  let resp = make () in
  check_equal (call resp ["SET"; "key"; "0"]) (Response.of_string "OK");
  check_equal (call resp ["INCR"; "key"]) (Response.of_int 1)

let test_call_and_receive_bulk_string () =
  let resp = make () in
  check_equal (call resp ["SET"; "key"; "value"]) (Response.of_string "OK");
  check_equal (call resp ["GET"; "key"]) (Response.of_string "value")

let test_call_and_receive_array () =
  let resp = make () in
  check_equal (call resp ["SET"; "foo"; "bar"]) (Response.of_string "OK");
  check_equal (call resp ["SET"; "baz"; "bat"]) (Response.of_string "OK");
  check_equal (call resp ["MGET"; "foo"; "baz"]) (Response.of_list [Response.of_string "bar"; Response.of_string "bat"])

(* Uri_helper *)

let test_uri_helper_to_host_default () =
  let uri = Uri.of_string "" in
  check_equal (Uri_helper.to_host uri) Uri_helper.default_host

let test_uri_helper_to_port_default () =
  let uri = Uri.of_string "" in
  check_equal (Uri_helper.to_port uri) Uri_helper.default_port

let test_uri_helper_to_address () =
  let address = Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 8080) in
  let uri     = Uri.of_string "//0.0.0.0:8080" in
  check_equal (Uri_helper.to_address uri) address

(* Request *)

let test_request_to_string () =
  check_equal (Request.to_string ["foo"]) "*1\r\n$3\r\nfoo\r\n";
  check_equal (Request.to_string ["foo"; "bar"]) "*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n"

(* Response *)

let test_response_get_int () =
  let i       = 0 in
  let payload = Response.of_int i in
  check_equal (Response.get_int payload) i

let test_response_get_list () =
  let l = [Response.of_int 0; Response.of_string "Foo"] in
  let payload = Response.of_list l in
  check_equal (Response.get_list payload) l

let test_response_get_string () =
  let s       = "Foo" in
  let payload = Response.of_string s in
  check_equal (Response.get_string payload) s

let test_response_of_int () =
  let i = 0 in
  check_equal (Response.of_int i) (Response.Int i)

let test_response_of_list () =
  let l = [Response.of_int 0; Response.of_string "Foo"] in
  check_equal (Response.of_list l) (Response.List (Some l))

let test_response_of_string () =
  let s = "Foo" in
  check_equal (Response.of_string s) (Response.String (Some s))

let () =
  test_call_and_receive_simple_string ();
  test_call_and_receive_error ();
  test_call_and_receive_integer ();
  test_call_and_receive_bulk_string ();
  test_call_and_receive_array ();
  test_uri_helper_to_host_default ();
  test_uri_helper_to_port_default ();
  test_uri_helper_to_address ();
  test_request_to_string ();
  test_response_get_int ();
  test_response_get_list ();
  test_response_get_string ();
  test_response_of_int ();
  test_response_of_list ();
  test_response_of_string ();
  ()
