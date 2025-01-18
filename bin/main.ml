open Lwt

let clients = ref []

let broadcast sender_oc message =
  let send_to_client oc =
    if oc != sender_oc then Lwt_io.write_line oc message else Lwt.return_unit
  in
  Lwt_list.iter_p send_to_client !clients

let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10

let handle_connection ic oc () =
  clients := oc :: !clients;
  let client_id = string_of_int (List.length !clients) in

  let rec chat_loop () =
    Lwt_io.write oc "> " >>= fun () ->
    Lwt_io.read_line_opt ic >>= function
    | Some msg ->
        let formatted_msg = Printf.sprintf "Client %s: %s" client_id msg in
        broadcast oc formatted_msg >>= chat_loop
    | None ->
        clients := List.filter (fun x -> x != oc) !clients;
        Logs_lwt.info (fun m -> m "Client %s disconnected" client_id) >>= return
  in

  broadcast oc (Printf.sprintf "Client %s joined the chat" client_id)
  >>= chat_loop

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e ->
      Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  Logs_lwt.info (fun m -> m "New connection") >>= return

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (bind sock @@ ADDR_INET (listen_address, port) |> fun x -> ignore x);
  listen sock backlog;
  sock

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

let () =
  let sock = create_socket () in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run (create_server sock ())
