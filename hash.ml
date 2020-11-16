open Bechamel
open Toolkit

external unsafe_get_int64 : string -> int -> int64 = "%caml_string_get64u"

let v0 v = Int64.to_int (unsafe_get_int64 v 0)
external v1 : string -> int = "caml_hash_v1" [@@noalloc]
external v2 : string -> int = "caml_hash_v2" [@@noalloc]

let () = Random.self_init ()

let random_max = 32767.

let ( <.> ) f g x = f (g x)

let to_hex n =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  let tmp = ref n in
  while !tmp <> 0 do
    Format.fprintf ppf "%02x" (!tmp mod 16) ;
    tmp := !tmp / 16
  done ; Format.fprintf ppf "%!" ; Buffer.contents buf

let normal k n =
  let m = n + (n mod 2) in
  let values = Array.create_float m in
  for i = 0 to (m / 2) - 1 do
    let x = ref 0. and y = ref 0. and rsq = ref 0. in
    while
      x := (Random.float random_max /. random_max *. 2.0) -. 1. ;
      y := (Random.float random_max /. random_max *. 2.0) -. 1. ;
      rsq := (!x *. !x) +. (!y *. !y) ;
      !rsq >= 1. || !rsq = 0.
    do
      ()
    done ;
    let f = sqrt (-2.0 *. log !rsq /. !rsq) in
    values.(i * 2) <- !x *. f ;
    values.((i * 2) + 1) <- !y *. f
  done ;
  Array.map (Digestif.of_hex k <.> to_hex <.> Float.to_int <.> ( *. ) random_max) values

let v0 k n = 
  let vs = normal k n in
  Staged.stage @@ fun () ->
  Array.iter (ignore <.> v0 <.> Digestif.to_raw_string k) vs

let v1 k n = 
  let vs = normal k n in
  Staged.stage @@ fun () ->
  Array.iter (ignore <.> v1 <.> Digestif.to_raw_string k) vs

let v2 k n =
  let vs = normal k n in
  Staged.stage @@ fun () ->
  Array.iter (ignore <.> v2 <.> Digestif.to_raw_string k) vs

let test0 =
  Test.make_indexed ~name:"v0" ~fmt:"%s %d" ~args:[ 10; 50; 100 ]
    (v0 Digestif.SHA1)

let test1 =
  Test.make_indexed ~name:"v1" ~fmt:"%s %d" ~args:[ 10; 50; 100 ]
    (v1 Digestif.SHA1)

let test2 =
  Test.make_indexed ~name:"v2" ~fmt:"%s %d" ~args:[ 10; 50; 100 ]
    (v2 Digestif.SHA1)

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) () in
  let raw_results =
    Benchmark.all cfg instances
      (Test.make_grouped ~name:"hash" ~fmt:"%s %s" [ test0; test1; test2 ]) in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let compare k0 k1 =
  let a = ref 0 and b = ref 0 in
  Scanf.sscanf k0 "%s %s %d" (fun _ _ a' -> a := a') ;
  Scanf.sscanf k1 "%s %s %d" (fun _ _ b' -> b := b') ;
  !a - !b

let nothing _ = Ok ()

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

let () =
  let results = benchmark () in
  match Sys.argv with
  | [| _; "cli"; |] ->
    let open Notty_unix in
    List.iter
      (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
      Instance.[ minor_allocated; major_allocated; monotonic_clock ] ;
    let window =
      match winsize Unix.stdout with
      | Some (w, h) -> { Bechamel_notty.w; h }
      | None -> { Bechamel_notty.w = 80; h = 1 } in
    let results, _ = benchmark () in
    img (window, results) |> eol |> output_image
  | [| _; "json"; |] ->
    let results =
      let open Bechamel_js in
      emit ~dst:(Channel stdout) nothing ~compare ~x_label:Measure.run
        ~y_label:(Measure.label Instance.monotonic_clock)
        results in
    ( match results with Ok () -> () | Error (`Msg err) -> invalid_arg err )
  | _ -> Format.eprintf "%s [cli|json]\n%!" Sys.argv.(0)

