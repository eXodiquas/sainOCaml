(** 
[get_dictionary_path path] reads a textfile from a path and splits the content at newlines. 
*)
let get_dictionary path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  String.split_on_char '\n' s

(** 
[swap_letters s1 s2] takes the strings `s1` and `s2` and swaps the first letter
of both strings around. The upper- and lowercase state of the string is preserverd.
If at least one of the strings is empty this function returns
(s1, s2).   
*)
let swap_letters s1 s2 =
  try begin
  let open String in
  let is_upper c =
    c = Char.uppercase_ascii c in
  let fl1 = sub s1 0 1 in
  let fl2 = sub s2 0 1 in
  let ss1 = sub s1 1 (length s1 - 1) in
  let ss2 = sub s2 1 (length s2 - 1) in
  match (is_upper (get fl1 0), is_upper (get fl2 0)) with
  | (false, false) -> [fl2 ^ ss1; fl1 ^ ss2]
  | (false,  true) -> [(lowercase_ascii fl2) ^ ss1; (uppercase_ascii fl1 ^ ss2)]
  | (true,  false) -> [(uppercase_ascii fl2) ^ ss1; (lowercase_ascii fl1 ^ ss2)]
  | (true,   true) -> [fl2 ^ ss1; fl1 ^ ss2] end
  with
  | Invalid_argument _ -> [s1; s2]

(** 
[swap_letters_exn s1 s2] takes the strings `s1` and `s2` and swaps the first letter
of both strings around. The upper- and lowercase state of the string is preserverd. 
@raise Invalid_argument if either [s1] or [s2] is an empty string.   
*)
let swap_letters_exn s1 s2 =
  let open String in
  let is_upper c =
    c = Char.uppercase_ascii c in
  let fl1 = sub s1 0 1 in
  let fl2 = sub s2 0 1 in
  let ss1 = sub s1 1 (length s1 - 1) in
  let ss2 = sub s2 1 (length s2 - 1) in
  match (is_upper (get fl1 0), is_upper (get fl2 0)) with
  | (false, false) -> [fl2 ^ ss1; fl1 ^ ss2]
  | (false,  true) -> [(lowercase_ascii fl2) ^ ss1; (uppercase_ascii fl1 ^ ss2)]
  | (true,  false) -> [(uppercase_ascii fl2) ^ ss1; (lowercase_ascii fl1 ^ ss2)]
  | (true,   true) -> [fl2 ^ ss1; fl1 ^ ss2]

(**
[map_two_strings fn l] takes a function [fn: string -> string -> string list] and applies it to consecutive string pairs in [l].
*)
let map_two_strings fn l =
  let rec f l a =
    match l with
    | []           -> a
    | [x]          -> [x; ""] :: a
    | x :: y :: xs -> f xs ((fn x y) :: a) in
  (f l []) |> List.rev |> List.flatten

(**
ENTRY POINT
*)
let () =
  let input = String.split_on_char ' ' (read_line ()) in
  let result = map_two_strings swap_letters input in
  print_endline (String.concat " " result)
  
