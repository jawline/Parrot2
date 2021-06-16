open Core
open Article
open Template

exception InvalidArticleName of string
exception InvalidTimezone

let out_article_name = function
  | "" -> raise (InvalidArticleName "all whitespace")
  | name ->
    name
    |> String.substr_replace_all ~pattern:" " ~with_:"_"
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> String.substr_replace_all ~pattern:"," ~with_:"_"
    |> String.substr_replace_all ~pattern:"'" ~with_:""
    |> String.substr_replace_all ~pattern:"\"" ~with_:""
    |> String.substr_replace_all ~pattern:":" ~with_:""
    |> String.substr_replace_all ~pattern:"." ~with_:"_"
    |> String.substr_replace_all ~pattern:"&" ~with_:"and"
;;

let out_index_path = sprintf "%s/index.html"

let out_article_path article output_base =
  sprintf "%s/%s.html" output_base (out_article_name article.name)
;;

let emit_article article article_template output_folder ~zone =
  Out_channel.write_all
    (out_article_path article output_folder)
    ~data:(render_article article article_template ~zone)
;;

let emit_index base output_folder =
  Out_channel.write_all
    (out_index_path output_folder)
    ~data:(render_index (load_index_template base))
;;

let%test "directory_scan" =
  let base = "/home/blake/Parrot2/website/" in
  let output_base = "/home/blake/Parrot2/out/" in
  let article_template = load_article_template base in
  let timezone = match Time.Zone.find "utc" with
  | Some v -> v
  | None -> raise InvalidTimezone
  in
  let rec print_list = function
    | [] -> ()
    | x :: xs ->
      let article = ingest_article x in
      printf "Processing: %s\n" article.name;
      emit_article article article_template output_base ~zone:timezone;
      print_list xs
  in
  emit_index base output_base;
  print_list (Articles.all_articles base);
  true
;;
