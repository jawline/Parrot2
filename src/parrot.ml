open Core
open Article
open Template

exception InvalidArticleName of string

let out_article_name = function
  | "" -> raise (InvalidArticleName "all whitespace")
  | name -> name |> String.substr_replace_all ~pattern:" " ~with_:"_"  |> String.substr_replace_all ~pattern:"-" ~with_:"slash" |> String.substr_replace_all ~pattern:"," ~with_:"comma" |> String.substr_replace_all ~pattern:"'" ~with_:"single_quote" |> String.substr_replace_all ~pattern:"\"" ~with_:"quote"
;;

let out_article_path article output_base =
  sprintf "%s/%s.html" output_base (out_article_name article.name)
;;

let emit_article article article_template output_folder =
  Out_channel.write_all (out_article_path article output_folder) ~data:(render_article_template article article_template)
;;

let%test "directory_scan" =
  let base = "/home/blake/Parrot2/website/" in
  let output_base = "/home/blake/Parrot2/out/" in
  let article_template = load_article_template base in
  let rec print_list = function
    | [] -> ()
    | x :: xs ->
      let article = ingest_article x in
      emit_article article article_template output_base;
      print_list xs
  in
  print_list (Articles.all_articles base);
  true
;;
