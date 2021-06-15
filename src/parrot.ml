open Core
open Article
open Template

let%test "directory_scan" =
  let base = "/home/blake/Parrot2/website/" in
  let article_template = load_article_template base in
  let rec print_list = function
    | [] -> ()
    | x :: xs ->
      let article = ingest_article x in
      printf "%s\n" (render_article_template article article_template);
      print_list xs
  in
  print_list (Articles.all_articles base);
  true
;;
