open Core

let articles_subdir base = sprintf "%s/articles/" base
let is_article path = String.is_suffix ~suffix:".md" path

let all_articles base =
  let articles_base = articles_subdir base in
  List.map
    ~f:(fun article_path -> String.concat [ articles_base; article_path ])
    (List.filter
       ~f:(fun article_path -> is_article article_path)
       (Sys.ls_dir (articles_subdir base)))
;;
