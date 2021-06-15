open Core

let templates_subdir base = sprintf "%s/templates" base
let article_template base = sprintf "%s/article.html" (templates_subdir base)
let load_article_template base = In_channel.read_all (article_template base)

let template_replace template_id fragment =
  String.substr_replace_first ~pattern:("${{{" ^ template_id ^ "}}}") ~with_:fragment
;;

let render_article_template article template =
  template
  |> template_replace "ARTICLE_CONTENT" (Article.to_html article)
  |> template_replace "ARTICLE_TITLE" (Article.title_to_html article)
  |> template_replace "ARTICLE_TIME" (Article.created_time_to_html article)
;;
