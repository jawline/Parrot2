open Core

let templates_subdir base = sprintf "%s/templates" base

(* Articles are constructed out of the article template file *)
let article_template base = sprintf "%s/article.html" (templates_subdir base)

(* The nav bar at the top is static but appears in multiple different templates. Because of this we pull it out into it's own template file
  * TODO: It would be more powerful to use a template-schema like ${{{file:/templates/nav.html}}} across all templates in the future. *)
let nav_template base = sprintf "%s/nav.html" (templates_subdir base)

(* Replace a template identifier like ${{{HELLO}}} by a substituted value *)
let template_replace template_id fragment =
  String.substr_replace_first ~pattern:("${{{" ^ template_id ^ "}}}") ~with_:fragment
;;

let load_article_template base =
  let nav_template = In_channel.read_all (nav_template base) in
  template_replace "NAV_BAR_CONTENT" nav_template (In_channel.read_all (article_template base))

let render_article_template article template =
  template
  |> template_replace "ARTICLE_CONTENT" (Article.to_html article)
  |> template_replace "ARTICLE_TITLE" (Article.title_to_html article)
  |> template_replace "ARTICLE_TIME" (Article.created_time_to_html article)
;;
