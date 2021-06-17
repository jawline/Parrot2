open Core

let templates_subdir base = sprintf "%s/templates" base

(* Articles are constructed out of the article template file *)
let article_template base = sprintf "%s/article.html" (templates_subdir base)

(* A tag template is used to create tag items (descriptions of an article) in the page for a specific page *)
let tag_template base = sprintf "%s/tag.html" (templates_subdir base)

(* The tags template contains a list of tag entries (descriptions of articles) with links *)
let tags_template base = sprintf "%s/tags.html" (templates_subdir base)

(* The nav bar at the top is static but appears in multiple different templates. Because of this we pull it out into it's own template file
  * TODO: It would be more powerful to use a template-schema like ${{{file:/templates/nav.html}}} across all templates in the future. *)
let nav_template base = sprintf "%s/nav.html" (templates_subdir base)
let index_template base = sprintf "%s/index.html" (templates_subdir base)

(* Replace a template identifier like ${{{HELLO}}} by a substituted value *)
let template_replace template_id fragment =
  String.substr_replace_first ~pattern:("${{{" ^ template_id ^ "}}}") ~with_:fragment
;;

let load_nav_template base = In_channel.read_all (nav_template base)
let load_index_template base = In_channel.read_all (index_template base)
let load_tag_template base = In_channel.read_all (tag_template base)
let load_tags_template base = In_channel.read_all (tags_template base)
let load_article_template base = In_channel.read_all (article_template base)

let render_article
    (article : Article.t)
    ~(template : string)
    ~(template_rules : Template_engine.t)
    ~(zone : Time.Zone.t)
  =
  let templated_content =
    Template_engine.apply ~rules:template_rules (Article.to_html article)
  in
  Template_engine.apply
    template
    ~rules:
      (template_rules
      |> Template_engine.a (Template_engine.c "ARTICLE_CONTENT" templated_content)
      |> Template_engine.a
           (Template_engine.c "ARTICLE_TITLE" (Article.title_to_html article))
      |> Template_engine.a
           (Template_engine.c "ARTICLE_TIME" (Article.created_time_to_html article ~zone))
      )
;;

let render_tag_item
    (article : Article.t)
    ~(template : string)
    ~(template_rules : Template_engine.t)
    ~(zone : Time.Zone.t)
  =
  Template_engine.apply
    template
    ~rules:
      (template_rules
      |> Template_engine.a (Template_engine.c "LI_NAME" (Article.title_to_html article))
      |> Template_engine.a
           (Template_engine.c "LI_DESCRIPTION" (Article.intro_to_html article))
      |> Template_engine.a (Template_engine.c "LI_TAGS" (Article.tags_to_html article))
      |> Template_engine.a
           (Template_engine.c "LI_CREATED" (Article.created_time_to_html article ~zone))
      |> Template_engine.a
           (Template_engine.c "LI_TARGET" (Util.article_path article.name)))
;;

let render_tag
    (tag_name : string)
    (tag : Tag.t)
    ~(tags_template : string)
    ~(tag_template : string)
    ~(template_rules : Template_engine.t)
    ~(zone : Time.Zone.t)
  =
  let tags =
    String.concat
      ~sep:"\n"
      (List.map tag ~f:(fun article ->
           render_tag_item article ~template:tag_template ~template_rules ~zone))
  in
  Template_engine.apply
    tags_template
    ~rules:
      (template_rules
      |> Template_engine.a (Template_engine.c "LIST_TITLE" tag_name)
      |> Template_engine.a (Template_engine.c "LIST_CONTENT" tags))
;;

let render_index template ~(template_rules : Template_engine.t) =
  Template_engine.apply ~rules:template_rules template
;;
