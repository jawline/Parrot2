open Core

exception InvalidTimezone

let out_index_path = sprintf "%s/index.html"

let out_article_path ~(article : Article.t) output_base =
  sprintf "%s/%s.html" output_base (Article.path article)
;;

let out_tags_path tag_name output_base =
  sprintf "%s/tags/%s.html" output_base (Util.sanitize_path tag_name)
;;

let emit_tag tag_name tag tags_template tag_template output_folder ~template_rules ~zone =
  Out_channel.write_all
    (out_tags_path tag_name output_folder)
    ~data:(Template.render_tag tag_name tag ~tags_template ~tag_template ~template_rules ~zone)
;;

let emit_article article article_template output_folder ~template_rules ~zone =
  Out_channel.write_all
    (out_article_path ~article output_folder)
    ~data:(Template.render_article article ~template:article_template ~template_rules ~zone)
;;

let emit_index base output_folder ~template_rules =
  Out_channel.write_all
    (out_index_path output_folder)
    ~data:(Template.render_index (Template.load_index_template base) ~template_rules)
;;

let clean_build input_directory =
  let input_directory = input_directory ^ "/" in
  let output_directory = input_directory ^ "_build/" in
  let article_template = Template.load_article_template input_directory in
  let tag_template = Template.load_tag_template input_directory in
  let tags_template = Template.load_tags_template input_directory in
  let template_rules = Template_engine.make (Template.load_nav_template input_directory) in
  FileUtil.rm ~recurse:true [ output_directory ];
  FileUtil.mkdir (sprintf "%s/" output_directory);
  FileUtil.mkdir (sprintf "%s/articles/" output_directory);
  FileUtil.mkdir (sprintf "%s/tags/" output_directory);
  let timezone =
    match Time_unix.Zone.find "utc" with
    | Some v -> v
    | None -> raise_s [%message "timezone utc is invalid"]
  in
  let tags = Tags.make () in
  let rec emit_articles = function
    | [] -> ()
    | path :: xs ->
      let article = Article.ingest_file ~path in
      printf "Processing: %s\n" article.title;
      emit_article
        article
        article_template
        output_directory
        ~template_rules
        ~zone:timezone;
      Tags.add tags article;
      emit_articles xs
  in
  let rec emit_tags = function
    | [] -> ()
    | (tag_name, tag_items) :: xs ->
      printf "Processing Tag: %s\n" tag_name;
      emit_tag
        tag_name
        tag_items
        tags_template
        tag_template
        output_directory
        ~template_rules
        ~zone:timezone;
      emit_tags xs
  in
  FileUtil.cp ~recurse:true [ input_directory ^ "static/" ] output_directory;
  FileUtil.cp ~recurse:true [ input_directory ^ "images/" ] (output_directory ^ "images/");
  emit_index input_directory output_directory ~template_rules;
  emit_articles (Articles.all_articles input_directory);
  emit_tags (Hashtbl.to_alist tags);
  printf "Finished. Output -> %s\n" output_directory
;;
