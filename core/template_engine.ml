open Core

exception NoRule of string

type replacement_rule = string -> string option
type t = replacement_rule list

let article_replacer template =
  if String.is_prefix template ~prefix:"article:"
  then (
    let template = String.drop_prefix template (String.length "article:") in
    Some (Util.article_path template))
  else None
;;

let image_replacer template =
  printf "Replacing %s\n" template;
  if String.is_prefix template ~prefix:"img:"
  then Some (Util.image_path (String.drop_prefix template 4))
  else None
;;

let c (from : string) (to_ : string) template =
  if String.( = ) template from then Some to_ else None
;;

let make nav_template =
  [ c "NAV_BAR_CONTENT" nav_template; image_replacer; article_replacer ]
;;

let a (rule : replacement_rule) (list_ : t) = rule :: list_

let apply (fragment : string) ~(rules : t) =
  let rec apply_rules rules template =
    match rules with
    | [] -> raise (NoRule template)
    | rule :: xs ->
      (match rule template with
      | Some r -> r
      | None -> apply_rules xs template)
  in
  let template_regexp = Re.Pcre.regexp "\\$\\{\\{\\{.*?\\}\\}\\}" in
  Re.Pcre.substitute
    ~rex:template_regexp
    ~subst:(fun template ->
      apply_rules rules (String.drop_prefix (String.drop_suffix template 3) 4))
    fragment
;;
