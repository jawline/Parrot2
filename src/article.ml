open Core
open Markdown_parser

type t = {
  name: string;
  tags: string list;
  created: string;
  intro: Markdown_parser.t;
  full_contents: Markdown_parser.t;
} [@@deriving show]

exception NoMeta of string

let rec meta_intro_until lines =
  match lines with
  | [] -> raise (NoMeta "There is no intro end")
  | x::_ when String.is_prefix ~prefix:"!=!=! Intro: End" x -> []
  | x::lines -> x::meta_intro_until lines
;;

let rec meta_extract_intro lines =
  match lines with
  | [] -> raise (NoMeta "There is no intro start")
  | x::lines when String.is_prefix ~prefix:"!=!=! Intro: Start" x -> meta_intro_until lines
  | _::lines -> meta_extract_intro lines
;;

let rec meta_extract_string prefix lines =
  match lines with
  | [] -> raise (NoMeta prefix)
  | x::_ when String.is_prefix ~prefix:prefix x -> String.strip (String.drop_prefix x (String.length prefix))
  | _::lines -> meta_extract_string prefix lines
;;

let rec remove_meta_lines xs =
  match xs with
  | [] -> []
  | (x::xs) when String.is_prefix ~prefix:"!=!=!" x -> remove_meta_lines xs
  | (x::xs) -> x::remove_meta_lines xs
;;

let meta_extract_tags lines = List.map ~f:(fun x -> String.strip x) (String.split ~on:',' (meta_extract_string "!=!=! Tags:" lines))
;;

let ingest_article (article_path: string): t =
  let article_contents = String.split_lines (In_channel.read_all article_path) in
  let article_title = meta_extract_string "!=!=! Title:" article_contents in
  let article_tags = meta_extract_tags article_contents in
  let article_created = meta_extract_string "!=!=! Created:" article_contents in
  let article_intro = parse (String.concat ~sep:"\n" (meta_extract_intro article_contents)) in
  let article_markdown = String.concat ~sep:"\n" (remove_meta_lines article_contents) in
  let article_parsed_markdown = parse article_markdown in
  { name = article_title; tags = article_tags; created = article_created; intro = article_intro; full_contents = article_parsed_markdown }
