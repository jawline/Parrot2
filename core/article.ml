open Core
open Markdown_parser

type t =
  { title : string
  ; uuid : string
  ; tags : string list
  ; created : string
  ; intro : Markdown_parser.t
  ; full_contents : Markdown_parser.t
  }
[@@deriving show, sexp]

exception NoMeta of string

let path_of_uuid uuid = sprintf "/articles/%s" (Util.sanitize_path uuid)
let path article = path_of_uuid article.uuid

let rec meta_intro_until lines =
  match lines with
  | [] -> raise (NoMeta "There is no intro end")
  | x :: _ when String.is_prefix ~prefix:"!=!=! Intro: End" x -> []
  | x :: lines -> x :: meta_intro_until lines
;;

let rec meta_extract_intro lines =
  match lines with
  | [] -> raise (NoMeta "There is no intro start")
  | x :: lines when String.is_prefix ~prefix:"!=!=! Intro: Start" x ->
    meta_intro_until lines
  | _ :: lines -> meta_extract_intro lines
;;

let rec meta_extract_string prefix lines =
  match lines with
  | [] -> raise (NoMeta prefix)
  | x :: _ when String.is_prefix ~prefix x ->
    String.strip (String.drop_prefix x (String.length prefix))
  | _ :: lines -> meta_extract_string prefix lines
;;

let rec remove_meta_lines xs =
  match xs with
  | [] -> []
  | x :: xs when String.is_prefix ~prefix:"!=!=!" x -> remove_meta_lines xs
  (* We allow escaping of meta lines using \ and remove it with this rule *)
  | x :: xs when String.is_prefix ~prefix:"\\!=!=!" x -> (String.drop_prefix x 1) :: remove_meta_lines xs
  | x :: xs -> x :: remove_meta_lines xs
;;

let title_to_html (article : t) = article.title

(* If the creation time is an epoch time string then we format it using a UTC
   timezone, otherwise we assume it's a specific string. *)
(* TODO: This is horrible legacy, I should standardize on one or the other *)
let created_time_to_html (article : t) ~(zone : Time_unix.Zone.t) =
  try
    let created =
      Time_unix.of_span_since_epoch (Time_unix.Span.of_sec (Float.of_string article.created))
    in
    Time_unix.format created "%d-%m-%Y" ~zone
  with
  | Invalid_argument _ -> article.created
;;

let to_html (article : t) = Markdown_parser.to_html article.full_contents

let intro_to_html (article : t) = Markdown_parser.to_html article.intro

let tags_to_html (article : t) = String.concat ~sep:", " article.tags

let created_epoch (article : t) ~(zone : Time_unix.Zone.t) =
  try Float.of_string article.created with
  | Invalid_argument _ ->
    Time_unix.Span.to_sec
      (Time_unix.to_span_since_epoch (Time_unix.parse ~zone ~fmt:"%d-%m-%Y" article.created))
;;

let meta_extract_tags lines =
  List.map
    ~f:(fun x -> String.strip x)
    (String.split ~on:',' (meta_extract_string "!=!=! Tags:" lines)) |> List.filter ~f:(fun tag -> not (String.is_empty tag))
;;

let ingest_string ~article =
  let article = String.split_lines article in
  let title = meta_extract_string "!=!=! Title:" article in
  let uuid = meta_extract_string "!=!=! Uuid:" article in
  let tags = meta_extract_tags article in
  let created = meta_extract_string "!=!=! Created:" article in
  let intro =
    parse (String.concat ~sep:"\n" (meta_extract_intro article))
  in
  let article_markdown = String.concat ~sep:"\n" (remove_meta_lines article) in
  let article_parsed_markdown = parse article_markdown in
  { title 
  ; uuid
  ; tags 
  ; created 
  ; intro 
  ; full_contents = article_parsed_markdown
  }

let ingest_file ~path : t =
  let article = In_channel.read_all path in
ingest_string ~article
;;

let sample_article = "!=!=! Uuid:123456\n!=!=! Title: Test Article\n!=!=! Tags: Cow\n!=!=! Created: 3949\n!=!=! Intro: Start\nTest Intro.\n!=!=! Intro: End\nHello World.\n- A\n- B\n- C\n"

let sample_article_similar_line = "!=!=! Uuid:123456\n!=!=! Title: Test Article\n!=!=! Tags: Cow\n!=!=! Created: 3949\n!=!=! Intro: Start\nTest Intro.\n!=!=! Intro: End\n!=!= Donkey!\nHello World.\n- A\n- B\n- C\n"

let sample_article_no_intro = "!=!=! Uuid:123456\n!=!=! Title: Test Article\n!=!=! Tags: Cow\n!=!=! Created: 3949\nHello World.\n- A\n- B\n- C\n"

let sample_article_no_created = "!=!=! Uuid:123456\n!=!=! Title: Test Article\n!=!=! Tags: Cow\n!=!=! Intro: Start\nTest Intro.\n!=!=! Intro: End\nHello World.\n- A\n- B\n- C\n"

let sample_article_no_tags = "!=!=! Uuid:123456\n!=!=! Title: Test Article\n!=!=! Created: 3949\n!=!=! Intro: Start\nTest Intro.\n!=!=! Intro: End\nHello World.\n- A\n- B\n- C\n"

let%expect_test "article ingest" =
        let ingested = ingest_string ~article:sample_article in
        print_s [%message (ingested : t)];
  [%expect {|
    (ingested
     ((title "Test Article") (uuid 123456) (tags (Cow)) (created 3949)
      (intro (Fragments ((Paragraph ((Text "Test Intro."))))))
      (full_contents
       (Fragments
        ((Paragraph ((Text "Test Intro. Hello World.")))
         (List (style Unordered)
          (items
           ((Fragments ((Text A))) (Fragments ((Text B))) (Fragments ((Text C))))))))))) |}]
;;

let%expect_test "article ingest with similar line to template" =
  let ingested = ingest_string ~article:sample_article_similar_line in
  print_s [%message (ingested : t)];
  [%expect {|
    (ingested
     ((title "Test Article") (uuid 123456) (tags (Cow)) (created 3949)
      (intro (Fragments ((Paragraph ((Text "Test Intro."))))))
      (full_contents
       (Fragments
        ((Paragraph
          ((Text "Test Intro. ") (Text !=) (Text "!= Donkey")
           (Text "! Hello World.")))
         (List (style Unordered)
          (items
           ((Fragments ((Text A))) (Fragments ((Text B))) (Fragments ((Text C))))))))))) |}]
;;

let expect_raises fn =
        try fn () 
        with ex -> print_s [%message ( ex : exn) ]
;;

let%expect_test "article ingest with no intro" =
  expect_raises (fun () -> let ingested = ingest_string ~article:sample_article_no_intro in
  print_s [%message (ingested : t)]; );
  [%expect {| (ex ("Parrot_core__Article.NoMeta(\"There is no intro start\")")) |}]
;;

let%expect_test "article ingest with no tags" =
  expect_raises (fun () -> let ingested = ingest_string ~article:sample_article_no_tags in
  print_s [%message (ingested : t)]; );
  [%expect {| (ex ("Parrot_core__Article.NoMeta(\"!=!=! Tags:\")")) |}]
;;

let%expect_test "article ingest with no created" =
        expect_raises (fun () -> 
  let ingested = ingest_string ~article:sample_article_no_created in
  print_s [%message (ingested : t)]);
  [%expect {| (ex ("Parrot_core__Article.NoMeta(\"!=!=! Created:\")")) |}]
;;

let%expect_test "extract tags" =
        let extracted = meta_extract_tags ["!=!=! Tags: Cat"; "!=!=! Tags: Dog"; "Hello"] in
        print_s [%message (extracted : string list)];
        [%expect {| (extracted (Cat)) |}]
;;
