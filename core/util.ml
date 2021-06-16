open Core

exception InvalidArticleName of string

let sanitize_path = function
  | "" -> raise (InvalidArticleName "all whitespace")
  | name ->
    name
    |> String.substr_replace_all ~pattern:" " ~with_:"_"
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> String.substr_replace_all ~pattern:"," ~with_:"_"
    |> String.substr_replace_all ~pattern:"'" ~with_:""
    |> String.substr_replace_all ~pattern:"\"" ~with_:""
    |> String.substr_replace_all ~pattern:":" ~with_:""
    |> String.substr_replace_all ~pattern:"." ~with_:"_"
    |> String.substr_replace_all ~pattern:"&" ~with_:"and"
;;

let article_path article_name = sprintf "article/%s" (sanitize_path article_name)
