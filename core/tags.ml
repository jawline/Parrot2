open Core

type t = (string, Tag.t) Hashtbl.Poly.t

let make () = Hashtbl.create (module String);;

let add (tags: t) (article: Article.t) =
  let rec add_to_lists = function
  | [] -> ()
  | tag :: xs -> (
    Hashtbl.set tags ~key:tag ~data:(Tag.add (Hashtbl.find_or_add tags tag ~default:(fun () -> Tag.make ())) article);
    add_to_lists xs;
  )
  in add_to_lists article.tags
;;
