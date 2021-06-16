type t = Article.t list

let make () = []
let add (list_: t) (article: Article.t) = article :: list_ ;;
