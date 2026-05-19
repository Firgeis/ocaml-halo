type media_type = {
  type_ : string;
  subtype : string;
  parameters : (string * string) list;
}

val header : Router.request -> string -> string option
val headers : Router.request -> string -> string list
val content_type : Router.request -> media_type option
