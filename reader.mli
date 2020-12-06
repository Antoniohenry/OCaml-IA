type direction = Horizontal | Vertical
type variable = {
  id : int;
  coord : int * int;
  length : int;
  direction : direction;
  mutable crossing : int list;
}
val print_words : variable list -> unit
val print_grid : string -> unit

val get_words_from_txt : string -> variable list
