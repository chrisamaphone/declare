(*
*
*
* TODO: have the token table return a *list* (or set) of meanings for ambiguous
* words.
*)

structure StringOrd : ORD_KEY =
struct
  type ord_key = string
  val compare = String.compare
end

structure SynTable =  BinaryMapFn(StringOrd)

structure Tokens =
struct
datatype token =
    T_QUIT
  | T_HELP
  | T_LOOK
  | T_EXAMINE
  | T_TAKE
  | T_OPEN
  | T_READ
  | T_GO
  | T_NORTH
  | T_SOUTH
  | T_EAST
  | T_WEST
  | T_NORTHEAST
  | T_NORTHWEST
  | T_SOUTHEAST
  | T_SOUTHWEST
  | T_UP
  | T_DOWN
  | T_INVENTORY
  | T_CLOSE
  | T_DROP
  | T_WAIT
  | T_PUSH
  | T_PULL
  | T_PUT
  | T_ON
  | T_IN
  | T_WEAR
  | T_DOFF
  | T_ATTACK
  | T_EAT
  | T_DRINK
  | T_ENTER
  | T_YES
  | T_NO

  val syns = [
    (T_QUIT, ["quit", "q"]),
    (T_HELP, ["help", "h"]),
    (T_LOOK, ["look", "l"]),
    (T_EXAMINE, ["examine", "x"]),
    (T_TAKE, ["take", "get"]),
    (T_OPEN, ["open"]),
    (T_READ, ["read"]),
    (T_GO, ["go"]),
    (T_NORTH, ["north", "n"]),
    (T_SOUTH, ["south", "s"]),
    (T_EAST, ["east", "e"]),
    (T_WEST, ["west", "w"]),
    (T_NORTHEAST, ["northeast", "ne"]),
    (T_NORTHWEST, ["northwest", "nw"]),
    (T_SOUTHEAST, ["southeast", "se"]),
    (T_SOUTHWEST, ["southwest", "sw"]),
    (T_UP, ["up", "u"]),
    (T_DOWN, ["down", "d"]),
    (T_INVENTORY, ["inventory", "i", "inv", "ls"]),
    (T_CLOSE, ["close"]),
    (T_PUT, ["put", "set", "place", "insert"]),
    (T_IN, ["in", "into"]),
    (T_ON, ["on", "onto"]),
    (T_DROP, ["drop", "throw"]),
    (T_WAIT, ["wait", "z"]),
    (T_PUSH, ["push", "shove", "press"]),
    (T_PULL, ["pull", "tug", "jerk", "yank"]),
    (T_WEAR, ["wear", "don"]),
    (T_DOFF, ["doff", "remove", "shed"]),
    (T_ATTACK, ["attack", "kick", "punch", "hit", "kill"]),
    (T_EAT, ["eat", "swallow"]),
    (T_DRINK, ["drink"]),
    (T_YES, ["yes", "y"]),
    (T_NO, ["no"]) (* oops n clashes with north *)
    ]

  (* Build a table mapping all the synonyms to their token *)

  val tokenTable =
    let
      fun addSyns ((token, syns), table) = 
        let
          fun add (key, table) = SynTable.insert (table, key, token)
        in
          foldl add table syns
        end
    in
      foldl addSyns (SynTable.empty) syns
    end

end
