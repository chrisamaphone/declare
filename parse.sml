signature PARSER =
sig
  val parse : string -> Game.command
end

structure Parser : PARSER =
struct
open Game
open Tokens

  fun isWhitespace c = (Char.ord c) < 33

  fun split [] = NONE
    | split (x::xs) = SOME (x, xs)

  
  fun merge words = String.concatWith " " words

  fun no_arg_verb token command args =
    case args of
         [] => command
       | _ => PARTIAL_GARBAGE(token)

  fun one_arg_verb token command args = 
    case args of 
         [] => PARTIAL_GARBAGE(token)
       | _ => command (merge args)

  fun parse_examine args = one_arg_verb T_EXAMINE EXAMINE args
  fun parse_take args = one_arg_verb T_TAKE TAKE args
  fun parse_open args = one_arg_verb T_OPEN OPEN args
  fun parse_read args = one_arg_verb T_READ READ args
  fun parse_close args = one_arg_verb T_CLOSE CLOSE args
  fun parse_drop args = one_arg_verb T_DROP DROP args
  fun parse_push args = one_arg_verb T_PUSH PUSH args
  fun parse_pull args = one_arg_verb T_PULL PULL args
  fun parse_wear args = one_arg_verb T_WEAR WEAR args
  fun parse_doff args = one_arg_verb T_DOFF DOFF args
  fun parse_eat args = one_arg_verb T_EAT EAT args
  fun parse_drink args = one_arg_verb T_DRINK DRINK args
  fun parse_enter args = one_arg_verb T_ENTER ENTER args

  fun parse_quit args = no_arg_verb T_QUIT QUIT args
  fun parse_help args = no_arg_verb T_HELP HELP args
  fun parse_look args = no_arg_verb T_LOOK LOOK args
  fun parse_inventory args = no_arg_verb T_INVENTORY INVENTORY args
  fun parse_wait args = no_arg_verb T_WAIT WAIT args

  fun direction token =
    case token of
         T_NORTH => SOME N
       | T_SOUTH => SOME S
       | T_EAST => SOME E
       | T_WEST => SOME W
       | T_NORTHEAST => SOME NE
       | T_NORTHWEST => SOME NW
       | T_SOUTHEAST => SOME SE
       | T_SOUTHWEST => SOME SW
       | T_UP => SOME Up
       | T_DOWN => SOME Down
       | _ => NONE

  fun parse_go args =
    case args of
         [] => PARTIAL_GARBAGE T_GO
       | [x] => 
           (case (SynTable.find (tokenTable, x)) of
                 NONE => PARTIAL_GARBAGE T_GO
               | SOME token => 
                   (case (direction token) of
                         SOME dir => GO dir
                       | NONE => PARTIAL_GARBAGE T_GO))
       | _ => PARTIAL_GARBAGE T_GO

  fun parse_dir dir_t dir args = no_arg_verb dir_t (GO dir) args

  fun parse_put args = 
  let
    fun split_prep l prefix =
      case l of
           [] => PARTIAL_GARBAGE T_PUT
         | (x::xs) => (case (SynTable.find (tokenTable, x)) of
                           SOME T_IN => 
                            PUT_IN (merge (rev prefix), merge xs)
                         | SOME T_ON => 
                            PUT_ON (merge (rev prefix), merge xs)
                         | _ => split_prep xs (x::prefix))
  in
    split_prep args []
  end

 
  fun parseAction (first, rest) =
    case (SynTable.find (tokenTable, first)) of
         NONE => GARBAGE (merge (first::rest))
       | SOME t => (
          case t of
               T_QUIT => parse_quit rest
             | T_HELP => parse_help rest
             | T_LOOK => parse_look rest
             | T_EXAMINE => parse_examine rest
             | T_TAKE => parse_take rest
             | T_OPEN => parse_open rest
             | T_READ => parse_read rest
             | T_GO => parse_go rest
             | T_NORTH => parse_dir T_NORTH N rest
             | T_SOUTH => parse_dir T_SOUTH S rest
             | T_WEST => parse_dir T_WEST W rest
             | T_EAST => parse_dir T_EAST E rest
             | T_NORTHEAST => parse_dir T_NORTHEAST NE rest
             | T_NORTHWEST => parse_dir T_NORTHWEST NW rest
             | T_SOUTHEAST => parse_dir T_SOUTHEAST SE rest
             | T_SOUTHWEST => parse_dir T_SOUTHWEST SW rest
             | T_UP => parse_dir T_UP Up rest
             | T_DOWN => parse_dir T_DOWN Down rest
             | T_INVENTORY => parse_inventory rest
             | T_CLOSE => parse_close rest
             | T_DROP => parse_drop rest
             | T_WAIT => parse_wait rest
             | T_PUSH => parse_push rest
             | T_PULL => parse_pull rest
             | T_PUT => parse_put rest
             | T_WEAR => parse_wear rest
             | T_DOFF => parse_doff rest
             (* | ...attack... *)
             | T_EAT => parse_eat rest
             | T_DRINK => parse_drink rest
             | T_ENTER => parse_enter rest
             | T_YES => YES
             | T_NO => NO
       )

    (*
    if (isLook verb) then
      (case args of
            [] => LOOK
          | _ => GARBAGE)
    else if (isExamine verb) then
      (case args of
            [x] => (case (lookupObject x) of
                         SOME i => EXAMINE i
                       | NONE => GARBAGE)
          | _ => GARBAGE)
    else
      GARBAGE
    *)

  fun parse s =
  let
    val tokens = String.tokens isWhitespace s
    val parts = split tokens 
  in
    case parts of
         NONE => GARBAGE s
       | SOME (first, rest) => parseAction (first, rest)
  end

  (* TODO: normalize captilization *)
  
end
