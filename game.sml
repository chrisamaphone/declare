(*
*
* TODO: 
* - debug eating something in your inventory
* - context-(room-)sensitive object naming
* - factor out game-specific logic and write a new example
*)

structure Game =
struct
  type object = string

  datatype dir = N | S | E | W | NE | NW | SE | SW | Up | Down


  datatype command =
    GARBAGE of string
  | PARTIAL_GARBAGE of Tokens.token
  | QUIT
  | HELP
  | LOOK
  | EXAMINE of object
  | TAKE of object
  | OPEN of object
  | READ of object
  | GO of dir
  | INVENTORY
  | CLOSE of object
  | DROP of object
  | WAIT
  | PUSH of object
  | PULL of object
  | PUT_ON of object * object (* put OBJECT on SUPPORTER *)
  | PUT_IN of object * object (* put OBJECT in CONTAINER *)
  | WEAR of object
  | DOFF of object
  | ATTACK of object * (object option) (* parse SUBJECT (with WEAPON) *)
  | EAT of object
  | DRINK of object
  | ENTER of object
  | YES
  | NO


  (* Things the game world knows about. *)
  datatype thing =
      WestOfHouse
    | Woods
    | House
    | Mailbox
    | Leaflet
    | Mushroom
    (* | ... *)

  val syns = [
        (Woods, ["woods", "trees", "forest"]),
        (House, ["house"]),
        (Mailbox, ["mailbox"]),
        (Leaflet, ["leaflet"]),
        (Mushroom, ["mushroom", "shroom"])
      ]

  (* Build a table mapping all object synyonyms to their thing *)
  val thingTable =
    let
      fun addSyns ((thing, syns), table) =
      let
        fun add (key, table) = SynTable.insert (table, key, thing)
      in
        foldl add table syns
      end
    in
      foldl addSyns (SynTable.empty) syns
    end

  fun thingToString thing =
  let
    fun findin ((t,(syn0::_))::rest) = if t = thing then syn0 else findin rest
  in
    findin syns
  end

  exception Quit

  fun dirToString dir =
    case dir of
         N => "north"
       | S => "south"
       | E => "east"
       | W => "west"
       | NE => "northeast"
       | SE => "southeast"
       | SW => "southwest"
       | NW => "northwest"
       | Up => "up"
       | Down => "down"


  (* State *)
  structure State =
  struct
    val room : thing ref = ref WestOfHouse
    val inventory : thing list ref = ref nil
    val score : int ref = ref 0

    (* Object state *)
    datatype container_state = OPEN | CLOSED
    datatype switch_state = ON | OFF
    (* etc *)

    datatype object_state = 
        CONTAINER of container_state ref
      | SWITCH of switch_state ref
      | BORING (* none of the above *)


    (* configuration of objects in the world *)
    datatype matryoshka = 
       Node of 
         {data:thing, 
          state:object_state, 
          children: matryoshka list ref,
          parent:matryoshka ref}
     | Root of matryoshka list ref (* top-level world *)
     | Player (* children are inventory *)

     (*
    fun modifyChildren f (Node {children=r, ...}) =
          r := f (!r)
          (*
      | modifyChildren f (Root r) =
          r := f (!r) 
          *)
    *)

    fun removeChild toRemove (m as Node {children=r, ...}) =
     (*  modifyChildren (List.filter (fn (Node {data=d,...}) => not (d=x))) *)
     let
       val l = !r
       fun f (Node {data=d,...}) = d=toRemove
       fun separate f [] prefix = (NONE, prefix nil)
         | separate f (y::ys) prefix = if (f y) then (SOME y, prefix ys)
            else separate f ys (fn l => prefix (y::l))
     in
       case (separate f l (fn x => x)) of
            (NONE, _) => false
          | (SOME (Node {parent=p,...}), l') => (r := l'; p := Player; true)
          (* Make the new parent the player. XXX parameterize? *)
     end

     (* remove x from !r; remove m from its parents *)

    fun addToChildren x (Node {children=r, ...}) =
      r := x::(!r)

    fun addToInventory x =
      inventory := x::(!inventory)

    fun close r =
      case (!r) of
          CLOSED => "That's already closed."
        | OPEN => (r := CLOSED; "Closed.")

    fun open_ r =
      case (!r) of
           OPEN => "That's already open."
         | CLOSED => (r := OPEN; "Opened.")

    val rooms = ref (nil : matryoshka list)
    val world = Root rooms
    val woods = Node {data=Woods, state=BORING, children=ref nil, parent=ref world}
    val house = Node {data=House, state=BORING, children=ref nil, parent=ref world}
    val westOfHouse = 
      Node {data=WestOfHouse, state=BORING, children=ref nil, parent=ref world}
    val mailbox =
      Node {data=Mailbox, 
            state=CONTAINER(ref CLOSED), 
            children=ref nil, 
            parent=ref westOfHouse}
    val leaflet = 
      Node {data=Leaflet, state=BORING, children=ref nil, parent=ref mailbox}
    val mushroom = 
      Node {data=Mushroom, state=BORING, children=ref nil, parent=ref woods}

    val () = rooms := [woods, house, westOfHouse]
    val () = addToChildren mailbox westOfHouse
    val () = addToChildren mushroom woods
    val () = addToChildren leaflet mailbox

    fun mapping thing =
      case thing of
           Woods => woods
         | House => house
         | WestOfHouse => westOfHouse
         | Mailbox => mailbox
         | Leaflet => leaflet
         | Mushroom => mushroom

    fun getData (Node {data=d, ...}) = d
    fun getChildren (Node {children=r, ...}) = !r
    fun getState (Node {state=s, ...}) = s


    fun is_open thing =
    let
      val Node {state = s, ...} = mapping thing
    in
      case s of
           CONTAINER r => !r = OPEN
         | _ => false
    end

    fun stateToString s =
      case s of
           CONTAINER r => (case (!r) of OPEN => "open" | CLOSED => "closed")
         | SWITCH r => (case (!r) of OFF => "off" | ON => "on")
         | BORING => "none"

    (* contents : thing -> thing list *)
    val contents = (map getData) o getChildren o mapping
  end

  (* Flags. Configure by setting different data values. *)
  structure Flags =
  struct
    datatype flag = ON | OFF
    val print_objects_in_room = ON
  end


  (* Thing attributes *)

  fun dir_of N WestOfHouse = SOME Woods
    | dir_of E WestOfHouse = SOME House
    | dir_of S Woods = SOME WestOfHouse
    | dir_of W House = SOME WestOfHouse
    | dir_of _ _ = NONE

  fun description thing =
    (case thing of
         WestOfHouse => 
         "You are in a field west of a white house."
       | Woods => "You are in woods."
       | House => "You are in a house."
       | Mailbox =>
           let
             val state = State.getState (State.mapping Mailbox)
             val stateString = State.stateToString state
           in
             "The mailbox is "^stateString^"."
           end
       | Leaflet => "The leaflet reads, \"Welcome to the game.\""
       | Mushroom => "The mushroom looks tasty.")

  fun portable Leaflet = true
    | portable Mushroom = true
    | portable _ = false

  fun edible Mushroom = true
    | edible _ = false

  (* is thing visible? *)
  fun visible thing =
  let
    val roomContents = State.contents (!State.room)
  in
    (List.exists (fn x => x = thing) roomContents)
    orelse
    (List.exists
      (fn x => visible x 
        andalso (visibleFrom thing x)) 
    roomContents)
    orelse
    List.exists (fn x => visibleFrom thing x) (!State.inventory)
  end

  (* is inner visible from within outer? *)
  and visibleFrom inner outer = inner = outer orelse (
  (* inner must be contents of outer, and outer must be open *)
  State.is_open outer andalso
  let
    val outerContents = State.contents outer
  in
    (List.exists (fn x => x = inner) outerContents)
    orelse
    (List.exists (fn x => visibleFrom x outer andalso (visibleFrom inner x))
    outerContents)
  end)

  fun resolve obj = 
    case SynTable.find (thingTable, obj) of
         NONE => NONE
       | SOME thing =>
          if (visible thing) then SOME thing
           else NONE

  fun contentsToString thing =
    String.concatWith ", " (map thingToString (State.contents thing))



  fun try_look () =
  let
    val r = !State.room 
  in
    description r
    ^
    (case Flags.print_objects_in_room of
          Flags.ON => "\nContents: "^(contentsToString r)
        | Flags.OFF => "")
  end

  fun try_go dir =
  let
    val r = !State.room
  in
    case (dir_of dir r) of
         NONE => "You can't go that way."
       | SOME r' => (State.room := r'; try_look ())
  end

  (* XXX in these try_functions, factor out option logic *)
  fun try_examine obj =
  let
    val thingmaybe = resolve obj
  in
    case thingmaybe of
         NONE => "Sorry, I don't see that here."
       | SOME thing => description thing
    ^
    (if State.is_open thing then
      "\nContents: "^
      (contentsToString thing)
      else ""
    )
  end

  fun try_open obj =
  let
    val thingmaybe = resolve obj
  in
    case thingmaybe of
         NONE => "Sorry, I don't see that here." (* XXX name object *)
       | SOME thing =>
           let
             val State.Node {state=s, ...} = State.mapping thing
           in
             case s of
                  State.CONTAINER r => (State.open_ r) ^ " Contents: " ^
                    (contentsToString thing)
                | _ => "That can't be opened."
           end
  end

  fun try_take obj =
  let
    val thingmaybe = resolve obj
  in
    case thingmaybe of
         NONE => "Sorry, I don't see that here."
       | SOME thing =>
          if (portable thing) then
            (* Move it in the object tree *)
            let
              val m = State.mapping thing 
              val State.Node {parent = ref p, ...} = m
              val success = State.removeChild thing p
            in (* check that it's actually there *)
              if success then
              (State.addToInventory thing; "Taken")
              else
                "I don't see that there."
          end
          else "That's hardly portable."
  end

  fun report_eat thing =
    case thing of
         Mushroom => "You eat the mushroom. Nom."
       | _ => "You scarf it down."

  fun try_eat obj failmsg =
  let
    val thingmaybe = resolve obj
  in
    case thingmaybe of
         NONE => failmsg
       | SOME thing =>
           if (edible thing) then
             let
               val m = State.mapping thing
               val State.Node {parent = ref p, ...} = m
               val success = State.removeChild thing p
             in
               if success then
                 report_eat thing
               else
                 failmsg
             end
          else
            "That's plainly inedible."
  end
   
  val fail_notthere = "I don't see that there."

  fun reply command =
    case command of
         GARBAGE s => 
          (case s of "\n" => "Sorry?" 
              | _ => "sorry, i don't know what \""^s^"\" means.")
       | PARTIAL_GARBAGE t => "I only understood you as far as..."
          (* eventually, ^ token_to_string t *)
       | QUIT => raise Quit 
       | LOOK => try_look ()
       | EXAMINE obj => try_examine obj
       | INVENTORY => "You are carrying"^
           (case (! State.inventory) of
                nil => " nothing."
              | inv => ":\n" ^ String.concatWith "\n" (map thingToString inv))
       | GO dir => try_go dir
       | OPEN obj => try_open obj
       | TAKE obj => try_take obj
       | PUT_IN (a, b) => "you want to put "^a^" in "^b^"."
       | PUT_ON (a, b) => "you want to put "^a^" on "^b^"."
       | EAT obj => try_eat obj fail_notthere
       | _ => "can't do that yet"

end
