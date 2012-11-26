
structure Prompt =
struct

  open TextIO

  val prompt0 = Game.try_look ()

  fun quitloop quitprompt gameprompt =
  let
    val () = print quitprompt
    val () = print "\n"
    val answerString = valOf (inputLine stdIn)
    val answer = Parser.parse answerString
  in
    case answer of
         Game.YES => ()
       | Game.NO => loop gameprompt
       | _ => 
           quitloop "Sorry, I didn't understand. Do you want to quit?" gameprompt
  end

  and loop prompt =
  let
    val () = print prompt
    val () = print "\n> "
    val commandString = valOf (inputLine stdIn)
    val command = Parser.parse commandString
    val reply = Game.reply command
  in
    loop reply
  end
  handle Game.Quit => quitloop "Do you really want to quit?" prompt
       | _ => loop "Wha?"

  fun main () = loop prompt0
end
