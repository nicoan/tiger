open lexer
open parser
open escape
open seman
open codegen
open flow
open frame
open assem
open BasicIO Nonstdio

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun errParsing(lbuf) = (print("Error en parsing!("
        ^(makestring(!num_linea))^
        ")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun concatWith c l = List.foldl (fn (x, xs) => x ^ c ^ xs) "" l

fun getNameFromPath path = 
  let 
    val splittedName = (String.tokens (fn c => c = #"/") path)
    val fileName = List.nth (splittedName, (List.length splittedName) - 1)
  in
    List.nth (String.tokens (fn c => c = #".") fileName, 0)
  end

fun main(args) =
    let	fun arg(l, s) =
          (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
      val (asm, l1)		= arg(args, "-asm")
      val (arbol, l2)		= arg(l1, "-arbol")

      val usage = "Modo de uso:\n./tigerc [-arbol] [-asm] FILE.tig\n    -arbol:\tImprime el AST.\n    -asm:\nDevuelve unicamente el código assembler."

      val (entrada, fileName) =
        case l2 of
          [n] => ((open_in n, getNameFromPath n)
              handle _ => raise Fail (n^" no existe!"))
        | _ => raise Fail usage

      val lexbuf = lexstream entrada

      val expr = prog Tok lexbuf handle _ => errParsing lexbuf

      val _ = findEscape(expr)

      val _ = if arbol then prettyprinter.exprAst expr else ()

      val _ = transProg(expr)

      fun name (frame.PROC{body, frame}) = frame.name(frame)
        | name _ = raise Fail "Error interno (name): no es PROC"
      val frags = translate.getResult() (* lista de fragmentos *)
      val func_frags = let fun isFunc (frame.PROC _) = true
            | isFunc _ = false
        in
          List.filter isFunc frags
        end

      val str_frags = let fun isStr (frame.STRING _) = true
            | isStr _ = false
          fun strip (frame.STRING f) = f
            | strip _ = raise Fail "Error interno (strip): no es STRING"
        in
          List.map strip (List.filter isStr frags)
        end

      fun remRedundantMoves [] = []
        | remRedundantMoves ((i as MOVE {assem=a, dst=d, src=s})::is) = if d = s then remRedundantMoves is
          else (i::remRedundantMoves is)
        | remRedundantMoves (i::is) = (i::remRedundantMoves is)


      fun canonizar n = canon.traceSchedule o (canon.basicBlocks n) o canon.linearize
      
      fun canon_frag (p as frame.PROC {body, frame}) = (canonizar (name p) body, frame)
        | canon_frag _ = raise Fail "Error interno (canon_frag): no es proc"

      val canon_frags = List.map canon_frag func_frags

      val instrlist = let
          fun aplanar (x, frame) = List.map (fn y => (frame, y)) x

          fun applyCodeGen stmList frame = List.map (fn x => codegen.codegen frame x) stmList
          val assemsBlocks = List.map (fn (x, y) => (applyCodeGen x y , y)) canon_frags
          val plainAssemsBlocks = List.map (fn (x, y) => (List.concat x, y, true)) assemsBlocks
          val precoloredCode = List.map coloring.coloring plainAssemsBlocks
          val coloredCode = List.map (fn (i, f, c) => (coloring.replaceTforColors i c, f)) precoloredCode
          val procExitedCode = List.map (fn (x, y) => (frame.procEntryExit3 (y, x), y)) coloredCode

          val colprint = map remRedundantMoves (List.map (fn (x,y) => x) procExitedCode)
          val stringSection = map frame.string str_frags
          val globlSection = map frame.globl (List.map name func_frags)
          val codeSection = map (assem.strAssem) (List.concat colprint)

          val allProgram = String.concat ([".data\n"] @ stringSection @ [".text\n\t.globl _main\n"] @ codeSection)

          (* Pasamos el assembler a un archivo y lo linkeamos con gcc *)
          val assemblerFileName = (fileName ^ ".s")
          val fd = TextIO.openOut assemblerFileName
          val _ = TextIO.output(fd, allProgram)
          val _ = TextIO.closeOut fd
          val _ = 
            if not asm then 
              let 
                val _ = Process.system("gcc -m32 -g runtime.o " ^ assemblerFileName ^ " -o " ^ fileName ^ " && rm " ^ assemblerFileName) 
              in 
                ()
              end
            else 
              ()
        in
          ()
        end
    in
      print "Compilacion terminada\n"
    end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
