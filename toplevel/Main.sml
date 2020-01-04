structure Main = struct
    fun main (arg0: string, args: string list) =  (
        case args of
            [] => Roll.interpreter(NONE)
        |   cmd =>
        let val command = List.foldl
          (fn (a, b) => if b = "" then a else b ^ " " ^ a)
          ""
          cmd
        in Roll.interpreter(SOME command)
        end;

	OS.Process.success
    )

    val _ = SMLofNJ.exportFn("roll", main)
end
