structure Main = struct
    fun main (arg0: string, args: string list ) =  (
        case args of
            [] => Roll.interpreter(NONE)
        |   [name] => Roll.interpreter(SOME name)
        |   _ => print "Usage: roll [filename]\n";

	OS.Process.success
    )

    val _ = SMLofNJ.exportFn("roll", main)
end
