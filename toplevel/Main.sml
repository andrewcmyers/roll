structure Main = struct
    fun main (arg0: string, args: string list ) =  (
	Roll.interpreter();
	OS.Process.success
    )

    val _ = SMLofNJ.exportFn("roll", main)
end
