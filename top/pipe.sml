(* I stole all this code from StackOverflow. *)
structure Popen :>
      sig
          (* Parent wants to write to stdin, read stdout, or read stdout + stderr *)
          datatype pipe_type = PIPE_W | PIPE_R | PIPE_RE
          val popen : string * pipe_type -> Posix.IO.file_desc
          val pclose : Posix.IO.file_desc -> Posix.Process.exit_status option
      end =
struct
  datatype pipe_type = PIPE_W | PIPE_R | PIPE_RE

  type pinfo = { fd : Posix.ProcEnv.file_desc, pid : Posix.Process.pid }

  val pids : pinfo list ref = ref []

  (* Implements popen(3) *)
  fun popen (cmd, t) =
    let val { infd = readfd, outfd = writefd } = Posix.IO.pipe ()
    in case (Posix.Process.fork (), t)
        of (NONE, t) => (* Child *)
       (( case t
           of PIPE_W => Posix.IO.dup2 { old = readfd, new = Posix.FileSys.stdin }
            | PIPE_R => Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stdout }
            | PIPE_RE => ( Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stdout }
                         ; Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stderr })
        ; Posix.IO.close writefd
        ; Posix.IO.close readfd
        ; Posix.Process.execp ("/bin/sh", ["sh", "-c", cmd]))
        handle OS.SysErr (err, _) =>
               ( print ("Fatal error in child: " ^ err ^ "\n")
               ; OS.Process.exit OS.Process.failure ))
         | (SOME pid, t) => (* Parent *)
       let val fd = case t of PIPE_W => (Posix.IO.close readfd; writefd)
                            | PIPE_R => (Posix.IO.close writefd; readfd)
                            | PIPE_RE => (Posix.IO.close writefd; readfd)
           val _ = pids := ({ fd = fd, pid = pid } :: !pids)
       in fd end
    end

  (* Implements pclose(3) *)
  fun pclose (fd : Posix.IO.file_desc) =
    case List.partition (fn { fd = f, pid = _ } => f = fd) (!pids)
     of ([], _) => NONE
      | ([{ fd = _, pid = pid }], pids') =>
        let val _ = pids := pids'
        val (_, status) = Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
        val _ = Posix.IO.close fd
        in SOME status end
      | _ => raise Bind (* This should be impossible. *)
end

