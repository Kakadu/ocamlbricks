
(** Additional features for the standard library [Unix]. 
    Open this module in order to use the extended version of [Unix] instead of
    the standard one. *)

module Extra :
  sig
    type filename   = string
    type foldername = string
    type content    = string
    val current_umask : int
    val touch : ?perm:Unix.file_perm -> filename -> unit

    (** {2 Copying files} *)

    module Copylib :
      sig
        val buffer_size : int
        val buffer : string
        val file_copy :
          ?perm:Unix.file_perm ->
          ?flag:Unix.open_flag -> string -> string -> unit
      end

    val file_copy : ?perm:Unix.file_perm -> filename -> filename -> unit
    val file_append : ?perm:Unix.file_perm -> filename -> filename -> unit

    (** {2 Saving strings} *)

    val put : ?perm:Unix.file_perm -> filename -> content -> unit
    val rewrite : ?perm:Unix.file_perm -> filename -> content -> unit
    val append : ?perm:Unix.file_perm -> filename -> content -> unit

    (** {2 Loading strings} *)

    val cat : filename -> string

    (** {2 Temporary files} *)

    module Templib :
      sig
        val temp_name :
          dir:bool ->
          perm:Unix.file_perm ->
          parent:string -> prefix:string -> suffix:string -> unit -> string
      end

    val temp_dir :
      ?perm:Unix.file_perm ->
      ?parent:string -> ?prefix:string -> ?suffix:string -> unit -> string

    val temp_file :
      ?perm:Unix.file_perm ->
      ?parent:string ->
      ?prefix:string -> ?suffix:string -> ?content:content -> unit -> string

    module TMPDIR :
      sig
       val temp_file : ?perm:Unix.file_perm -> ?prefix:string -> ?suffix:string -> unit -> string
       val open_temp : ?perm:Unix.file_perm -> ?prefix:string -> ?suffix:string -> unit -> string * Unix.file_descr
      end

    (** {2 File kind} *)

    val file_kind_of_char : char -> Unix.file_kind option

    (** {2 Directories} *)

    val iter_dir : (string -> 'a) -> string -> unit

    (** {3 Find} *)

    module Findlib :
      sig
        exception Hidden of exn
        val hide_exn : ('a -> 'b) -> 'a -> 'b
        val reveal_exn : ('a -> 'b) -> 'a -> 'b
        val find :
          (Unix.error * string * string -> unit) ->
          (string -> Unix.stats -> bool) ->
          bool -> int -> string list -> unit
      end

    val find :
      ?follow:bool ->
      ?maxdepth:int -> ?kind:char -> ?name:string -> string -> string list

    (** {2 Password} *)

    module Passwdlib : sig val read_passwd : string -> string end
    val read_passwd : string -> string

    (** {2 Process status printers} *)

    module Process_status :
      sig
        type t = Unix.process_status
        val string_of : t -> string
        val print : Unix.process_status -> unit
        val prerr : Unix.process_status -> unit
        val print_endline : Unix.process_status -> unit
        val prerr_endline : Unix.process_status -> unit
        val fprintf :
          out_channel ->
          (string -> 'a, out_channel, unit) format ->
          Unix.process_status -> 'a
        val eprintf :
          (string -> 'a, out_channel, unit) format ->
          Unix.process_status -> 'a
        val printf :
          (string -> 'a, out_channel, unit) format ->
          Unix.process_status -> 'a
        val sprintf :
          (string -> 'a, unit, string) format -> Unix.process_status -> 'a
      end

    (** {2 Running} *)

    type command = string
    type program = string

    val kill_safe : int -> int -> unit

    exception Signal_forward of int
    exception Waitpid

    val create_process_and_wait :
          ?stdin:Endpoint.Source.t ->
          ?stdout:Endpoint.Sink.t ->
          ?stderr:Endpoint.Sink.t ->
          ?pseudo:string option ->
          ?forward:int list -> program -> string list -> int

    val run :
      ?shell:program ->
      ?trace:bool ->
      ?input:content -> command -> string * Unix.process_status

    val shell : ?shell:program -> ?trace:bool -> ?input:content -> command -> string
  end

module Unix :
  sig
    type error =
      Unix.error =
        E2BIG
      | EACCES
      | EAGAIN
      | EBADF
      | EBUSY
      | ECHILD
      | EDEADLK
      | EDOM
      | EEXIST
      | EFAULT
      | EFBIG
      | EINTR
      | EINVAL
      | EIO
      | EISDIR
      | EMFILE
      | EMLINK
      | ENAMETOOLONG
      | ENFILE
      | ENODEV
      | ENOENT
      | ENOEXEC
      | ENOLCK
      | ENOMEM
      | ENOSPC
      | ENOSYS
      | ENOTDIR
      | ENOTEMPTY
      | ENOTTY
      | ENXIO
      | EPERM
      | EPIPE
      | ERANGE
      | EROFS
      | ESPIPE
      | ESRCH
      | EXDEV
      | EWOULDBLOCK
      | EINPROGRESS
      | EALREADY
      | ENOTSOCK
      | EDESTADDRREQ
      | EMSGSIZE
      | EPROTOTYPE
      | ENOPROTOOPT
      | EPROTONOSUPPORT
      | ESOCKTNOSUPPORT
      | EOPNOTSUPP
      | EPFNOSUPPORT
      | EAFNOSUPPORT
      | EADDRINUSE
      | EADDRNOTAVAIL
      | ENETDOWN
      | ENETUNREACH
      | ENETRESET
      | ECONNABORTED
      | ECONNRESET
      | ENOBUFS
      | EISCONN
      | ENOTCONN
      | ESHUTDOWN
      | ETOOMANYREFS
      | ETIMEDOUT
      | ECONNREFUSED
      | EHOSTDOWN
      | EHOSTUNREACH
      | ELOOP
      | EOVERFLOW
      | EUNKNOWNERR of int
    exception Unix_error of error * string * string
    val error_message : error -> string
    val handle_unix_error : ('a -> 'b) -> 'a -> 'b
    val environment : unit -> string array
    val getenv : string -> string
    val putenv : string -> string -> unit
    type process_status =
      Unix.process_status =
        WEXITED of int
      | WSIGNALED of int
      | WSTOPPED of int
    type wait_flag = Unix.wait_flag = WNOHANG | WUNTRACED
    val execv : string -> string array -> 'a
    val execve : string -> string array -> string array -> 'a
    val execvp : string -> string array -> 'a
    val execvpe : string -> string array -> string array -> 'a
    val fork : unit -> int
    val wait : unit -> int * process_status
    val waitpid : wait_flag list -> int -> int * process_status
    val system : string -> process_status
    val getpid : unit -> int
    val getppid : unit -> int
    val nice : int -> int
    type file_descr = Unix.file_descr
    val stdin : file_descr
    val stdout : file_descr
    val stderr : file_descr
    type open_flag =
      Unix.open_flag =
        O_RDONLY
      | O_WRONLY
      | O_RDWR
      | O_NONBLOCK
      | O_APPEND
      | O_CREAT
      | O_TRUNC
      | O_EXCL
      | O_NOCTTY
      | O_DSYNC
      | O_SYNC
      | O_RSYNC
    type file_perm = int
    val openfile : string -> open_flag list -> file_perm -> file_descr
    val close : file_descr -> unit
    val read : file_descr -> string -> int -> int -> int
    val write : file_descr -> string -> int -> int -> int
    val single_write : file_descr -> string -> int -> int -> int
    val in_channel_of_descr : file_descr -> in_channel
    val out_channel_of_descr : file_descr -> out_channel
    val descr_of_in_channel : in_channel -> file_descr
    val descr_of_out_channel : out_channel -> file_descr
    type seek_command = Unix.seek_command = SEEK_SET | SEEK_CUR | SEEK_END
    val lseek : file_descr -> int -> seek_command -> int
    val truncate : string -> int -> unit
    val ftruncate : file_descr -> int -> unit
    type file_kind =
      Unix.file_kind =
        S_REG
      | S_DIR
      | S_CHR
      | S_BLK
      | S_LNK
      | S_FIFO
      | S_SOCK
    type stats =
      Unix.stats = {
      st_dev : int;
      st_ino : int;
      st_kind : file_kind;
      st_perm : file_perm;
      st_nlink : int;
      st_uid : int;
      st_gid : int;
      st_rdev : int;
      st_size : int;
      st_atime : float;
      st_mtime : float;
      st_ctime : float;
    }
    val stat : string -> stats
    val lstat : string -> stats
    val fstat : file_descr -> stats
    val isatty : file_descr -> bool
    module LargeFile :
      sig
        val lseek : file_descr -> int64 -> seek_command -> int64
        val truncate : string -> int64 -> unit
        val ftruncate : file_descr -> int64 -> unit
        type stats =
          Unix.LargeFile.stats = {
          st_dev : int;
          st_ino : int;
          st_kind : file_kind;
          st_perm : file_perm;
          st_nlink : int;
          st_uid : int;
          st_gid : int;
          st_rdev : int;
          st_size : int64;
          st_atime : float;
          st_mtime : float;
          st_ctime : float;
        }
        val stat : string -> stats
        val lstat : string -> stats
        val fstat : file_descr -> stats
      end
    val unlink : string -> unit
    val rename : string -> string -> unit
    val link : string -> string -> unit
    type access_permission =
      Unix.access_permission =
        R_OK
      | W_OK
      | X_OK
      | F_OK
    val chmod : string -> file_perm -> unit
    val fchmod : file_descr -> file_perm -> unit
    val chown : string -> int -> int -> unit
    val fchown : file_descr -> int -> int -> unit
    val umask : int -> int
    val access : string -> access_permission list -> unit
    val dup : file_descr -> file_descr
    val dup2 : file_descr -> file_descr -> unit
    val set_nonblock : file_descr -> unit
    val clear_nonblock : file_descr -> unit
    val set_close_on_exec : file_descr -> unit
    val clear_close_on_exec : file_descr -> unit
    val mkdir : string -> file_perm -> unit
    val rmdir : string -> unit
    val chdir : string -> unit
    val getcwd : unit -> string
    val chroot : string -> unit
    type dir_handle = Unix.dir_handle
    val opendir : string -> dir_handle
    val readdir : dir_handle -> string
    val rewinddir : dir_handle -> unit
    val closedir : dir_handle -> unit
    val pipe : unit -> file_descr * file_descr
    val mkfifo : string -> file_perm -> unit
    val create_process :
      string -> string array -> file_descr -> file_descr -> file_descr -> int
    val create_process_env :
      string ->
      string array ->
      string array -> file_descr -> file_descr -> file_descr -> int
    val open_process_in : string -> in_channel
    val open_process_out : string -> out_channel
    val open_process : string -> in_channel * out_channel
    val open_process_full :
      string -> string array -> in_channel * out_channel * in_channel
    val close_process_in : in_channel -> process_status
    val close_process_out : out_channel -> process_status
    val close_process : in_channel * out_channel -> process_status
    val close_process_full :
      in_channel * out_channel * in_channel -> process_status
    val symlink : string -> string -> unit
    val readlink : string -> string
    val select :
      file_descr list ->
      file_descr list ->
      file_descr list ->
      float -> file_descr list * file_descr list * file_descr list
    type lock_command =
      Unix.lock_command =
        F_ULOCK
      | F_LOCK
      | F_TLOCK
      | F_TEST
      | F_RLOCK
      | F_TRLOCK
    val lockf : file_descr -> lock_command -> int -> unit
    val kill : int -> int -> unit
    type sigprocmask_command =
      Unix.sigprocmask_command =
        SIG_SETMASK
      | SIG_BLOCK
      | SIG_UNBLOCK
    val sigprocmask : sigprocmask_command -> int list -> int list
    val sigpending : unit -> int list
    val sigsuspend : int list -> unit
    val pause : unit -> unit
    type process_times =
      Unix.process_times = {
      tms_utime : float;
      tms_stime : float;
      tms_cutime : float;
      tms_cstime : float;
    }
    type tm =
      Unix.tm = {
      tm_sec : int;
      tm_min : int;
      tm_hour : int;
      tm_mday : int;
      tm_mon : int;
      tm_year : int;
      tm_wday : int;
      tm_yday : int;
      tm_isdst : bool;
    }
    val time : unit -> float
    val gettimeofday : unit -> float
    val gmtime : float -> tm
    val localtime : float -> tm
    val mktime : tm -> float * tm
    val alarm : int -> int
    val sleep : int -> unit
    val times : unit -> process_times
    val utimes : string -> float -> float -> unit
    type interval_timer =
      Unix.interval_timer =
        ITIMER_REAL
      | ITIMER_VIRTUAL
      | ITIMER_PROF
    type interval_timer_status =
      Unix.interval_timer_status = {
      it_interval : float;
      it_value : float;
    }
    val getitimer : interval_timer -> interval_timer_status
    val setitimer :
      interval_timer -> interval_timer_status -> interval_timer_status
    val getuid : unit -> int
    val geteuid : unit -> int
    val setuid : int -> unit
    val getgid : unit -> int
    val getegid : unit -> int
    val setgid : int -> unit
    val getgroups : unit -> int array
    type passwd_entry =
      Unix.passwd_entry = {
      pw_name : string;
      pw_passwd : string;
      pw_uid : int;
      pw_gid : int;
      pw_gecos : string;
      pw_dir : string;
      pw_shell : string;
    }
    type group_entry =
      Unix.group_entry = {
      gr_name : string;
      gr_passwd : string;
      gr_gid : int;
      gr_mem : string array;
    }
    val getlogin : unit -> string
    val getpwnam : string -> passwd_entry
    val getgrnam : string -> group_entry
    val getpwuid : int -> passwd_entry
    val getgrgid : int -> group_entry
    type inet_addr = Unix.inet_addr
    val inet_addr_of_string : string -> inet_addr
    val string_of_inet_addr : inet_addr -> string
    val inet_addr_any : inet_addr
    val inet_addr_loopback : inet_addr
    val inet6_addr_any : inet_addr
    val inet6_addr_loopback : inet_addr
    type socket_domain = Unix.socket_domain = PF_UNIX | PF_INET | PF_INET6
    type socket_type =
      Unix.socket_type =
        SOCK_STREAM
      | SOCK_DGRAM
      | SOCK_RAW
      | SOCK_SEQPACKET
    type sockaddr =
      Unix.sockaddr =
        ADDR_UNIX of string
      | ADDR_INET of inet_addr * int
    val socket : socket_domain -> socket_type -> int -> file_descr
    val domain_of_sockaddr : sockaddr -> socket_domain
    val socketpair :
      socket_domain -> socket_type -> int -> file_descr * file_descr
    val accept : file_descr -> file_descr * sockaddr
    val bind : file_descr -> sockaddr -> unit
    val connect : file_descr -> sockaddr -> unit
    val listen : file_descr -> int -> unit
    type shutdown_command =
      Unix.shutdown_command =
        SHUTDOWN_RECEIVE
      | SHUTDOWN_SEND
      | SHUTDOWN_ALL
    val shutdown : file_descr -> shutdown_command -> unit
    val getsockname : file_descr -> sockaddr
    val getpeername : file_descr -> sockaddr
    type msg_flag = Unix.msg_flag = MSG_OOB | MSG_DONTROUTE | MSG_PEEK
    val recv : file_descr -> string -> int -> int -> msg_flag list -> int
    val recvfrom :
      file_descr -> string -> int -> int -> msg_flag list -> int * sockaddr
    val send : file_descr -> string -> int -> int -> msg_flag list -> int
    val sendto :
      file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int
    type socket_bool_option =
      Unix.socket_bool_option =
        SO_DEBUG
      | SO_BROADCAST
      | SO_REUSEADDR
      | SO_KEEPALIVE
      | SO_DONTROUTE
      | SO_OOBINLINE
      | SO_ACCEPTCONN
    type socket_int_option =
      Unix.socket_int_option =
        SO_SNDBUF
      | SO_RCVBUF
      | SO_ERROR
      | SO_TYPE
      | SO_RCVLOWAT
      | SO_SNDLOWAT
    type socket_optint_option = Unix.socket_optint_option = SO_LINGER
    type socket_float_option =
      Unix.socket_float_option =
        SO_RCVTIMEO
      | SO_SNDTIMEO
    val getsockopt : file_descr -> socket_bool_option -> bool
    val setsockopt : file_descr -> socket_bool_option -> bool -> unit
    external getsockopt_int : file_descr -> socket_int_option -> int
      = "unix_getsockopt_int"
    external setsockopt_int : file_descr -> socket_int_option -> int -> unit
      = "unix_setsockopt_int"
    external getsockopt_optint :
      file_descr -> socket_optint_option -> int option
      = "unix_getsockopt_optint"
    external setsockopt_optint :
      file_descr -> socket_optint_option -> int option -> unit
      = "unix_setsockopt_optint"
    external getsockopt_float : file_descr -> socket_float_option -> float
      = "unix_getsockopt_float"
    external setsockopt_float :
      file_descr -> socket_float_option -> float -> unit
      = "unix_setsockopt_float"
    val open_connection : sockaddr -> in_channel * out_channel
    val shutdown_connection : in_channel -> unit
    val establish_server :
      (in_channel -> out_channel -> unit) -> sockaddr -> unit
    type host_entry =
      Unix.host_entry = {
      h_name : string;
      h_aliases : string array;
      h_addrtype : socket_domain;
      h_addr_list : inet_addr array;
    }
    type protocol_entry =
      Unix.protocol_entry = {
      p_name : string;
      p_aliases : string array;
      p_proto : int;
    }
    type service_entry =
      Unix.service_entry = {
      s_name : string;
      s_aliases : string array;
      s_port : int;
      s_proto : string;
    }
    val gethostname : unit -> string
    val gethostbyname : string -> host_entry
    val gethostbyaddr : inet_addr -> host_entry
    val getprotobyname : string -> protocol_entry
    val getprotobynumber : int -> protocol_entry
    val getservbyname : string -> string -> service_entry
    val getservbyport : int -> string -> service_entry
    type addr_info =
      Unix.addr_info = {
      ai_family : socket_domain;
      ai_socktype : socket_type;
      ai_protocol : int;
      ai_addr : sockaddr;
      ai_canonname : string;
    }
    type getaddrinfo_option =
      Unix.getaddrinfo_option =
        AI_FAMILY of socket_domain
      | AI_SOCKTYPE of socket_type
      | AI_PROTOCOL of int
      | AI_NUMERICHOST
      | AI_CANONNAME
      | AI_PASSIVE
    val getaddrinfo :
      string -> string -> getaddrinfo_option list -> addr_info list
    type name_info =
      Unix.name_info = {
      ni_hostname : string;
      ni_service : string;
    }
    type getnameinfo_option =
      Unix.getnameinfo_option =
        NI_NOFQDN
      | NI_NUMERICHOST
      | NI_NAMEREQD
      | NI_NUMERICSERV
      | NI_DGRAM
    val getnameinfo : sockaddr -> getnameinfo_option list -> name_info
    type terminal_io =
      Unix.terminal_io = {
      mutable c_ignbrk : bool;
      mutable c_brkint : bool;
      mutable c_ignpar : bool;
      mutable c_parmrk : bool;
      mutable c_inpck : bool;
      mutable c_istrip : bool;
      mutable c_inlcr : bool;
      mutable c_igncr : bool;
      mutable c_icrnl : bool;
      mutable c_ixon : bool;
      mutable c_ixoff : bool;
      mutable c_opost : bool;
      mutable c_obaud : int;
      mutable c_ibaud : int;
      mutable c_csize : int;
      mutable c_cstopb : int;
      mutable c_cread : bool;
      mutable c_parenb : bool;
      mutable c_parodd : bool;
      mutable c_hupcl : bool;
      mutable c_clocal : bool;
      mutable c_isig : bool;
      mutable c_icanon : bool;
      mutable c_noflsh : bool;
      mutable c_echo : bool;
      mutable c_echoe : bool;
      mutable c_echok : bool;
      mutable c_echonl : bool;
      mutable c_vintr : char;
      mutable c_vquit : char;
      mutable c_verase : char;
      mutable c_vkill : char;
      mutable c_veof : char;
      mutable c_veol : char;
      mutable c_vmin : int;
      mutable c_vtime : int;
      mutable c_vstart : char;
      mutable c_vstop : char;
    }
    val tcgetattr : file_descr -> terminal_io
    type setattr_when = Unix.setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH
    val tcsetattr : file_descr -> setattr_when -> terminal_io -> unit
    val tcsendbreak : file_descr -> int -> unit
    val tcdrain : file_descr -> unit
    type flush_queue = Unix.flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH
    val tcflush : file_descr -> flush_queue -> unit
    type flow_action = Unix.flow_action = TCOOFF | TCOON | TCIOFF | TCION
    val tcflow : file_descr -> flow_action -> unit
    val setsid : unit -> int
    type filename = string
    type foldername = string
    type content = string
    val current_umask : int
    val touch : ?perm:Unix.file_perm -> filename -> unit
    module Copylib :
      sig
        val buffer_size : int
        val buffer : string
        val file_copy :
          ?perm:Unix.file_perm ->
          ?flag:Unix.open_flag -> string -> string -> unit
      end
    val file_copy : ?perm:Unix.file_perm -> filename -> filename -> unit
    val file_append : ?perm:Unix.file_perm -> filename -> filename -> unit
    val put : ?perm:Unix.file_perm -> filename -> content -> unit
    val rewrite : ?perm:Unix.file_perm -> filename -> content -> unit
    val append : ?perm:Unix.file_perm -> filename -> content -> unit
    val cat : filename -> string
    module Templib :
      sig
        val temp_name :
          dir:bool ->
          perm:Unix.file_perm ->
          parent:string -> prefix:string -> suffix:string -> unit -> string
      end
    val temp_dir :
      ?perm:Unix.file_perm ->
      ?parent:string -> ?prefix:string -> ?suffix:string -> unit -> string
    val temp_file :
      ?perm:Unix.file_perm ->
      ?parent:string ->
      ?prefix:string -> ?suffix:string -> ?content:content -> unit -> string
    module TMPDIR :
      sig
       val temp_file : ?perm:Unix.file_perm -> ?prefix:string -> ?suffix:string -> unit -> string
       val open_temp : ?perm:Unix.file_perm -> ?prefix:string -> ?suffix:string -> unit -> string * Unix.file_descr
      end

    val file_kind_of_char : char -> Unix.file_kind option
    val iter_dir : (string -> 'a) -> string -> unit
    module Findlib :
      sig
        exception Hidden of exn
        val hide_exn : ('a -> 'b) -> 'a -> 'b
        val reveal_exn : ('a -> 'b) -> 'a -> 'b
        val find :
          (Unix.error * string * string -> unit) ->
          (string -> Unix.stats -> bool) ->
          bool -> int -> string list -> unit
      end
    val find :
      ?follow:bool ->
      ?maxdepth:int -> ?kind:char -> ?name:string -> string -> string list
    module Passwdlib : sig val read_passwd : string -> string end
    val read_passwd : string -> string

    module Process_status :
      sig
        type t = Unix.process_status
        val string_of : t -> string
        val print : Unix.process_status -> unit
        val prerr : Unix.process_status -> unit
        val print_endline : Unix.process_status -> unit
        val prerr_endline : Unix.process_status -> unit
        val fprintf :
          out_channel ->
          (string -> 'a, out_channel, unit) format ->
          Unix.process_status -> 'a
        val eprintf :
          (string -> 'a, out_channel, unit) format ->
          Unix.process_status -> 'a
        val printf :
          (string -> 'a, out_channel, unit) format ->
          Unix.process_status -> 'a
        val sprintf :
          (string -> 'a, unit, string) format -> Unix.process_status -> 'a
      end

    type command = string
    type program = string

    val kill_safe : int -> int -> unit
    exception Signal_forward of int
    exception Waitpid
    val create_process_and_wait :
          ?stdin:Endpoint.Source.t ->
          ?stdout:Endpoint.Sink.t ->
          ?stderr:Endpoint.Sink.t ->
          ?pseudo:string option ->
          ?forward:int list -> program -> string list -> int

    val run :
      ?shell:program ->
      ?trace:bool ->
      ?input:content -> command -> string * Unix.process_status
    val shell : ?shell:program -> ?trace:bool -> ?input:content -> command -> string
  end
