let idSTOP    = 1
let idPUSHADDR = 2
let idCONST   = 3
let idPUSH    = 4
let idCLOSURE = 5
let idAPPLY   = 6
let idGRAB    = 7
let idRESTART  = 8
let idSTACKACC = 9
let idENVACC  = 10
let idADDINT  = 11
let idRETURN  = 12
let idBRANCH  = 13
let idBRANCHIF = 14

type buffer = {mutable data: bytes; mutable pos: int};;

let buffer_append b1 b2 =
  let len = Bytes.length b1.data in
  (if b1.pos + b2.pos > len then
     let newbuf = Bytes.create ((len*2)+b2.pos) in
     Bytes.blit b1.data 0 newbuf 0 b1.pos;
     Bytes.blit b2.data 0 newbuf b1.pos b2.pos;
     b1.data <- newbuf;
   else
     Bytes.blit b2.data 0 b1.data b1.pos b2.pos);
  b1.pos <- b1.pos + b2.pos

let new_buffer () = {data = Bytes.create 32; pos = 0}

let o_byte b c =
  let len = Bytes.length b.data in
  if b.pos >= len then (
    let newbuf = Bytes.create (len*2) in
    Bytes.blit b.data 0 newbuf 0 len;
    b.data <- newbuf
  );
  Bytes.set b.data b.pos c;
  b.pos <- b.pos + 1

let o_uint32 b u =
  let p0 = u land 255 |> char_of_int in
  let p1 = u lsr 8 land 255 |> char_of_int in
  let p3 = u lsr 16 land 255 |> char_of_int in
  let p4 = u lsr 24 land 255 |> char_of_int in
  o_byte b p0; o_byte b p1; o_byte b p3; o_byte b p4

let o_uint64 b u =
  let u1 = u land 65535 in
  let u2 = u land 65535 lsr 32 in
  o_uint32 b u1; o_uint32 b u2

let o b id =
  o_byte b (id land 255 |> char_of_int)

let rec emit_inst buf x =
  match x with
  | Instruct.Const n -> o buf idCONST;o_uint64 buf (n*2+1)
  | Instruct.Bool n -> o buf idCONST;
    o_uint64 buf (if n then 18 else 34)
  | Instruct.Stop -> o buf idSTOP
  | Instruct.Apply -> o buf idAPPLY
  | Instruct.Plus -> o buf idADDINT
  | Instruct.Return n ->
    o buf idRETURN;
    o_byte buf (char_of_int n)
  | Instruct.Closure l ->
    let tmpbuf = new_buffer () in
    List.iter (emit_inst tmpbuf) l;
    o buf idCLOSURE;
    o_uint32 buf tmpbuf.pos;
    buffer_append buf tmpbuf
  | Instruct.Grab n ->
    o buf idRESTART;
    o buf idGRAB;
    o_byte buf (char_of_int n);
  | Instruct.PushRetAddr l ->
    let tmpbuf = new_buffer () in
    List.iter (emit_inst tmpbuf) l;
    o buf idPUSHADDR;
    o_uint32 buf tmpbuf.pos;
    buffer_append buf tmpbuf
  | Instruct.Push ->
    o buf idPUSH
  | Instruct.StackAccess n ->
    o buf idSTACKACC;
    o_byte buf (char_of_int n)
  | Instruct.EnvAccess n ->
    o buf idENVACC;
    o_byte buf (char_of_int n)
  | Instruct.Branch (t,f) ->
    let truebuf = new_buffer () in
    let falsebuf = new_buffer () in
    List.iter (emit_inst truebuf) t;
    List.iter (emit_inst falsebuf) f;
    o buf idBRANCHIF;
    o_uint32 buf (falsebuf.pos+5);
    buffer_append buf falsebuf;
    o buf idBRANCH;
    o_uint32 buf truebuf.pos;
    buffer_append buf truebuf

let emit buf bc =
  List.iter (emit_inst buf) bc;
  Bytes.sub buf.data 0 buf.pos
