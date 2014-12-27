let word_size = 32

let sizeof_word = word_size / 8

let obj_magic32 = "zo32"
let obj_magic64 = "zo64"
let exe_magic32 = "ml32"
let exe_magic64 = "ml64"

let obj_magic = if word_size = 32 then obj_magic32 else obj_magic64
let exe_magic = if word_size = 32 then exe_magic32 else exe_magic64
