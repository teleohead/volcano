type t = {
  line_start : int;
  line_finish : int;
  char_start : int;
  char_finish : int;
}

let dummy = { line_start = 0; line_finish = 0; char_start = 0; char_finish = 0 }

let make line_start line_finish char_start char_finish =
  { line_start; line_finish; char_start; char_finish }

let to_string p =
  Printf.sprintf "%d(%d)..%d(%d)" p.line_start p.char_start p.line_finish
    p.char_finish

let span start_pos end_pos =
  {
    line_start = start_pos.line_start;
    line_finish = end_pos.line_finish;
    char_start = start_pos.char_start;
    char_finish = end_pos.char_finish;
  }
