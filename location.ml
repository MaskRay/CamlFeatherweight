let input_chan = ref stdin

let output_loc oc input seek (pos1,pos2) =
  let rec move_to pos topos line line_pos =
    if pos >= topos then
      pos, line, line_pos
    else
      try
        let c = input() in
        if c = '\n' then
          move_to (pos+1) topos (line+1) (pos+1)
        else
          move_to (pos+1) topos line line_pos
      with End_of_file ->
        pos+1, line, line_pos
  in
  let copy_line () =
    let c = ref ' ' in
    begin try
      while c := input(); !c != '\n' do output_char oc !c done
    with End_of_file ->
      ()
    end;
    output_char oc '\n'
  in
  let skip_line () =
    try
      while input() != '\n' do () done
    with End_of_file ->
      ()
  in
  let pr_line f l ch =
    let c = ref ' ' and f = ref f and l = ref l in
    try
      while c := input(); !c != '\n' do
        if !f > 0 then (
          decr f;
          output_char oc (if !c = '\t' then !c else ' ')
        ) else if !l > 0 then (
          decr l;
          output_char oc (if !c = '\t' then !c else ch)
        )
      done
    with End_of_file ->
      ()
  in

  seek 0;
  let pos1, line1, line1_pos = move_to 0 pos1 1 0 in
  let pos2, line2, line2_pos = move_to pos1 pos2 line1 line1_pos in

  if line1 = line2 then
    Printf.fprintf oc "line %d, columns %d-%d:\n" line1 (pos1-line1_pos)
    (pos2-line2_pos)
  else
    Printf.fprintf oc "line %d-%d, columns %d-%d:\n" line1 line2 (pos1-line1_pos)
    (pos2-line2_pos);

  if line1 = line2 then (
    seek line1_pos;
    copy_line();
    seek line1_pos;
    pr_line (pos1-line1_pos) (pos2-pos1) '^';
    output_char oc '\n'
  ) else (
    pr_line 0 (pos1-line1_pos) '.';
    seek pos1;
    copy_line();
    if line2-line1 < 8 then
      for i = line1+1 to line2-1 do
        copy_line()
      done
    else (
      for i = line1+1 to line1+3 do copy_line() done;
      output_string oc "........\n";
      for i = line1+4 to line2-4 do skip_line() done;
      for i = line2-3 to line2-1 do copy_line() done
    );
    begin try
      for i = line2_pos to pos2-1 do output_char oc (input()) done;
      pr_line 0 100 '.';
      output_char oc '\n'
    with End_of_file ->
      ()
    end
  )

let output_location oc loc =
  output_loc oc (fun () -> input_char !input_chan)
  (seek_in !input_chan) loc
