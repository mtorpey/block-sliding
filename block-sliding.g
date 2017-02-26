make_game := function(nrows, ncols, shapes, obstacle, type_lookup)
  local game, i, j;
  game := rec();
  game.board := List([1..nrows], i-> ListWithIdenticalEntries(ncols, 0));
  game.nrows := nrows;
  game.ncols := ncols;
  game.shapes := shapes;
  # for each shape, the lowest number of an identical shape
  game.type_lookup := type_lookup;
  game.history := "";
  if Length(obstacle) > 1 then
    for j in [1..Size(obstacle[1])] do
      game.board[obstacle[1][j]][obstacle[2][j]] := -1;
    od;
  fi;
  for i in [1..Size(shapes)] do
    for j in [1..Size(shapes[i][1])] do
      game.board[shapes[i][1][j]][shapes[i][2][j]] := i;
    od;
  od;
  return game;
end;

move_shape := function(game, shape, drows, dcols)
  local board, rows, cols, newrows, newcols, i;
  board := game.board;
  rows := game.shapes[shape][1];
  cols := game.shapes[shape][2];
  
  # Find the new coordinates
  newrows := rows + drows;
  newcols := cols + dcols;

  # See if the shape can move
  if Minimum(newrows) < 1 or Maximum(newrows) > game.nrows or
     Minimum(newcols) < 1 or Maximum(newcols) > game.ncols then
    return fail;
  fi;
  for i in [1..Size(rows)] do
    if board[newrows[i]][newcols[i]] <> 0 and
       board[newrows[i]][newcols[i]] <> shape then
      return fail;
    fi;
  od;
  
  # Move the shape
  for i in [1..Size(rows)] do
    board[rows[i]][cols[i]] := 0;
  od;
  for i in [1..Size(rows)] do
    board[newrows[i]][newcols[i]] := shape;
  od;
  game.shapes[shape] := [newrows, newcols];
  return true;
end;

hash_game := function(game)
  local list, i;
  list := Concatenation(game.board);
  for i in [1 .. Length(list)] do
    if list[i] > 0 then
      list[i] := game.type_lookup[list[i]];
    fi;
  od;
  return list;
end;

explore := function(game, win)
  local ht, game_list, gameNo, nrgames, last_queue, len, i, first_shape, 
        shapes_in_order, shape, newgame;
  ht := HTCreate(hash_game(game), rec(forflatplainlists := true));
  # treehashsize := 10000)
  game_list := [game];
  gameNo := 1;
  nrgames := 1;
  last_queue := 0;
  while gameNo <= nrgames do
    if gameNo mod 20000 = 0 then
      Print(gameNo, " games processed, ",
            nrgames - gameNo, " in queue, ",
            nrgames - gameNo - last_queue, " longer than last time.\n");
      last_queue := nrgames - gameNo;
    fi;
    game := game_list[gameNo];
    # Have we succeeded?
    if win(game) then
      return game;
    fi;
    
    # Preference for the last moved piece
    len := Length(game.history);
    if len > 0 then
      i := len - 1;
      repeat
        i := i - 1;
      until i = 0 or not game.history[i] in "0123456789";
      first_shape := EvalString(game.history{[i+1 .. len-1]});
      shapes_in_order := Concatenation([first_shape],
                                       [1 .. first_shape-1],
                                       [first_shape+1 .. Length(game.shapes)]);
    else
      shapes_in_order := [1 .. Size(game.shapes)];
    fi;

    for shape in shapes_in_order do
      # Go North
      if move_shape(game, shape, -1, 0) <> fail then
        if HTValue(ht, hash_game(game)) = fail then
          newgame := StructuralCopy(game);
          newgame.history := Concatenation(newgame.history, PrintString(shape), "N");
          nrgames := nrgames + 1;
          game_list[nrgames] := newgame;
          HTAdd(ht, hash_game(newgame), true);
        fi;
        move_shape(game, shape, +1, 0);
      fi;
      # Go South
      if move_shape(game, shape, +1, 0) <> fail then
        if HTValue(ht, hash_game(game)) = fail then
          newgame := StructuralCopy(game);
          newgame.history := Concatenation(newgame.history, PrintString(shape), "S");
          nrgames := nrgames + 1;
          game_list[nrgames] := newgame;
          HTAdd(ht, hash_game(newgame), true);
        fi;
        move_shape(game, shape, -1, 0);
      fi;
      # Go East
      if move_shape(game, shape, 0, +1) <> fail then
        if HTValue(ht, hash_game(game)) = fail then
          newgame := StructuralCopy(game);
          newgame.history := Concatenation(newgame.history, PrintString(shape), "E");
          nrgames := nrgames + 1;
          game_list[nrgames] := newgame;
          HTAdd(ht, hash_game(newgame), true);
        fi;
        move_shape(game, shape, 0, -1);
      fi;
      # Go West
      if move_shape(game, shape, 0, -1) <> fail then
        if HTValue(ht, hash_game(game)) = fail then
          newgame := StructuralCopy(game);
          newgame.history := Concatenation(newgame.history, PrintString(shape), "W");
          nrgames := nrgames + 1;
          game_list[nrgames] := newgame;
          HTAdd(ht, hash_game(newgame), true);
        fi;
        move_shape(game, shape, 0, +1);
      fi;
    od;
    Unbind(game_list[gameNo]);
    gameNo := gameNo + 1;
  od;
  
  # We can't reach a winning state
  return fail;
end;

process_history := function(history, shape_names)
  local dir, i;
  for dir in ["N", "S", "E", "W"] do
    history := ReplacedString(history, dir, Concatenation("~", dir, "@"));
  od;
  history := SplitString(history, "", "@");
  for i in [Length(history), Length(history)-1 .. 1] do
    history[i] := SplitString(history[i], "", "~");
    history[i][1] := shape_names[EvalString(history[i][1])];
  od;
  for i in [Length(history), Length(history)-1 .. 1] do
    if i>1 and history[i][1] = history[i-1][1] then
      history[i-1][2] := Concatenation(history[i-1][2], " ", history[i][2]);
      Unbind(history[i]);
    else
      history[i] := Concatenation(history[i][1], " ", history[i][2], "\n");
    fi;
  od;
  return Flat(history);
end;

read_puzzle := function(filename)
  local stream, lines, next_line, list, nrrows, nrcols, obstacle, shapes, 
        shape_names, type_lookup, lineNo, xs, ys, type_no, shape_nos, cond, 
        shape_no, x, y, win, initial_game;
  # Open the file
  stream := InputTextFile(filename);
  if stream = fail then
    ErrorNoReturn("that file does not exist,");
  fi;
  
  # Read all the lines
  lines := [];
  repeat
    next_line := ReadLine(stream);
    if next_line = fail then
      break;
    fi;
    if Remove(next_line) <> '\n' then
      ErrorNoReturn("line ended without newline,");
    fi;
    next_line := SplitString(next_line, "#")[1]; # Remove comments
    if ForAny(next_line, c-> c <> ' ') then # Don't use blank lines
      Add(lines, next_line);
    fi;
  until false;
  
  # game nrrows nrcols
  list := SplitString(lines[1], "", " ");
  if list[1] <> "game" or Length(list) <> 3 then
    ErrorNoReturn("first line should be \"game nrrows nrcols\",");
  fi;
  nrrows := EvalString(list[2]);
  nrcols := EvalString(list[3]);
  if not (IsPosInt(nrrows) and IsPosInt(nrcols)) then
    ErrorNoReturn("nrrows and nrcols should be integers, not ",
                  nrrows, " and ", nrcols, ",");
  fi;
  
  obstacle := [];
  shapes := [];
  shape_names := [];
  type_lookup := [];
  # shapes
  for lineNo in [2 .. Length(lines) - 1] do
    list := SplitString(lines[lineNo], ":,()");
    if not Length(list) in [3,4] then
      ErrorNoReturn("\"", lines[lineNo], "\" is not a valid shape,");
    fi;
    xs := List(SplitString(list[2], "", " "), EvalString);
    ys := List(SplitString(list[3], "", " "), EvalString);
    if not (Length(xs) = Length(ys)
            and ForAll(xs, IsPosInt)
            and ForAll(ys, IsPosInt)) then
      ErrorNoReturn(xs, " and ", ys,
                    " should all be pos ints and have the same length,");
    fi;
    if list[1] = "obstacle" then
      obstacle := [xs, ys];
    else
      Add(shape_names, list[1]);
      Add(shapes, [xs, ys]);
      if IsBound(list[4]) then
        type_no := EvalString(list[4]);
        if not IsPosInt(type_no) then
          ErrorNoReturn(type_no, " should be a pos int,");
        elif type_no > Length(shapes) then
          ErrorNoReturn("type_no should be no more than ", Length(shapes), ",");
        fi;
        Add(type_lookup, type_no);
      else
        Add(type_lookup, Length(shapes));
      fi;
    fi;
    if Size(shapes) <> Size(shape_names)
       or Size(shapes) <> Size(type_lookup) then
      ErrorNoReturn("this should never happen,");
    fi;
  od;
  
  # win condition
  xs := [];
  ys := [];
  shape_nos := [];
  
  list := SplitString(lines[Length(lines)], ",");
  list := List(list, str-> SplitString(str, "", " "));
  if Remove(list[1], 1) <> "win" then
    ErrorNoReturn("final line should start with \"win\",");
  fi;
  for cond in list do
    if Length(cond) <> 3 then
      ErrorNoReturn(cond, " should have 3 elements,");
    fi;
    shape_no := Position(shape_names, cond[1]);
    if shape_no = fail then
      ErrorNoReturn("\"", cond[1], "\" is not the name of a shape,");
    fi;
    x := EvalString(cond[2]);
    y := EvalString(cond[3]);
    if not (IsPosInt(x) and IsPosInt(y)) then
      ErrorNoReturn(x, " and ", y, " should be pos ints,");
    fi;
    Add(xs, x);
    Add(ys, y);
    Add(shape_nos, shape_no);
  od;
  
  win := function(game)
    return ForAll([1 .. Length(xs)],
                  i-> game.board[xs[i]][ys[i]] = shape_nos[i]);
  end;
  
  initial_game := make_game(nrrows, nrcols, shapes, obstacle, type_lookup);
  return rec(initial_game := initial_game,
             win := win,
             shape_names := shape_names,
             type_lookup := type_lookup
            );
end;

solve_game_file := function(filename)
  local record, won_game;
  record := read_puzzle(filename);
  Print("The board is as follows:\n");
  PrintArray(record.initial_game.board);
  Print("The type lookup is:\n", record.type_lookup, "\n");
  Print("Solving now...\n");
  won_game := explore(record.initial_game, record.win);
  if won_game = fail then
    Print("No solution possible\n");
  else
    Print(process_history(won_game.history, record.shape_names));
  fi;
  return;
end;
