(* Inclusion de bibliothèques *)
 (* Spécification de la constante TERM_CRITERION *)
 let term_criterion = 1

 (* Spécification de constantes *)
 let zero =int_of_float 0x1p0 - 1
 let b32 =int_of_float 0x1p32 - 1
 let b16 =int_of_float 0x1p16 - 1
 let b08 =int_of_float 0x1p8 - 1
 let b04 =int_of_float 0x1p4 - 1
 let b02 =int_of_float 0x1p2 - 1
 let b01 =int_of_float 0x1p1 - 1
 let board =  ref (int_of_float 0x1p0 - 1)

 let ind_empty_hold=ref 0
 
 (* Contient des masques de bits masquant les n bits les plus bas *)
 let b_lvl = [| b01; b02; b04; b08; b16; b32 |]
 
 (* Taille du plateau (nombre de trous) *)
 let num_board_bits = 33
 
 (* Après l'initialisation, ce tableau contiendra les numéros de bits de tous les trous
 commençant par le haut (de gauche à droite dans les rangées). *)
 let board_bits = Array.make num_board_bits 0
 
 (* Nombre de bits qui spécifient la bordure. Notez que plusieurs bits sont utilisés comme bordure
 des deux côtés du plateau. Cela est dû au schéma d'adressage du plateau et
 ne nuit pas. *)
 let num_boundary_bits = 32
 
 (* Les bits réels qui spécifient la bordure du plateau *)
 let boundary_bits = [| 48;58;4;14;24;23;22;32;42;41;40;39;38;28;18;17;16;6;60;50;40;41;42;32;22;23;24;25;26;36;46;47 |]
 
 let border =  ref Int64.zero
 (* Spécifie les coins (bord) du plateau. Nécessaire pour empêcher les mouvements dans les coins
 (ou en fait; essayez-les plus tard) *)
 let num_corners = 24
 let corner_bits = [|57;3;13;12;11;21;31;30;29;19;9;8;7;61;51;52;53;43;33;34;35;45;55;56|]
 (* Masque de bits réel pour tous les trous d'angle *)
 let corners =  ref Int64.zero
 
 (* Combien de rangées (colonnes) le plateau a-t-il *)
 let num_rows = 7
 
 (* Opérations pour déplacer les pions. Une opération UP (vers le haut) provoquera tous les pions du plateau à être
 déplacé d'un cran vers le haut (certains pourraient être déplacés dans la bordure). *)
 let up = 1
 let down = -1
 let left = -10
 let right = 10
 let directions = [| down; left; up; right |]
 
 (* Constantes pour la table de transposition. L'utilisation de cette table peut réduire considérablement
 les efforts pour l'algorithme de*)
 (* Constants for the transposition table. Using this table can significantly reduce the
 the efforts for the backtracking algorithm; since re-occuring positions do not have to
 be searched twice. Permutations of one move sequence might lead to the same position,
 which only has to be investigated once. *)
 
 let hashsize = (1 lsl 25)
 let hashmask = hashsize - 1
 let hashmiss = -99
 
 (* Number of symmetries. In total there are actually 8 symmetric positions for each board.
 Currently, only vertically and horizontally mirrored positions are considered. Rotations
 are not yet implemented. *)
 let numsymmetries = 4
 
 (* Masks for horizontal and vertical lines. Needed for mirroring a board along the vertical or
 horizontal axis *) 
 let horlines = Array.make num_rows 0L
 let vertlines = Array.make num_rows 0L
 
 
 (* Definition of one element of the transposition table. It contains a key (actual board, since several positions
 can be mapped to the same hash-table entry) and a value (number of remaining pegs when solving this specific
 position). *)
 type hash_element = { mutable key: int64; mutable value: int }
 let hash_table = Array.make hashsize { key = 0L; value = 0; }
 
 (* Modulo operator, since the %-operator is the remainder and cannot deal with negative integers *)
 let mod' a b =
   let r = a mod b in
   if r < 0 then r + b else r
 
 (* Determines the position of a single bit in a 64bit variable in logarithmic time. *)
 let bit_pos x =
   let rec loop bPos i x =
     if i < 0 then bPos
     else if Int64.logand (x) (Int64.of_int b_lvl.(i)) = 0L then
       let nBits = b01 lsl i in
       let x = Int64.shift_right x nBits in
       let bPos = bPos + nBits in
       loop bPos (i - 1) x
     else loop bPos (i - 1) x
   in loop 0 5 x
 
 
 (* Fast way to count the one-bits in a 64-bit variable.
 Only requires as many iterations as bits are set. *)
 let bit_count x =
   let rec loop x c =
     if x = 0L then c
     else loop (Int64.logand x (Int64.sub x 1L)) (c + 1)
   in loop x 0
 
 (* Rotatate 64-bit variable x by y bits. Note that this is not a shift-operation but a real
 rotate-left *)
 let rol x y =
   let y = y mod 64 in
   Int64.logor (Int64.shift_left x y) (Int64.shift_right_logical x (64 - y))
 
 let board_bits = Array.make num_board_bits 0
 
 let init_board_bits () =
   let start_row = ref 57 and start_col = ref 35 and nrow_1 = 3 and nrow = ref 3 and nrow_2 = 7 in
   let l = ref 0 in
   for i = 0 to num_rows - 1 do
     let k = ref !start_row and m = ref !start_col in
     for j = 0 to !nrow - 1 do
       board_bits.(!l) <- !k;
       horlines.(i) <- Int64.logor horlines.(i) (Int64.shift_left 1L !k);
       vertlines.(i) <- Int64.logor vertlines.(i) (Int64.shift_left 1L !m);
         (** Printf.printf "%d*" !k;*)
       incr l;
       k := (!k + 10) mod 64;
       decr m;
     done;
   
     nrow := if i > 0 && i < 4 then nrow_2 else nrow_1;
     start_row :=
       if i = 1 then ((!start_row - 21) mod 64)
       else if i = 4 then ((!start_row + 19) mod 64)
       else ((!start_row - 1) mod 64);
     start_col :=
       if i = 1 then ((!start_col + 12) mod 64)
       else if i = 4 then ((!start_col + 8) mod 64)
       else ((!start_col + 10) mod 64);
   done
 
 let rec log2 n x =
   if x >= n then 0 else 1 + log2 n (x*2)
 
 
 (*ajout num & temps de chque coup*)
 
 
   let write_csv move_num time =
    let oc = open_out_gen [Open_append; Open_creat] 0o666 ("bit_num&time_trou"^(string_of_int !ind_empty_hold)^"_vide \n")  in
        output_string oc (string_of_int move_num);
        output_string oc "  ";
        output_string oc (string_of_float time);
    output_string oc "\n";
    close_out oc
 
 
 
 
 
 
 
 let init_board_boundary () =
   for i = 0 to num_boundary_bits - 1 do
     border :=Int64.logor !border  ( (Int64.shift_left  1L boundary_bits.(i)));
   done;
  
   for i = 0 to num_board_bits - 1 do
     board := !board lor (Int64.to_int (Int64.shift_left  1L board_bits.(i)))
   done
   
  
 let init_corners () =
   for i = 0 to num_corners - 1 do
     corners :=Int64.logor !corners  ( (Int64.shift_left  1L corner_bits.(i)))
   done;
   corners = corners    
 
 let init_hash_table () =
   for i = 0 to hashsize - 1 do
     hash_table.(i) <- {key = 0L; value = 0}
   done
   
 let init () =
   init_board_bits ();
   init_board_boundary ();
   init_hash_table ();
   init_corners ()
 
 let print_board b =
   let l = ref 0 and nrow_1 = 3 and nrow = ref 3 and nrow_2 = 7 in
   Printf.printf "\n";
   for i = 0 to num_rows - 1 do
     for k = 0 to num_rows - !nrow - 1 do
       Printf.printf " ";
     done;
     Printf.printf "|";
     for j = 0 to !nrow - 1 do
       let symb = if (Int64.(logand (shift_left 1L board_bits.(!l)) b) <> 0L) then 'x' else 'o' in
       Printf.printf "%c|"  symb;
 
       incr l;
     done;
     Printf.printf "\n";
     nrow := if i > 0 && i < 4 then nrow_2 else nrow_1
   done;
 
   Printf.printf "\n"
 
 
 let write_board_to_csv b  dir x file_name =
   let oc = open_out_gen [Open_append; Open_creat] 0o666 file_name in
   let l = ref 0 and nrow_1 = 3 and nrow = ref 3 and nrow_2 = 7 in
   output_string oc ("Move:"^(string_of_int dir)^"  "^(string_of_int (bit_pos x))^"\n");
   for i = 0 to num_rows - 1 do
     for k = 0 to num_rows - !nrow - 1 do
       output_string oc " "
     done;
     for j = 0 to !nrow - 1 do
       let symb = if (Int64.(logand (shift_left 1L board_bits.(!l)) b) <> 0L) then 'x' else 'o' in
       output_string oc (String.make 1 symb);
       output_string oc " ";
       incr l;
     done;
     output_string oc "\n";
     nrow := if i > 0 && i < 4 then nrow_2 else nrow_1
   done;
   output_string oc "\n";
   output_string oc "\n";
   close_out oc
     
     
 let remove_peg b bit  = 
   let k=Int64.logand b (Int64.lognot (Int64.shift_left 1L bit)) in 
  
   write_board_to_csv k 0 0L ("config_avec_trou"^(string_of_int bit)^"_vide \n") ;
 
   k
   
 
 
           
     (* Place a peg at a certain position and return the modified board *)
 let set_peg b bit =
   Int64.logor (Int64.shift_left 1L bit) b
           
     (* Function to compute the hash for a 64bit variable. Maybe Zobrist keys would work better (has to be investigated
        in future). *)
 let get_hash x =
   let h1 = Int64.logxor x (Int64.shift_right_logical x 30) in
   let h2 = Int64.mul h1 0xbf58476d1ce4e5b9L in
   let h3 = Int64.logxor h2 (Int64.shift_right_logical h2 27) in
   let h4 = Int64.mul h3 0x94d049bb133111ebL in
   Int64.logxor h4 (Int64.shift_right_logical h4 31)
       
       
       
     
 let mirror_hor b =
   let m = Int64.logand b horlines.(4) in
   let m = Int64.logor m (rol (Int64.logand b horlines.(0)) (6 * down))in
   let m = Int64.logor m (rol (Int64.logand b horlines.(1)) (4 * down)) in (* Second row *)
   let m = Int64.logor m (rol (Int64.logand b horlines.(2)) (2 * down)) in (* Third row *)
   let m = Int64.logor m (rol (Int64.logand b horlines.(4)) (2 * up)) in   (* Fifth row *)
   let m = Int64.logor m (rol (Int64.logand b horlines.(5)) (4 * up)) in   (* Sixth row *)
   let m = Int64.logor m (rol (Int64.logand b horlines.(6)) (6 * up)) in   (* Seventh row *)
   m      
 let mirror_vert b =
   let m = Int64.logand b vertlines.(4) in
   let m = Int64.logor m (rol (Int64.logand b vertlines.(0)) (6 * right)) in (* First Column *)
   let m = Int64.logor m (rol (Int64.logand b vertlines.(1)) (4 * right)) in (* Second Column *)
   let m = Int64.logor m (rol (Int64.logand b vertlines.(2)) (2 * right)) in (* Third Column *)
   let m = Int64.logor m (rol (Int64.logand b vertlines.(4)) (2 * left)) in  (* Fifth Column *)
   let m = Int64.logor m (rol (Int64.logand b vertlines.(5)) (4 * left)) in  (* Sixth Column *)
   let m = Int64.logor m (rol (Int64.logand b vertlines.(6)) (6 * left)) in  (* Seventh Column *)
   m
       
     (* Compute all mirrored positions for a board b and return all in the array m *)
 let mirror b m =
   m.(0) <- b;
   m.(1) <- (mirror_vert b);
   m.(2) <- mirror_hor b;
   m.(3) <- mirror_hor m.(1)
     
 (* Check if a position b or a mirrored equivalent is already stored in the transposition table. If yes, then
 * return the value for this position. *)
 let get_transposition b =
   let m = Array.make numsymmetries 0L in
   mirror b m;
   let rec check_symmetry i =
     if i < numsymmetries then
       let hash = get_hash m.(i) in
       let hash_index = (Int64.to_int hash) land hashmask in
       if hash_table.(hash_index).key = m.(i) then hash_table.(hash_index).value
       else check_symmetry (i + 1)
     else hashmiss in
   check_symmetry 0
 
 (* After a position is completely evaluated, store the value of the position in the transposition table *)
 let rec put_transposition b value =
   let hash = get_hash b in
   let hash_index = (Int64.to_int hash) land hashmask in
   hash_table.(hash_index).key <- b;
   hash_table.(hash_index).value <- value
    
     
   
  
   
     (* Génère la liste de tous les coups possibles dans toutes les directions pour une position de plateau b *)
 let generate_moves (b: int64) (allmv:int64 array) =
   let dir = ref 0 in
   let mv = ref 0L in
   for i = 0 to 3 do
     dir := directions.(i);
     mv :=Int64.logand (rol  (Int64.logand (Int64.logand (( (rol b !dir))) (Int64.of_int !board)) (b)) (!dir)) (Int64.logand (Int64.of_int !board) (Int64.lognot b));
          (* Certaines cases dans chaque direction doivent également être évitées en premier. Elles violent généralement une fonction de Pagoda *)
     let cmv = Int64.logand !mv !corners in (* Trouve tous les coups dans les coins *)
     let zmv = Int64.logand !mv 1L in (* Le déplacement au centre n'est pas bon non plus *)
     let dmv = ref 0L in
     if !dir = down then
       dmv := Int64.logand !mv (Int64.logor (Int64.shift_left 1L 9)
                                  (Int64.logor (Int64.shift_left 1L 53)
                                     (Int64.shift_left 1L 62)))
     else if !dir = up then
       dmv := Int64.logand !mv (Int64.logor (Int64.shift_left 1L 2)
                                  (Int64.logor (Int64.shift_left 1L 11)
                                     (Int64.shift_left 1L 55)))
     else if !dir = left then
       dmv := Int64.logand !mv (Int64.logor (Int64.shift_left 1L 44)
                                  (Int64.logor (Int64.shift_left 1L 53)
                                     (Int64.shift_left 1L 55)))
     else if !dir = right then
       dmv := Int64.logand !mv (Int64.logor (Int64.shift_left 1L 9)
                                  (Int64.logor (Int64.shift_left 1L 11)
                                     (Int64.shift_left 1L 20))) ;
          (* Supprime ces mouvements de la liste de mouvements initiale et essayez-les plus tard, car ils sont probablement sub-optimaux *)
     let mv_later = Int64.logor (Int64.logor cmv zmv) !dmv in
     let mv = Int64.logxor !mv mv_later in
     allmv.(i) <- mv;
     allmv.(i + 4) <- mv_later;
     
       (*print_board allmv.(i+4);*)
 
   done
       
     
 external backtrack : int64 -> int = "backtrack"
 let tryMoves b mv dir backtrack =
   let rec loop b mv move_num st_atime =
     if mv = 0L && move_num <>31 then 0  
       
     else
       let x = Int64.logand (Int64.logxor (Int64.sub mv 1L) mv) mv in
       let b' = Int64.logor b x in (* set peg at new position *)
       let b' = Int64.logand b' (Int64.lognot (rol x (-dir))) in (* remove jumped-over peg *)
       let b' = Int64.logand b' (Int64.lognot (rol x (-2 * dir))) in 
       if(move_num=31) then
       begin
        print_board b';
        write_board_to_csv b' dir x ("resul_trou"^(string_of_int !ind_empty_hold)^"_vide \n");
        write_csv move_num (Sys.time ()-.st_atime); (* Ajout de l'appel à la fonction write_csv *)
        ignore(move_num)
       end
       ;
       (* remove peg from old position *)
       let res = backtrack b' move_num st_atime in
       let b' = Int64.logand b' (Int64.lognot x) in (* remove peg from new position again *)
       let b' = Int64.logor b' (rol x (-dir)) in (* add jumped-over peg again *)
       let b' = Int64.logor b' (rol x (-2 * dir)) in (* set peg to old position *)
       let mv' = Int64.logand mv (Int64.sub mv 1L) in (* remove this move from the list *)
       if res > 0  && res <= term_criterion then begin
         Printf.printf "Move: %d, %d" dir (bit_pos x);
         print_board b';
         write_board_to_csv b' dir x ("resul_trou"^(string_of_int !ind_empty_hold)^"_vide \n");
         write_csv move_num (Sys.time ()-.st_atime); (* Ajout de l'appel à la fonction write_csv *)
         res
       end else loop b' mv' (move_num) st_atime
   in loop b mv
 
 let rec backtrack b (move_num) st_atime =
   let value = get_transposition b in (* first check transposition table for this particular position *)
   if value <> hashmiss then value
   else
     let numTrys = 8 in
     let allmv = Array.make numTrys 0L in (* will contain all possible moves later *)
     generate_moves b allmv; (* find all possible moves, sorted according to some characteristics *)
     let rec loop i nomv =
       if i = numTrys then
         let ret = if nomv = numTrys then (bit_count b) else 0 in (* if no move in any direction was possible *)
         put_transposition b ret;
         ret (* no move could lead to a solution *)
       else
         let mv = allmv.(i) in
         if mv <> 0L then begin
           let res = tryMoves b mv directions.(i mod 4) backtrack (move_num+1) st_atime in
           if res > 0 && res <= term_criterion then res
           else loop (i + 1) nomv
         end else loop (i + 1) (nomv + 1)
     in loop 0 0
 
 
 
 
 
 let print_moves (b:int64) =
   let b = remove_peg  (Int64.of_int !board) 0 in
   let allmv = Array.make 8 0L in
   generate_moves b allmv;
   Printf.printf "Possible moves:\n";
   for i = 0 to 7 do
     Printf.printf "Direction %d: " (directions.(i mod 4));
     print_board allmv.(i);
     Printf.printf "\n";
   done
                     
                 
 let solve () =
   let start_time = Sys.time () in
   ignore (init ());
   ind_empty_hold:=20;
   let b = remove_peg (Int64.of_int !board) !ind_empty_hold in
   ignore(backtrack b 0 start_time) ;
   let end_time = Sys.time () in
   Printf.printf "Temps d'exécution: %f secondes\n" (end_time -. start_time)
   
 
       
      
 let p=solve ()
 
 
 
  
 