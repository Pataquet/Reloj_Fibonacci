open Graphics;;
open Unix;;
 
type colour = W | B | R | G;;

type clock = colour array;;

let colour_at clock i=
	if i>=1 && i<= 5 then clock.(i-1)
	else raise (Invalid_argument "colour_at");;

let all_white =Array.make 5 W;;

type time ={hh:int ; mm:int};;

let valid_time t =
	t.hh>= 0 && t.hh < 24 && t.mm >= 0 && t.mm<60;;

let normal_time t =
	t.hh>0 && t.hh <= 12 && t.mm >=0 && t.mm <60;;

let rounded_time t =
	{t with mm = 5*(t.mm /5)};;

let time (h,m)=
	let t = {hh=h ; mm= m} in 
	if valid_time t then t else invalid_arg "time";;
(*Ejercicio 1*)
let time_of_clock clock =
	let h = ref 0 in 
	let m = ref 0 in 
	let fib = ref 1 in
	let ant = ref 0 in
	for i =1 to 5 do 	 
		if clock.(i-1)=B then (h := (!h+ !fib); m :=(!m+ !fib))
		else if clock.(i-1)=R then h := (!h+ !fib)
		else if clock.(i-1)=G then m := (!m+ !fib);
		fib := !fib + !ant;
		ant := !fib - !ant;
	done;
	time (!h,!m*5);;

let arraySetAux c n color=
	(Array.set c n color); c;;

let rec allList clock cont =
	if cont = -1 then [clock]
	else (allList (Array.copy (arraySetAux clock cont W)) (cont-1)) @
		 (allList (Array.copy (arraySetAux clock cont B)) (cont-1)) @ 
		 (allList (Array.copy (arraySetAux clock cont R)) (cont-1)) @ 
		 (allList (Array.copy (arraySetAux clock cont G)) (cont-1));;

(*all_clocks_for_time (time(12,30));;*)
let all_clocks_for_time tiempo =
	if normal_time tiempo then 
		let rec aux l1 l2  = match l1 with
			| [] -> l2
			| h::t -> 
				try
					if time_of_clock h = tiempo
						then aux t (h::l2)
					else aux t l2
				with _ -> aux t l2
		in aux (allList (all_white) 4) []
	else invalid_arg "tiempo no valido";;

let a_clock_for_time tiempo =
	if normal_time tiempo then 
		let rec aux (h::t) = 
			try
				if time_of_clock h = tiempo
					then h
				else aux t
			with _ -> aux t
		in aux (allList (all_white) 4)
	else invalid_arg "tiempo no valido";;

let random_clock_for_time t =
	let all = all_clocks_for_time t in 
	let n = List.length all in List.nth all (Random.int n);;


time_of_clock (a_clock_for_time (time(12,30))) = rounded_time (time(12,30));;
time_of_clock (random_clock_for_time (time(10,30))) = rounded_time (time(10,30));;

(*Ejercicio 2*)




let colour = function 
	W -> white | B -> blue | R -> red | G -> green;;

type sq = {x:int; y:int; s :int};;

let sq = Array.make 5 {x=0;y=0;s=0};;

sq.(0) <- {x=2;y=3;s=1};;
sq.(1) <- {x=2;y=4;s=1};;
sq.(2) <- {x=0;y=3;s=2};;
sq.(3) <- {x=0;y=0;s=3};;
sq.(4) <- {x=3;y=0;s=5};;

let drawSquareS s i c = 
	set_color c ;
	let sq = sq.(i) in 
	fill_rect (sq.x * s) (sq.y *s) (sq.s *s) (sq.s *s);;

let drawlineS s ()= 
	let k = s in 
	set_color black; set_line_width 2;
	for i =0 to 4 do 
		let sq = sq.(i) in
		draw_rect (sq.x * k) (sq.y * k) (sq.s * k) (sq.s * k)
	done;;

let draw_clockS s (c:clock) =
    for i = 0 to 4 do
        drawSquareS s i (colour c.(i))
    done;
    drawlineS s ();;

let open_clock size = 
   let st = " " ^ 
       string_of_int (8 * size) ^ "x" ^ string_of_int (5 * size)
   in 
   open_graph st;
   set_window_title "Fibonacci Clock";
   drawlineS size ();
   draw_clockS size;;

let close_clock () = close_graph ();;

let reloj () = 
    let t = Unix.localtime (Unix.time()) in
    if t.tm_hour > 12 then
    	open_clock 60 (a_clock_for_time(rounded_time(time(t.tm_hour - 12 ,t.tm_min))))
    else 
    	open_clock 60 (a_clock_for_time(rounded_time(time(t.tm_hour,t.tm_min))));
    let rec aux tiempo = 
        if tiempo.tm_hour > 12 then 
           draw_clockS 60 (a_clock_for_time(rounded_time(time(tiempo.tm_hour - 12, tiempo.tm_min))))
        else
           draw_clockS 60 (a_clock_for_time(rounded_time(time(tiempo.tm_hour, tiempo.tm_min))));
        let x = (300-((tiempo.tm_min * 60 + tiempo.tm_sec) mod 300)) in 
        	sleep x; 
            aux (Unix.localtime (Unix.time()))
    in aux (Unix.localtime (Unix.time()));;
(*#use "fibonacciClock.ml";;*)
