(* Fonctions de lecture de fichier *)
let read_line_test chanel =
	try
		Some (input_line chanel)
	with
		|End_of_file -> None
let read_file_tab chanel = 
	let rec aux acc = match  read_line_test chanel with
		|None -> acc
		|Some x -> aux (x :: acc)
	in List.rev (aux [])
	

(* Debut *)
let contenu_tab = 
	let temp = read_file_tab (open_in "Cle36")
	in List.nth temps 0