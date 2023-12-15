type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  (*converti une base en un string*)
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let len = String.length str in
  let rec aux str list index  =
    match index with
    | i when i < len -> aux str ((str.[i])::list) (i+1)
    |_ -> List.rev list
  in aux str [] 0
  


(* conversions *)
let base_of_char (c : char) : base =
  match c with
  |'A'-> A
  |'C'-> C
  |'G'-> G
  |'T'-> T
  |_ -> WC

let char_of_base (b : base) : string = (*converti une base en un string de longueur 1 *)
  match b with
  |A -> "A"
  |C -> "C"
  |G -> "G"
  |T -> "T"
  |WC -> "."


let dna_of_string (s : string) : base list =
  let l = explode s in
  (*converti le string s en une list de string *)
  let rec aux l d =
    match l with
    |a::l -> aux l (base_of_char(a)::d)
    (*pour chaque element le convertir en Base et le mettre dans la liste de sortie et appeler
       la fonction recursive aux sur le rest de la liste*)
    |[] -> List.rev d
    (* si la liste est vide en retourne l'inverse de la liste  *)
  in aux l []


let string_of_dna (seq : dna) : string =
  (*converti de l'adn "seq" en un string*)
  let rec aux s seq =
    match seq with
    | a::seq' -> aux ((char_of_base a)^s) seq'
     (*Pour chaque element de la liste, le convertir en un caractere et le mettre dans mon string de sortie*)
    |[] -> s
  in aux "" (List.rev seq)



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  (*Coupe le prefix slice de list et retourn le reste *)
    match (slice,list) with
    (* Si le premier element slice est le meme avec celui de list alors verfier 
       alors appeler cut_prefix sur le reste de slice et list sinon retourner None*)
    |(a::slice, b::list) -> if(a=b) then cut_prefix slice list else None
    (*Cas particulié : Si slice et vide retourner Some(list) *)
    |([],_) ->Some(list)
    (* A part ses deux cas : slice n'existe pas dans list donc retourner None *)
    |_ -> None

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
(*deuxieme fonction auxiliaire de la fonction first_occ *)
(*cette fonction part du principe que l doit contenir slice au debut sinon retourne
   directement None, en d'autre terme l = slice@suf *)
let rec aux2 slice l before after =

  match slice, l with
  |[],_ -> Some(before,l) (* si slice est vide retourner before et l  comme after*)
  |_,[] -> None (*dans ce cas slice n'existe pas dans l*)
  |a::slice,b::l -> if a=b then (*si les deux premiers elements de slice et l sont egaux*)
        match aux2 slice l before after with (*appeler recursivement aux sur le reste de slice et l*)
        |Some(before,after) -> Some(before,after)
        |None -> None
      else  None (*si le premier element de slice et l sont deffirent donc slice n'exist pas dans l*)
   
let rec first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  let rec aux slice list before after =
    match list with
    |[] -> None
    | a::rest'->
        match aux2 slice list before after with
        |Some(before,after) -> Some(List.rev before,after) (*si slice etait au debut de l*)
        |None -> aux slice rest' (a::before) after (*sinon sauvgarder le premier element de l dans before et appeler
           aux pour chercher slice dans le reste de la liste *)
  in
  match slice,list with
  |[],[] -> Some([],[]) (*cas particulié: si slice et list sont vides retourner directement Some([],[])*)
  |_,_ -> aux slice list [] []  


(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =

  let rec aux start stop list result =
   
    match first_occ start list with 
    |None ->  result (*start n'est pas une occurence dans l *)
    |Some(before,after) -> (*after contient result@stop@rest_sans_importance*)
        match first_occ stop after with
        (*before2 contient la partie qu'on cherchait*)
        |Some(before2,after2) -> if(after2 = []) then (before2::result) 
          (*si after2 est vide retourner directement (before2::result)  *)
            else aux start stop after2 (before2::result)(* after2 n'est pas vide, on cherche encore
               d'autre slices entre start et stop*)
        |None-> result(* stop n'est pas une occurence dans l *)
                         
  in let res = aux start stop list [] in (List.rev res)

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between  [A;T;G] [T;A;A] dna
  (*recuperer directement avec slices_between les genes delemité par ATG et TAA*)

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

let  occ l b = (* revois le nombre d'occurence d'un element dans une List *)
  let rec aux l acc =
    match l with
    | a::l -> if a=b then aux l (acc +1) else aux l acc
    | [] -> acc
  in aux l 0

let  delete_element_list l a = (* supprime toutes les occurences de a dans l *)
  let rec aux l a result =
    match l with
    |b::l -> if a=b then aux l a result else aux l a (b::result)
    |[]-> List.rev result
  in aux l a []

let element_sans_rep l = (* renvoi une list sans répétitions de tout ses elements en utilisant delete_element_list*)
  let rec aux l result=
    match l with
    |a::l-> aux (delete_element_list l a) (a::result)
    |[] -> List.rev result;
  in aux l []

let is_full l = (*fonction booleane qui renvoi true si une liste est full false sinon*)
  match l with
  | a::l' -> let rec aux list a = (*verifie si ce premier element est egal au reste des elements de ma liste*)
               match list with
               | b ::list -> (a = b) && aux list a
               | [] -> true
      in aux l' a
  |[] -> false

let  is_partiel l = (*fonction booleane qui test si une list est partiel *)
(*max : est une variable dont la laquelle on garde l'occurence maximal des elements de la list
  unique: est un boolean qui nous indique si cette valeur maximal est unique, si ce n'est pas le cas
  is_partiel revoit false
  lcomp : contient tout les elemnts de l sans repitions *)
  let rec aux l lcomp max unique elem =
    match lcomp with
    |a::lcomp -> let nb = (occ l a) in if ( nb > max) then aux l lcomp nb true a else
      (*si l'occurence de a > que max, appeler aux sur le reste de la lcomp et true pour unique*)
        if nb = max then aux l lcomp max false a else aux l lcomp max unique elem
        (*sinon si elle est egale au max on appel aux avec unique = false sinon on appel aux sans changer max et unique*)
    |[] -> (unique,max,elem)
  in aux l (element_sans_rep l) 0 true (List.hd l)

let consensus (list : 'a list) : 'a consensus =
  if list =  [] then No_consensus else
    (*on test si list est full sinon tester si elle est partiel sinon elle est No_consensus *)
    if (is_full list)  then Full(List.hd list) else let (est_partiel,occ,elem) = is_partiel list in
      if est_partiel then Partial(elem,occ) else No_consensus

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

 (*fonction necessaire pour l'implementation de consensus_sequence
    son objectif est de retourner les premier elements des listes d'une list de list 
    ainsi que le reste de cette liste  *)
 let first_elem_list list =
  let rec aux list result nv_list =
    match list with
    |[] -> (List.rev result,List.rev nv_list)
    |a::list -> (*pour chaque list de ma list general*)
        match a with 
        (*si cette list n'est pas vide*)
        |b::a -> aux list (b::result) (a::nv_list) (*garder ce premier element
           dans result remetre le reste de la liste dans nv_list*)
        |[] -> (result,nv_list) 
  in aux list [] []
   


  let consensus_sequence (ll : 'a list list) : 'a consensus list =
    let rec aux list result =
      let (l,list') = first_elem_list list in 
      (*calculer le consensus de l et le mettre dans result et appeler
         la fonction recursivement sur le rest de la list *)
      if list' <> [] then aux list' ((consensus l)::result) else
        List.rev result 
    in aux ll []

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
