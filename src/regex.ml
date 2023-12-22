open Regex_base

(*concatener l n fois avec lui-même*)
let rec repeat n l =
  let rec aux n w result =  
    match n with
    | 0 -> result  (*fin*)
    | _ -> aux (n - 1) w (result @ w) (*concatener w avec result et appel recursif avec (n-1) fois restantes*)
  in
  aux n l []  (*initialiser result a la liste vide*)



(*renvoie une expression régulière qui reconnaît les mots formés de la concaténation de n mots reconnus par *)
let rec expr_repeat n e =
    let rec aux n expression result =
      match n with
      | 0 -> result (*fin*)
      | _ -> aux (n - 1) expression (Concat (expression, result))  (*concatener l'expression avec result en utilisant concat et appel reccursif avec n-1 fois restantes*)
    in
    aux n e Eps (*initialiser result à l'expression vide*)









(*tester si le langage reconnu par l'expression e ne contient le mot vide(renvoie true) ou non(renvoie false)*)    


let  is_empty e =
  let rec aux  list = 
  match list with
  |[] -> true 
  | Eps :: list'-> aux list'
  | Base (a):: list' -> false
  | Joker::list' -> false
  | Concat (a, b)::list' -> aux(a::b::list')
  | Alt (a, b)::list' -> aux(a::b::list')
  | Star (a):: list' -> aux (a::list')

  in aux [e]













(*tester si le mot vide appartient au langage reconnu par l'expression e*)    
let rec null e =
  match e with
  | Eps -> true 
  | Base a -> false
  | Joker -> false
  | Concat (a, b) -> null a && null b
  | Alt (a, b) -> null a || null b
  | Star a -> true  (*le lengage reconnu par star a tjr le mot vide*)











(*tester si le langage reconnu par l'expression e est fini ou non*)
let  is_finite e =
  let rec aux list = 
  match list  with
  | [] -> true
  | Eps:: list' -> aux list' 
  | Base(a)::list' -> aux list' 
  | Joker::list' -> aux list' 
  | Concat (a, b)::list' -> aux (a::b::list')
  | Alt (a, b)::list' -> aux (a::b::list')
  | Star(a)::list' -> is_empty a (*si a est une expression vide donc le langage est fini*)
  in aux [e]




















(*renvoie l’ensemble des mots formés de la concaténation d'un mot de l1 et d’un mot de l2.*) 
let product l1 l2 =
  if l1 = [] || l2 = [] then [] (*si l'une des liste est vide on renvoie la liste vide*)
  else if l1 = [[]] || l2 = [[]] then (sort_uniq(l1 @ l2)) (*si l'une des liste contient le mot vide on renvoie directement l'autre liste*)
  else
    (*fonction aux qui concatene un element avec tous les autres elements d'une liste l2*)
  let rec aux element l2 res = 
    match l2 with
    | [] -> res  
    | a::rest -> aux element rest ((element@ a) :: res)  (*concatener l'element avec le premier element de la liste l2, et appel reccursif sur le reste de la liste*) 
  in
    
  (*fonction aux2 qui concatene tous les mots de l1 avec ceux de l2*) 
  let rec aux2 l1 l2 result = 
    match l1 with
      | [] -> List.rev (sort_uniq(result))  (*sorter la liste resultat pour eliminer les doublons*)
      | a::rest -> aux2 rest l2 (aux a l2 result) (*pour chaque element de l1, le concatener avec tous les elems de l2*)
  in aux2 l1 l2 []  (*initialiser la liste resultat a la liste vide*)




  (*renvoyer le langage reconnu par l'expression e sur l'alphabet sous forme d'une liste des listes
      ou chaque sous-liste est un mot si le le langage est fini, None sinon*)
let rec enumerate alphabet e =
  match is_finite e with
  | true ->  (*le langage est fini*)
    (match e with 
    | Eps -> Some [[]]
    | Base a -> Some [[a]]
    | Joker -> Some (List.map (fun x -> [x]) alphabet) (*renvoyer chaque element de l'alphabet comme mot*)
    | Concat (a, b) ->  (*a et b sont des expressions*)
      (match (enumerate alphabet a, enumerate alphabet b) with (*former les langages reconnus par les deux exp a et b*)
      | (Some la, Some lb) -> Some (product la lb)  (*le produit cartésien des deux langages*)
      | _ -> None) (*si l'une des expressions a ou b n'est pas fini*)
    | Alt (a, b) ->
      (match (enumerate alphabet a, enumerate alphabet b) with
      | (Some la, Some lb) -> Some (la @ lb) (*la concat des deux langages*)
      | _ -> None) (*si l'une des expressions n'est pas fini*)
    | Star a -> None)  
  | false-> None  (*le langage n'est pas fini*)



 (*l'ensemble des lettres qui apparaient dans l'expression*) 
let rec alphabet_expr e =
  match e with
    | Eps -> []
    | Base a -> [a]  
    | Joker -> []
    | Concat (a, b) -> sort_uniq (alphabet_expr a @ alphabet_expr b)  (*concatener la liste des lettres de l'expressions a et celle de b et la sorter pour éliminer les doublons*)
    | Alt (a, b) -> sort_uniq (alphabet_expr a @ alphabet_expr b)  (*concatener la liste des lettres de l'expressions a et celle de b et la sorter pour éliminer les doublons*)
    | Star a -> sort_uniq ( alphabet_expr a)




type answer =
  Infinite | Accept | Reject



(*fonction qui cherche l'element x dans la liste l, si il existe elle renvoie true, false sinon*)
let rec trouve x l =
  match l with
  |[] -> false
  |a::b ->  if a=x then true else trouve x b

 (*renvoie answer selon l'expression entrée*) 
let accept_partial e w =
  match is_finite e with
  | false -> Infinite (*le langage reconnu par e n'est pas fini*)
  | true ->  (*le langage reconnu par e est fini*)
     let alphabet = sort_uniq ((alphabet_expr e) @ w)  (*former l'alphabet de l'expression*)
    in 
    (match enumerate alphabet e with  (*reccuperer la liste des mots reconnu par l'expression*)
    | None -> Reject  
    | Some language -> (
      if w = [] then (if( null e) then  Accept else Reject)
  else
    if(trouve w language) then Accept else Reject (*tester si le mot w appartien au ensmble des mots ou non*)
    )  
    )
  
  



