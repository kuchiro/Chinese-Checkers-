let dim = 3

  type case = int * int * int;;

  type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors
  (*case en dehors du plateau, utile pour l'affichage*)
  | Nombre of int
  (*pour mettre des petits noms*)
  | Nom of string

  let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"
  | Nombre n -> string_of_int n
  (*pour mettre des petits noms*)
  | Nom s -> s

  type case_coloree = case * couleur

  type liste_joueur = couleur list

  type configuration = case_coloree list * couleur list

  type coup = Du of case * case | Sm of case list

  let liste_joueurs (_, l) = l

  let gagnant _ = Libre

  let est_dans_losange = fun (i,j,k:case) ->
  if (i > 2*dim || i < 2* -dim) then false
  else if (k> dim || k < - dim || i+ j = -dim - 1) then false
  else if (j> dim || j < - dim  || i+ k = -dim -1) then false
  else if (i+j+k = 0) then true 
  else false;;

  let est_dans_etoile = fun (c:case) ->
  match c with
    (i,j,k) -> if ((est_dans_losange c || j <= dim && i < dim +1 && k <= dim && i < dim +1 || j >= -dim && i > -dim -1 && k >= -dim && i > -dim -1 ) && i+j+k = 0) then true
    else false

  (*let quelle_couleur = fun c -> fun conf -> 
  if (est_dans_etoile c = true) then Libre else Dehors;;*)

  let tourne_case = fun (m:int) -> fun (c:case) -> match c with 
    (i,j,k) -> match m with
      m -> match m mod 6 with 
        1 -> (i+j,j+k,k+i)
      | 2 -> (j,k,i) 
      | 3 -> if (i >= dim + 1 || i <=  - dim +1) then(-i,-j,-k) 
        else (i,k,j) 
      |4 ->(k,i,j)  
      |5 -> (-j,-k,-i)  
      | _ -> (i,j,k);
  ;;


  let rec swap_aux = fun joueur_list -> fun x ->
  match joueur_list with 
  |t::q -> t :: swap_aux q x 
  |[] -> [x]
  ;; 
  let swap = fun l ->
  match l with 
    [] -> []
  |t::q -> (swap_aux q t )
  ;;

  let rec tourne_all_case m (case_coloree)=
  match case_coloree with
    (i, j, k), couleur ->
    if m = 0 then (i, j, k), couleur
    else tourne_all_case (m-1) ((-k, -i, -j), couleur)
  ;;

  let rec tourne_cases m (case_coloree_l) =
  match case_coloree_l with
  | [] -> []
  | t::q -> tourne_all_case m t::tourne_cases m q

  let tourne_config (cases, couleurs: configuration): configuration =
  tourne_cases 1 cases, swap couleurs
  ;;

  let sont_cases_voisines = fun (c:case) -> fun (c1:case) -> match c with 
    (i,j,k) -> match c1 with 
    |(i1,j1,k1) -> if ((i, j+1,k-1) = c1) then true 
      else if ((i+1,j,k-1) = c1) then true
      else if ((i+1,j-1,k) = c1) then true
      else if ((i-1,j,k+1) = c1) then true
      else if ((i-1,j+1,k) = c1) then true
      else if((i,j-1,k+1) = c1) then true
      else false
  ;;

  let rec case_dans_config (c:case) (config:configuration) =
  match config with
  |[], [] -> false
  |[], col -> false
  |casecl, [] -> false
  |((case,couleur)::casecl), (couleur2::couleur_list) -> 
    if (case=c && (couleur2!=Dehors && couleur2!=Libre)) then true
    else case_dans_config c (casecl, couleur_list);;

  let rec quelle_couleur = fun (c:case) -> fun (config:configuration) ->
  match config with
  |([],_) -> if est_dans_etoile c then Libre else Dehors
  |(case2,couleur) :: q, joueur_list -> 
    if c = case2 && est_dans_etoile c then couleur
    else quelle_couleur c (q, joueur_list);;


  let rec remplir_triangle = fun (conf:configuration) -> fun (coul:couleur) -> fun (c:case) ->
  match c with
  |(i,j,k) ->
    let (x,y)=conf in 
    if (i>= -2*dim) && (i < -dim) && (i+j+k = 0) then remplir_triangle 
        (remplir_triangle ((c,coul)::x ,y) coul (i+1,j-1,k)) 
        coul (i+1,j,k-1)
    else conf;; 

  let rec remplir_init_aux conf jl = 
  match jl with
  |[] -> conf
  |t::q -> tourne_config (remplir_triangle (remplir_init_aux conf q) t (-(2*dim), dim, dim)) ;; 
  let rec remplir_init jl = remplir_init_aux ([],jl) (List.rev jl);;

  let configuration_initial = remplir_init [Vert; Jaune; Rouge; Noir; Bleu; Marron];;


  let rec couleur_case (conf: configuration) c = match conf with 
    cases_colorees, joueurs -> match cases_colorees with
    | [] -> Libre
    | t::q -> match t with
      | case, couleur ->
        if case = c then couleur
        else couleur_case (q, joueurs) c
  ;;

  let est_dep_unit (conf: configuration) c1 c2 = match conf with 
    cases_colorees, joueurs -> match joueurs with
    | [] -> false
    | joueur1::q -> 
      if (sont_cases_voisines c1 c2) && (couleur_case conf c1 = joueur1) && 
         (couleur_case conf c2 = Libre) && (est_dans_losange c2) then true
      else false

  let rec remplace_case cases_colorees c1 c2 =
  match cases_colorees with
  | [] -> []
  | t::q -> match t with
    | case, couleur ->
      if case = c1 then (c2, couleur)::q
      else t::remplace_case q c1 c2
  ;;
  let fait_dep_unit (conf: configuration) c1 c2 = match conf with 
    cases_colorees, joueurs -> (remplace_case cases_colorees c1 c2), joueurs
  ;;

  let mis_a_jour_configuration = fun conf -> fun cp -> match cp with 
    Du (c1,c2) -> if (est_dep_unit conf c1 c2 = true) then 
      Ok (tourne_config (fait_dep_unit conf c1 c2)) 
    else Error "Erreur coup invalide"
  | Sm cl-> Error "Erreur" 
  ;;
  let configuration_initial = remplir_init [Vert; Jaune; Rouge; Bleu; Marron;Noir; ];;
