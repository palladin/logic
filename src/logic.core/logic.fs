namespace logic.core
open System
open System.Collections.Generic
open System.Linq

module Logic = 

    // Type defs
    type VarName = string
    type Term = 
        | Var of VarName 
        | Pair of Term * Term 
        | Int of int 
        | Str of string
        | Empty


    type Subst = (VarName * Term) list
    type State = Subst * int
    type Goal<'T> = State -> seq<State * 'T>

    // walk : Term -> Subst -> Term
    let rec walk term subst =
        match term with
        | Var var -> 
            match List.tryFind (fun (var', _) -> var = var') subst with
            | Some (_, term') -> walk term' subst
            | None -> Var var
        | _ -> term

    let extend subst (var, term) = (var, term) :: subst

    // unify : Term -> Term -> Subst -> Subst option
    let rec unify term term' subst =
        match walk term subst, walk term' subst with
        | Var var, Var var' -> 
            if var = var' then 
                Some subst 
            else 
                Some <| extend subst (var, Var var') 
        | Var var, _ -> Some <| extend subst (var, term')
        | _, Var var -> Some <| extend subst (var, term)
        | Pair (leftTerm, rightTerm), Pair (leftTerm', rightTerm') ->
            match unify leftTerm leftTerm' subst with
            | Some subst' -> unify rightTerm rightTerm' subst'
            | None -> None
        | Int v, Int v' -> if v = v' then Some subst else None
        | Str v, Str v' -> if v = v' then Some subst else None 
        | Empty, Empty -> Some subst 
        | _, _ -> None


    // disj : Goal<'T> -> Goal<'T> -> Goal<'T> 
    let disj (goal : Goal<'T>) (goal' : Goal<'T>) =
        fun (state, counter) ->
            let rec interleave (enum : Lazy<IEnumerator<_>>) (enum' : Lazy<IEnumerator<_>>) = 
                seq { 
                    let flag = enum.Force().MoveNext()
                    if flag then
                        yield enum.Force().Current
                    
                    let flag' = enum'.Force().MoveNext()
                    if flag' then
                        yield enum'.Force().Current
                    
                    if flag || flag' then
                        yield! interleave enum enum'
                }

            let enum = lazy ((goal (state, counter)).GetEnumerator())
            let enum' = lazy ((goal' (state, counter)).GetEnumerator())
            interleave enum enum'

    // conde : Goal<'T> list -> Goal<'T>
    let conde list = List.reduce disj list

    // (==) : Term -> Term -> Goal<unit>
    let (==) (term : Term) (term' : Term) : Goal<unit> = 
        fun (subst, counter) ->
            match unify term term' subst with
            | Some subst' -> seq { yield (subst', counter), () }
            | None -> Seq.empty

    // fresh : Goal<Term>
    let fresh : Goal<Term> = fun (subst, counter) -> seq { yield ((subst, counter + 1), (Var (sprintf "var%d" counter))) }
    // fresh2 : Goal<Term * Term>
    let fresh2 : Goal<Term * Term> = fun (subst, counter) -> seq { yield ((subst, counter + 2), (Var (sprintf "var%d" counter), Var (sprintf "var%d" (counter + 1)))) }
    // fresh3 : Goal<Term * Term * Term>
    let fresh3 : Goal<Term * Term * Term> = fun (subst, counter) -> seq { yield ((subst, counter + 3), (Var (sprintf "var%d" counter), Var (sprintf "var%d" (counter + 1)), Var (sprintf "var%d" (counter + 2)))) }


    // print : string -> Term -> Goal<unit>
    let print name (term : Term) = 
         fun (subst, counter) -> 
              printfn "%s = %A" name (walk term subst)
              seq { yield ((subst, counter), ()) } 

    // run : int -> (Term -> Goal<'T>) -> Term list
    let run n (f : Term -> Goal<'T>) = 
        let rec substVars subst term = 
            match term with
            | Var _ -> 
                let term' = walk term subst
                if term = term' then term
                else substVars subst term'
            | Pair (left, right) -> Pair (substVars subst left, substVars subst right)
            | Int _ -> term
            | Str _ -> term
            | Empty -> term  
        let goalVar = Var "__goal__"
        let seq = 
            ([], 0)
            |> f goalVar
        seq.Take(n)
        |> Seq.map (fun ((subst, _), _) -> substVars subst (walk goalVar subst))
        |> Seq.toList


    type LogicBuilder() =

        member this.Zero() = fun _ -> Seq.empty 
        member this.Return v = fun (subst, counter) -> seq { yield ((subst, counter), v) } 
        member this.ReturnFrom goal = goal 
    
        member this.Bind (goal : Goal<'T>, f : 'T -> Goal<'R>) : Goal<'R> =
            fun (subst, counter) -> 
                (subst, counter)
                |> goal
                |> Seq.collect (fun ((subst', counter'), v) -> 
                                    f v (subst', counter'))  

        member this.Delay (f : unit -> Goal<'T>) : Goal<'T> =
            fun (subst, counter) ->
                f () (subst, counter) 
        
    let logic = new LogicBuilder()


    