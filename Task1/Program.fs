type Section =
    {
        Begin: float
        End: float;
    }

let getSectionsWithRoots N f domain =
    printfn $"Start looking for segments of the sign change of the function with the parameter N = %A{N}"
    printfn $"Domain:\n%A{domain}"
    
    let h = (domain.End - domain.Begin) / N
    printfn $"h = %A{h}"
   
    let sectionsWithRoot =
        domain.Begin
        |> Seq.unfold
               (fun a -> let b = a + h
                         if b <= domain.End
                         then
                             let section = {Begin = a; End = b}
                             Some (section, b)
                         else None)
        |> Seq.filter
               (fun section -> if f section.End * f section.Begin <= 0.
                               then
                                   printfn $"Found section with root:\n%A{section}"
                                   true
                               else
                                   false)
        |> List.ofSeq
    
    printfn $"Number of sections of function sign reversal: %A{List.length sectionsWithRoot}"
    sectionsWithRoot
    
let applyBisectionMethod eps f sectionWithRoot =
    printfn $"Start applying bisection method with eps=%A{eps} to section:\n%A{sectionWithRoot}"
    printfn $"Initial approximation to the root: %A{sectionWithRoot.End - sectionWithRoot.Begin}"
    
    let rec applyBisectionMethodRec i section =
        if section.End - section.Begin <= 2. * eps
        then
            (section, i)
        else
            let c = (section.Begin + section.End) / 2.
            if f section.Begin * f c <= 0.
            then
                applyBisectionMethodRec (i + 1) {Begin = section.Begin; End = c}
            else
                applyBisectionMethodRec (i + 1) {Begin = c; End = section.End}
                
    let bisect, numberOfSteps = applyBisectionMethodRec 0 sectionWithRoot
    
    printfn $"Number of steps of bisection method: %A{numberOfSteps}"
    printfn $"Length of the last section of bisection method: %A{(bisect.End - bisect.Begin) / 2.}"
    
    let approximatedSolution = (bisect.End + bisect.Begin) / 2.
    
    printfn $"Approximated solution = %A{approximatedSolution}"
    printfn $"Absolute value of the discrepancy = %A{approximatedSolution |> f |> abs}"
    
    approximatedSolution
    
let findRoots rootSeparationMethod rootClarificationMethod f domain =
    rootSeparationMethod f domain
    |> List.map (rootClarificationMethod f)

[<EntryPoint>]
let main _ =
    let f x = 1.2 * x ** 4. + 2. * x ** 3. + - 13. * x ** 2. - 14.2 * x - 24.1
    let domain = {Begin = -5.; End = 5.}
    let epsilon = 1e-06
    let N = 10000.
    
    printfn "NUMERICAL METHODS FOR SOLVING NONLINEAR EQUATIONS"
    printfn "Function f(x): 1.2 * x ** 4. + 2. * x ** 3. + - 13. * x ** 2. - 14.2 * x - 24.1"
    
    let rootSeparationMethods = [getSectionsWithRoots N]
    let rootClarificationMethods = [applyBisectionMethod epsilon;]
    
    for rootSeparationMethod in rootSeparationMethods do
        for rootClarificationMethod in rootClarificationMethods do
            findRoots rootSeparationMethod rootClarificationMethod f domain |> ignore

    0