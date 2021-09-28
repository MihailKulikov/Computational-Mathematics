open System

type Section =
    {
        Begin: float
        End: float
    }

type DividedDifference =
    {
        BeginX: float
        EndX: float
        Value: float
    }

let getArgumentToValueList f section tableSize =
    let delta = (section.End - section.Begin) / (tableSize - 1.)
    let rec generateTable x =
        seq {
            let newX = x + delta
            if x <= section.End
            then
                let value = f x
                printfn $"%A{x} -> %A{value}"
                yield x, value
                yield! generateTable <| newX
        }

    section.Begin |> generateTable |> List.ofSeq

let rec tryGetMaximumPolynomialDegree table =
    printfn "Введите максимальную степень интерполяционного многочлена"
    let maximumPolynomialDegree = Int32.Parse(Console.ReadLine())
    if maximumPolynomialDegree < Seq.length table
    then
        maximumPolynomialDegree
    else
        printfn $"Максимальная степень интерполяционного многочлена не должна превышать %A{Seq.length table - 1}"
        tryGetMaximumPolynomialDegree table
        
let calculateMultiplication arguments x k =
    arguments
    |> Seq.indexed
    |> Seq.filter (fun (i, _) -> i <> k)
    |> Seq.map (fun (_, arg) -> (x - arg))
    |> Seq.reduce (*)
   
let getLagrangeFormResult table x =
     printfn "Начинаю применять метод инторполяции методом Лагранжа"

     let arguments = table |> Seq.map fst

     table
     |> Seq.indexed
     |> Seq.map (fun (i, (arg, value)) ->
         (calculateMultiplication arguments x i) / (calculateMultiplication arguments arg i) * value)
     |> Seq.sum

let calculateNewDividedDifference reducedDividedDifference deductibleDividedDifference =
    {
        BeginX = deductibleDividedDifference.BeginX
        EndX = reducedDividedDifference.EndX
        Value =
            (reducedDividedDifference.Value - deductibleDividedDifference.Value)
            / (reducedDividedDifference.EndX - deductibleDividedDifference.BeginX)
    }
     
let interpolationPolynomialCoefficientsSeq table =
    let getNextDividedDifferences previousDividedDifferences =
        let reduced = previousDividedDifferences |> Seq.skip 1
        Seq.map2 calculateNewDividedDifference reduced previousDividedDifferences

    let coefficientsNumber = Seq.length table
    let rec interpolationPolynomialCoefficientsSeqRec i dividedDifferences =
        seq {
            if i < coefficientsNumber
            then
                yield (Seq.head dividedDifferences).Value
                
                yield! dividedDifferences
                       |> getNextDividedDifferences
                       |> interpolationPolynomialCoefficientsSeqRec (i + 1)
        }

    table
    |> Seq.map (fun (arg, value) -> {BeginX = arg; EndX = arg; Value = value})
    |> interpolationPolynomialCoefficientsSeqRec 0

let getNewtonFormResult table x =
    printfn "Начинаю применять метод интерполяции методом Ньютона"

    table
    |> Seq.take (Seq.length table - 1)
    |> Seq.map fst
    |> Seq.map (fun arg -> x - arg)
    |> Seq.mapFold (fun state value -> (state * value, state * value)) 1.
    |> fst
    |> Seq.append (Seq.singleton 1.)
    |> Seq.map2 (*) <| interpolationPolynomialCoefficientsSeq table
    |> Seq.sum

let rec interpolate interpolationMethods f table =    
    printfn "Введите точку интеполирования"
    let x = Double.Parse(Console.ReadLine())

    let maximumPolynomialDegree = tryGetMaximumPolynomialDegree table

    let firstNearestArgValuePairsToInterpolationPoint =
        table
        |> Seq.sortBy (fun (arg, _) -> abs <| arg - x)
        |> Seq.take (maximumPolynomialDegree + 1)

    printfn "Набор узлов ближайших к точке x, по которым будет строиться интерполяционный многочлен"
    Seq.iter (fun (arg, value) -> printfn $"%A{arg} -> %A{value}") firstNearestArgValuePairsToInterpolationPoint

    for interpolationMethod in interpolationMethods do
        let result = interpolationMethod firstNearestArgValuePairsToInterpolationPoint x
        let absoluteError = abs <| result - f x

        printfn $"Результат: %A{result}. Абсолютная ошибка: %A{absoluteError}"
        printfn ""

    printfn "Повторить для других x и n? Введите Yes или No"
    let answer = Console.ReadLine()
    if answer.ToLower() = "yes"
    then
        interpolate interpolationMethods f table

[<EntryPoint>]
let main _ =
    printfn "Задача алгебраического интерполирования"
    printfn "Вариант 13"
    printfn ""

    let f x = log (1. + x) - exp x

    printfn "Введите число значение в таблице"
    let tableSize = Double.Parse(Console.ReadLine())

    printfn "Введите начало отрезка, из которого выбираются узлы интерполирования"
    let sectionBegin = Double.Parse(Console.ReadLine())

    printfn "Введите конец отрезка, из которого выбираются узлы интерполирования"
    let sectionEnd = Double.Parse(Console.ReadLine())
    let section = { Begin = sectionBegin; End = sectionEnd; }
    let table = getArgumentToValueList f section tableSize
    let interpolationMethods = [getLagrangeFormResult; getNewtonFormResult]

    interpolate interpolationMethods f table

    0
