open System

type Section =
    {
        Begin: float
        End: float
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
    if maximumPolynomialDegree < List.length table
    then
        maximumPolynomialDegree
    else
        printfn $"Максимальная степень интерполяционного многочлена не должна превышать %A{List.length table - 1}"
        tryGetMaximumPolynomialDegree table
        
let calculateMultiplication table x k =
    table
    |> Seq.ofList
    |> Seq.mapi (fun i (arg, _) -> (i, arg))
    |> Seq.filter (fun (i, _) -> i <> k)
    |> Seq.map (fun (i, arg) -> (x - arg))
    |> Seq.reduce (*)
   
let getLagrangeFormResult table x =
     printfn "Начинаю применять метод инторполяции методом Лагранжа"

     let result =
         table
         |> List.mapi (fun i (arg, value) -> i, (arg, value))
     let check = calculateMultiplication table x 1
     let kek =
         result
         |> List.map (fun (i, (arg, value)) ->
             (calculateMultiplication table x i) / (calculateMultiplication table arg i) * value)
     
     let lul = kek |> List.sum

     lul
  
let rec interpolate f table interpolationMethods =    
    printfn "Введите точку интеполирования"
    let x = Double.Parse(Console.ReadLine())

    let maximumPolynomialDegree = tryGetMaximumPolynomialDegree table

    let firstNearestArgValuePairsToInterpolationPoint =
        table
        |> List.sortBy (fun (arg, _) -> abs <| arg - x)
        |> List.take (maximumPolynomialDegree + 1)

    printfn "Набор узлов ближайших к точке x, по которым будет строиться интерполяционный многочлен"
    List.iter (fun (arg, value) -> printfn $"%A{arg} -> %A{value}") firstNearestArgValuePairsToInterpolationPoint
    
    for interpolationMethod in interpolationMethods do
        let result = interpolationMethod firstNearestArgValuePairsToInterpolationPoint x
        let absoluteError = abs <| result - f x
        
        printfn $"Результат: %A{result}. Абсолютная ошибка: %A{absoluteError}"
        printfn ""
    
    printfn "Повторить для других x и n? Введите Yes или No"
    let answer = Console.ReadLine()
    if answer.ToLower() = "yes"
    then
        interpolate f table interpolationMethods

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
    let interpolationMethods = [getLagrangeFormResult]
    
    interpolate f table interpolationMethods
    
    0
