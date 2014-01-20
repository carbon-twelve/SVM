open System
open System.Text
open Microsoft.VisualBasic.FileIO
open FSharpx.Prelude

let undefined () = System.NotImplementedException() |> raise

type TrainingData = {
    Example: vector;
    Label: int
}

type Classifier = vector -> int

let eps = 1e-5
let floatEquals (x: float) (y: float): bool = Math.Abs(x - y) < eps
let convergenceEps = 0.1
let floatConverges (x: float) (y: float): bool = Math.Abs(x - y) < convergenceEps

// 最急降下法による1-ノルムソフトマージンSVM
let oneNormMarginSteepestAscent (trainingSet: TrainingData seq) (kernel: vector -> vector -> float) (boxConstraint: float): Classifier =
    let r: float = Seq.map (fun d -> Vector.norm d.Example) trainingSet |> Seq.max
    // バイアスを0とするために入力ベクトルを拡張する
    let extendedTrainingSet: (TrainingData * int) seq =
        Seq.map (fun d -> {d with Example = Vector.toArray d.Example |> flip Array.append [|r|] |> Vector.ofArray}) trainingSet
        |> flip Seq.zip (Seq.initInfinite id)
    // バイアスを0とするために適応カーネルを使用する
    let kernel (x1: vector) (x2: vector): float = kernel x1 x2 + r * r
    let f (dual: vector) (x: vector): float =
        Seq.map (fun (d, i) -> (float) d.Label * dual.[i] * kernel d.Example x) extendedTrainingSet
        |> Seq.sum
    let learningRate: vector =
        Seq.map (fun (d, _) -> 1. / kernel d.Example d.Example) extendedTrainingSet
        |> Vector.ofSeq
    let rec iterative (dual: vector): vector =
        for (data, index) in extendedTrainingSet do
            let gradient: float = 1. - (float) data.Label * Seq.sum (Seq.map (fun (d, i) -> dual.[i] * (float) d.Label * kernel data.Example d.Example) extendedTrainingSet)
            dual.[index] <- min boxConstraint (max 0. (dual.[index] + learningRate.[index] * gradient))
        let converges = 
            let checkKkt =
                seq {
                    for (data, index) in extendedTrainingSet do
                        let kkt = (float) data.Label * f dual data.Example
                        if floatEquals dual.[index] 0. then
                            yield kkt >= 1.
                        else if floatEquals dual.[index] boxConstraint then
                            yield kkt <= 1.
                        else
                            yield floatConverges kkt 1.
                }
            let numKkt = Seq.filter id checkKkt|> Seq.length
            Printf.printfn "KKT = %d" numKkt
            checkKkt|> Seq.reduce (&&)
        if converges then dual else iterative dual
    let l: int = Seq.length extendedTrainingSet
    let dual: vector = iterative (Vector.zero l)
    let classifier (x: vector): int =
        if f dual x >= 0. then 1 else -1
    classifier

let getTrainingSetFromCsv (path: string) : TrainingData list =
    use parser: TextFieldParser =
        let parser = new TextFieldParser(path, Encoding.UTF8)
        parser.TextFieldType <- FieldType.Delimited
        parser.SetDelimiters(",")
        parser
    Seq.unfold
        (fun (parser: TextFieldParser) ->
            if parser.EndOfData then None else
            let record = parser.ReadFields()
            if Array.isEmpty record then None else
            let label: int = Int32.Parse(record.[Array.length record - 1])
            let example: vector =
                Seq.map Double.Parse (Seq.take (Array.length record - 1) record)
                |> Vector.ofSeq
            Some ({Example = example; Label = if label = 1 then 1 else -1}, parser))
        parser
    |> List.ofSeq

let datasetPath = @"..\..\..\dataset\haberman.data"

[<EntryPoint>]
let main argv =
    let dataSet = getTrainingSetFromCsv datasetPath
    let trainingSet = Seq.take 200 dataSet
    let testSet = Seq.skip 200 dataSet
    let start = DateTime.Now
    let classifier = oneNormMarginSteepestAscent trainingSet Vector.dot 1e-4
    let finish = DateTime.Now
    let testSetWithAnswers = Seq.map classifier (Seq.map (fun d -> d.Example) testSet) |> Seq.zip testSet
    let (precision, recall) =
        let update (tp, fp, fn) (data, ans) =
            tp + (if ans = 1 && data.Label = 1 then 1 else 0),
            fp + (if ans = 1 && data.Label <> 1 then 1 else 0),
            fn + (if ans = 0 && data.Label = 1 then 1 else 0)
        let (tp, fp, fn) = Seq.fold update (0, 0, 0) testSetWithAnswers
        ((float) tp / (float) (tp + fp), (float) tp / (float) (tp + fn))
    Console.WriteLine("time = {0}; precision = {1}; recall = {1}", (finish - start).TotalSeconds, precision, recall)
    0 // 整数の終了コードを返します
