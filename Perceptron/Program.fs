open Microsoft.VisualBasic.FileIO
open System.Text
open System

type TrainingData = {
    example: vector;
    label: int
}

type HyperPlane = {
    weight: vector;
    bias: float
}

let perceptron (trainingSet: TrainingData seq) (learningRate: float) (exampleDimension: int) : HyperPlane =
    let rSquare: float =
        let r =
            Seq.map (fun (d: TrainingData) -> Vector.norm d.example) trainingSet
            |> Seq.max
        r * r
    let rec loop (p : HyperPlane) : HyperPlane =
        let update (p : HyperPlane) (d: TrainingData) : HyperPlane =
            let fails (p: HyperPlane) (d: TrainingData) : bool =
                (float) d.label * (Vector.dot p.weight d.example + p.bias) <= 0.
            if fails p d then
                let newWeight = p.weight + learningRate * (float) d.label * d.example
                let newBias = p.bias + learningRate * (float) d.label * rSquare
                {weight = newWeight; bias = newBias}
            else p
        let updatedPlane = Seq.fold update p trainingSet
        if p = updatedPlane then p else loop updatedPlane
    loop {weight = Vector.zero exampleDimension; bias = 0.}

let trainingSetFilePath = @"..\..\etc\training_set.csv"

(*
The file format is as follows:

label = "1" | "-1";
element = ?floating point number?;
example = element, {",", element};
field = label, ",", example, ?line separator?;
file = field, {field}, ?end of file?;

Each example vector must have the same dimension.
*)
let getTrainingSetFromCsv (path: string) : TrainingData list =
    use parser: TextFieldParser =
        let parser = new TextFieldParser(path, Encoding.UTF8)
        parser.TextFieldType <- FieldType.Delimited
        parser.SetDelimiters(",")
        parser
    Seq.unfold
        (fun (parser: TextFieldParser) ->
            if parser.EndOfData then None else
            match parser.ReadFields() |> List.ofArray with
            | hd :: tl ->
                let label: int = Int32.Parse(hd)
                let example: vector = List.map Double.Parse tl |> Vector.ofList
                Some ({example = example; label = label}, parser)
            | [] -> None)
        parser
    |> List.ofSeq

[<EntryPoint>]
let main argv =
    let trainingSet: TrainingData list = getTrainingSetFromCsv trainingSetFilePath
    let p: HyperPlane = perceptron trainingSet 0.01 2
    printf "weight = %A, bias = %f\n" p.weight p.bias
    0