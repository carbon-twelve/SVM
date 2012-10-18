type TrainingData = {
    example: vector;
    label: int
}

type HyperPlane = {
    weight: vector;
    bias: float
}

let perceptron (trainingSet: seq<TrainingData>) (lerningRate: float) (exampleDimension: int): HyperPlane =
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
                let newWeight = p.weight + lerningRate * (float) d.label * d.example
                let newBias = p.bias + lerningRate * (float) d.label * rSquare
                {weight = newWeight; bias = newBias}
            else
                p
        let updatedPlane = Seq.fold update p trainingSet
        if p = updatedPlane then p else loop updatedPlane
    loop {weight = Vector.zero exampleDimension; bias = 0.}

[<EntryPoint>]
let main argv = 
    let trainingSet: seq<TrainingData> =
        [
            {example = Vector.ofList [0.1; 0.15]; label = 1};
            {example = Vector.ofList [0.2; 0.22]; label = 1};
            {example = Vector.ofList [0.3; 0.4]; label = 1};
            {example = Vector.ofList [0.6; 0.61]; label = 1};
            {example = Vector.ofList [0.7; 1.0]; label = 1};
            {example = Vector.ofList [0.9; 0.94]; label = 1};
            {example = Vector.ofList [0.1; 0.09]; label = -1};
            {example = Vector.ofList [0.3; 0.2]; label = -1};
            {example = Vector.ofList [0.5; 0.48]; label = -1};
            {example = Vector.ofList [0.6; 0.55]; label = -1};
            {example = Vector.ofList [0.8; 0.77]; label = -1}
        ]
        |> Seq.ofList
    let p = perceptron trainingSet 0.01 2
    printf "weight = %A, bias = %f\n" p.weight p.bias
    0