
let undefined () = System.NotImplementedException() |> raise

type Data = vector
type HyperPlane = vector -> float
type Classifier = vector -> int

let directoryPath: string = undefined ()
let hyperPlanePath: string = sprintf "%s/hyper_plane.txt" directoryPath
let dataPath: string = sprintf "%s/data.csv" directoryPath

let generateHyperPlane (): HyperPlane = undefined ()
let generateData (hyperPlane: HyperPlane): Data list =  undefined ()
let writeHyperPlane (path: string) (hyperPlane: HyperPlane) = undefined ()
let writeData (path: string) (data: Data list) = undefined ()
[<EntryPoint>]
let main argv = 
    let hyperPlane = generateHyperPlane ()
    let data = generateData hyperPlane
    writeHyperPlane hyperPlanePath hyperPlane
    writeData dataPath data
    0