#r "bin/Debug/netstandard2.0/DotNetworks.dll"

open DotNetworks
open ShortestPath

let iter (x, y) f =
    f (x + 1, y) 1.0
    f (x + 1, y + 1) 1.0
    f (x, y + 1) 1.0
    f (x - 1, y) 1.0
    f (x, y - 1) 1.0

let graph = ImplicitGraph.OfIterator(iter)

let path = Dijkstra.FindShortestPath(graph, (0, 0), (10, 10), (fun _ e _ -> e))
