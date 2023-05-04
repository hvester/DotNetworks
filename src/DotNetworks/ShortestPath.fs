namespace DotNetworks

open System.Collections.Generic

module ShortestPath =

    type ShortesPathVertexState<'V>(vertex: 'V, accumulatedCost: float, precedingVertex: 'V voption) =
        member _.Vertex = vertex
        member val PrecedingVertex = precedingVertex with get, set
        member val AccumulatedCost = accumulatedCost with get, set
        member val IsClosed = false with get, set

        interface IBinaryHeapHandle<float> with
            member val Priority = 0.0 with get, set
            member val HeapIndex = 0 with get, set


    let private reconstructPath (endVertex: 'V) (visited: Dictionary<'V, ShortesPathVertexState<'V>>) =
        let rec loop acc vertex =
            match visited.TryGetValue(vertex) with
            | true, vertexState ->
                match vertexState.PrecedingVertex with
                | ValueSome(prevVertex) ->
                    loop (prevVertex :: acc) prevVertex
                | ValueNone ->
                    acc
            | _ ->
                failwith "Should never happen: Preceding vertex not found from visited Dictionary"

        loop [ endVertex ] endVertex


    type Dijkstra private () =

        static member FindShortestPath
            (
                graph: IImplicitGraph<'V, 'E>,
                startVertex: 'V,
                endVertex: 'V,
                edgeCost: 'V -> 'E -> 'V -> float
            ) =
            let queue = BinaryHeap<ShortesPathVertexState<'V>, float>()
            let states = Dictionary<'V, ShortesPathVertexState<'V>>()

            let startVertexState = ShortesPathVertexState(startVertex, 0.0, ValueNone)
            queue.Insert(startVertexState, 0.0)
            states.Add(startVertex, startVertexState)

            let rec loop () =
                match queue.TryRemoveMin() with
                | None -> None
                | Some vertexState when vertexState.Vertex = endVertex ->
                    Some (reconstructPath endVertex states)
                | Some vertexState ->
                    vertexState.IsClosed <- true
                    let fromVertex = vertexState.Vertex
                    graph.IterSuccessors(fromVertex, (fun toVertex edge ->
                        match states.TryGetValue(toVertex) with
                        | true, toVertexState when toVertexState.IsClosed ->
                            ()
                        | true, toVertexState ->
                            let cost = edgeCost fromVertex edge toVertex
                            let alternativeCost = vertexState.AccumulatedCost + cost
                            if alternativeCost < toVertexState.AccumulatedCost then
                                toVertexState.AccumulatedCost <- alternativeCost
                                toVertexState.PrecedingVertex <- ValueSome vertexState.Vertex
                                queue.UpdatePriority(toVertexState, alternativeCost)
                        | false, _ ->
                            let cost = edgeCost fromVertex edge toVertex
                            let accumulatedCost = vertexState.AccumulatedCost + cost
                            let toVertexState = ShortesPathVertexState(toVertex, accumulatedCost, ValueSome fromVertex)
                            queue.Insert(toVertexState, accumulatedCost)
                            states.Add(toVertex, toVertexState)
                        ))

                    loop()

            loop()

