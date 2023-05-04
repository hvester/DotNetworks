namespace DotNetworks

type ImplicitGraph<'V, 'E> private (iterSuccessors : 'V -> ('V -> 'E -> unit) -> unit) =

    interface IImplicitGraph<'V, 'E> with
        member _.IterSuccessors(vertex, action) = iterSuccessors vertex action

    static member OfIterator(iterSuccessors : 'V -> ('V -> 'E -> unit) -> unit) =
        ImplicitGraph(iterSuccessors)

    static member OfGenerator(generateSuccessors : 'V -> ('V * 'E) seq) =
        let iterSuccessors vertex action =
            for toVertex, edge in generateSuccessors vertex do
                action toVertex edge
        ImplicitGraph(iterSuccessors)


        