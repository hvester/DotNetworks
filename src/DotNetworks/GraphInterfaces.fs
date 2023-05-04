namespace DotNetworks

type IImplicitGraph<'V, 'E> =
    abstract member IterSuccessors: 'V * ('V -> 'E -> unit) -> unit