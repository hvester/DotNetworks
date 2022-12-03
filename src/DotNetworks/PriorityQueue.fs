namespace DotNetworks

type IBinaryHeapHandle<'Priority> =
    abstract member Priority : 'Priority with get, set
    abstract member HeapIndex : int with get, set

type BinaryHeap<'T, 'Priority when 'T :> IBinaryHeapHandle<'Priority> and 'Priority : comparison>() =
    let heap = ResizeArray<'T>()

    let insertAt index (handle : 'T) =
        heap[index] <- handle
        handle.HeapIndex <- index

    let rec heapifyUp (handle : 'T) =
        if handle.HeapIndex > 0 then
            let parentIndex = handle.HeapIndex / 2
            let parent = heap[parentIndex]
            if parent.Priority > handle.Priority then
                insertAt handle.HeapIndex parent
                handle.HeapIndex <- parentIndex
                heapifyUp handle
            else
                heap[handle.HeapIndex] <- handle
        else
            heap[0] <- handle

    let rec heapifyDown (handle : 'T) =
        let leftChildIndex: int = 2 * handle.HeapIndex
        let rightChildIndex = leftChildIndex + 1
        if leftChildIndex < heap.Count then
            let minChildIndex =
                if rightChildIndex < heap.Count && heap[rightChildIndex].Priority < heap[leftChildIndex].Priority then
                    rightChildIndex
                else
                    leftChildIndex
            
            let minChild = heap[minChildIndex]
            if minChild.Priority < handle.Priority then
                insertAt handle.HeapIndex minChild
                handle.HeapIndex <- minChildIndex
                heapifyDown handle
            else
                heap[handle.HeapIndex] <- handle
        else
            heap[handle.HeapIndex] <- handle

    member _.Insert(handle : 'T, priority : 'Priority) : unit =
        handle.HeapIndex <- heap.Count
        handle.Priority <- priority
        heap.Add(handle)
        heapifyUp handle

    member _.TryRemoveMin() : 'T option =
        if heap.Count = 0 then
            None
        elif heap.Count = 1 then
            let minHandle = heap[0]
            heap.RemoveAt(0)
            Some minHandle
        else
            let minHandle = heap[0]
            let lastHandle = heap[heap.Count - 1]
            heap.RemoveAt(heap.Count - 1)
            insertAt 0 lastHandle
            heapifyDown lastHandle
            Some minHandle

    member _.UpdatePriority(handle : 'T, priority : 'Priority) : unit =
        let oldPriority = handle.Priority
        handle.Priority <- priority
        if priority < oldPriority then
            heapifyUp handle
        else
            heapifyDown handle

    member _.Print() =
        for h in heap do
            printf $"|{h.Priority}, %A{h}"
        printfn "|"
