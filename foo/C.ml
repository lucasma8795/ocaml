let l : D.t list = [D.zero; D.one; D.two; D.three; D.four]
let f : D.t -> D.number = D.qux
let baz : D.number list = List.map f l
