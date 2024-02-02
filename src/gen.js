console.log(
    new Array(31)
    .fill(0)
    .map(
        (_, i) =>
            i
    )
    .reduce(
        (acc, i) =>
            `${acc}Vector31.Index${i} ->
    Col${i}\n`,
        ""
    )
)

// Row0 ->
//             Vector29.Index0

//         Row1 ->
//             Vector29.Index1

//         Row2 ->
//             Vector29.Index2