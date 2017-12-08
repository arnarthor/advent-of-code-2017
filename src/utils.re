let explode = (s) => {
  let rec expl = (i, l) =>
    if (i < 0) {
      l
    } else {
      expl(i - 1, [s.[i], ...l])
    };
  List.map(Char.escaped, expl(String.length(s) - 1, []))
};
