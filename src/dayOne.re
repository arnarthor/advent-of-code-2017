let explode = (s) => {
  let rec expl = (i, l) =>
    if (i < 0) {
      l
    } else {
      expl(i - 1, [s.[i], ...l])
    };
  List.map(Char.escaped, expl(String.length(s) - 1, []))
};

module PartOne = {
  type input = string;
  type answer = int;
  let cases = [("1122", 3), ("1111", 4), ("1234", 0), ("91212121219", 9)];
  let solve = (str) => {
    let chars = explode(str);
    let first = List.hd(chars);
    let total =
      List.fold_left(
        ((total, prev), current) =>
          if (current == prev) {
            (total + int_of_string(current), current)
          } else {
            (total, current)
          },
        (0, first),
        List.rev(chars)
      )
      |> fst;
    Printf.printf("Total: %d\n", total);
    total
  };
};

module PartTwo = {
  type input = string;
  type answer = int;
  let cases = [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)];
  let solve = (str) => {
    let chars = explode(str);
    let partitionList = () => {
      let (firstWithIndex, secondWithIndex) =
        List.partition(
          ((_, index)) => index < List.length(chars) / 2,
          List.mapi((index, value) => (value, index), chars)
        );
      (
        Array.of_list @@ List.map(fst, firstWithIndex),
        Array.of_list @@ List.map(fst, secondWithIndex)
      )
    };
    let (first, second) = partitionList();
    let total =
      Array.fold_left(
        ((total, index), current) =>
          if (current == second[index]) {
            (total + int_of_string(current) * 2, index + 1)
          } else {
            (total, index + 1)
          },
        (0, 0),
        first
      )
      |> fst;
    Printf.printf("Total: %d\n", total);
    total
  };
};
