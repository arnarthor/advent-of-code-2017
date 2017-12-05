let stringArray = (s) => {
  let rec expl = (i, l) =>
    if (i < 0) {
      l
    } else {
      expl(i - 1, [s.[i], ...l])
    };
  List.map(Char.escaped, expl(String.length(s) - 1, []))
};

let run = (str) => {
  let s = stringArray(str);
  let strArr = Array.of_list(s);
  let lastChar = Array.length(strArr) > 0 ? Some(strArr[Array.length(strArr) - 1]) : None;
  let rec innerRun = (~stringArray, ~sum) =>
    switch (stringArray, lastChar) {
    | ([], _) => sum
    | ([x], Some(lastChar)) when List.length(s) == 1 || List.length(s) > 1 && lastChar != x => sum
    | ([x], Some(lastChar)) when List.length(s) == 1 || List.length(s) > 1 && lastChar == x =>
      sum + 2 * int_of_string(x)
    | (_, None) => sum
    | ([x, xx, ...tail], _) when x == xx =>
      sum + innerRun(~stringArray=tail, ~sum) + 2 * int_of_string(x)
    | ([_, _, ...tail], _) => innerRun(~stringArray=tail, ~sum)
    | (_, _) => sum
    };
  innerRun(~stringArray=s, ~sum=0)
};
