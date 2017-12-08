module type Solution = {
  type input;
  type answer;
  let cases: list((input, answer));
  let solve: (input, answer) => answer;
};

module Test = (S: Solution) => {
  let check = () =>
    List.(
      S.cases |> map(((input, output)) => S.solve(input, output) == output) |> for_all((v) => v)
    );
};
