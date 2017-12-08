module DayOnePartOneSolution = Solution.Test(DayOne.PartOne);

module DayOnePartTwoSolution = Solution.Test(DayOne.PartTwo);

Printf.printf("%s\n", string_of_bool @@ DayOnePartOneSolution.check());

Printf.printf("%s\n", string_of_bool @@ DayOnePartTwoSolution.check());

module DayTwoPartOneSolution = Solution.Test(DayTwo.PartOne);

Printf.printf("%s\n", string_of_bool @@ DayTwoPartOneSolution.check());
