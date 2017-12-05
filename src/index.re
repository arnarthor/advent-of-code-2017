module DayOnePartOneSolution = Solution.Test(DayOne.PartOne);

module DayOnePartTwoSolution = Solution.Test(DayOne.PartTwo);

Printf.printf("%s\n", string_of_bool @@ DayOnePartOneSolution.check());

Printf.printf("%s\n", string_of_bool @@ DayOnePartTwoSolution.check());
