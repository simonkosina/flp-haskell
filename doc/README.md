# Knapsack solver

School project for the [FLP](https://www.fit.vut.cz/study/course/FLP/.cs) course at BUT FIT. To build the project run `make` in the main directory. The target program `flp22-fun` can be used as followed:

`flp22-fun option [file]`

Options are either `-i` to show information about the loaded knapsack problem isntance, `-b` solves the knapsack problem using brute force algorithm and the `-o` option uses a genetic algorithm to solve the problem. Knapsack instance can be either passed in as a file or from `stdin` (see the `test/data/test1.in` file for expected input format).

# Project structure
All the source code files can be found in the `src`directory. `Main.hs` takes care of all the IO actions, loads the input either from file or the `stdin` and parses it to create an internal representation of type `Knapsack`. The created data structure is then passed into the chosen solver algorithm.

All the user defined types and shared functions can be found in the `Helper/Types.hs` and `Helper/Functions.hs` files respectively.

Input parsing is done using the `Parsec` library and all the needed parsers are defined in the `Parser.hs` file.

The actual algorithms used for solving are defined in the `Solver/Brute.hs` and `Solver/Optim.hs` files. The brute force algorithm blindly generates all possible subsets of the passed-in items. These subsets are then filtered to keep only valid solutions (solutions with a total cost greater or equal to the minimum cost and the total weight less or equal to the maximum weight). From all these feasible solutions the one with the highest total cost is chosen.

The optimized solver uses a simple genetic algorithm to find the best solution, which might not always happen. First a population of a given size is randomly generated. To generate a new population we pick parents from the old one using tournaments. During tournaments, a given number of individuals is sampled from the population and fitness function is used to find the highest scoring one. Given two parents, a new child is created by splitting the parents geness in half and possibly mutating some of the genes. The genes are basically bits saying which item from the knapsack problem to take (value 1) and which not to take (value 0).
Tournaments and the crossover phase are repeated until a whole new population of individuals is created. Since we are trying to find the best solution the genetic algorithm ends after a given amount of iterations and then the highest scoring individual is chosen from the population as a proposed solution for the knapsack problem. Other options would be, for example to run the algorithm until a valid solution is found or until the avarage individual fitness score is improving, but there is a chance that these solutions wouldn't terminate.

The mentioned fitness functions checks if a given individual represents a valid solution and if so the total item cost is returned. Otherwise the difference of weights from the maximmal allowed weight or the difference of costs from the minimal cost is returned as a negative number. I've tried experimenting with returing 0 when the conditions are not met, but the negative differences seem to achieve better results.

Right now the hyperparameters are set in a way to have a good chance of finding the right solution but also not take too long for the algorithm to finish. All of the used hyperparameters (`mutationRate`, `numPlayers`, `populationSize`, `iterations`, `seed`) are set in the `Solver/Optim.hs` file and can be further finetuned.

# Tests
Along with the source code a few test cases are provided in the `test` directory. All the input and expected output files can are defined in `test/data`. For the invalid inputs there are no defined outputs as the tests only check if the return code is 1. Tests were created using the [behave](https://behave.readthedocs.io/en/stable/index.html) framework in python. the `arguments.feature` file defines tests that focus on the program interface and the parser. Solver tests are described in the `solver.feature` file. You can run all tests by navigating to the `test` directory and running the `behave` command. To run only one of the feature files you can simply specify the file in the command, e.g. `behave solver.feature`. In the solver tests, the `brute` and `optim` tags are defined to run the tests using only one of the solvers, e.g. `behave -k -t brute`. Knapsack intances used in the brute solver tests are limited in size to keep the runtime short.
