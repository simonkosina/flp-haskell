Feature: Check if options work properly

  @brute
  Scenario Outline: Brute solver
    Given flp22-fun is available
    When flp22-fun is run with "-b" option and "<in>" file
    Then output matches file "<out>"
    And return code is "0"

    Examples:
      | in              | out             |
      | data/test1.in   | data/test1.out  |
      | data/test2.in   | data/test2.out  |
      | data/test3.in   | data/test3.out  |
      | data/test4.in   | data/test4.out  |
      | data/test5.in   | data/test5.out  |
      | data/test6.in   | data/test6.out  |
      | data/test7.in   | data/test7.out  |
      | data/test8.in   | data/test8.out  |
      | data/test9.in   | data/test9.out  |
      | data/test10.in  | data/test10.out |
      | data/test11.in  | data/test11.out |
      | data/test12.in  | data/test12.out |
      | data/test13.in  | data/test13.out |
  
  @optim
  Scenario Outline: Optim solver
    Given flp22-fun is available
    When flp22-fun is run with "-o" option and "<in>" file
    Then output matches file "<out>"
    And return code is "0"

    Examples:
      | in              | out             |
      | data/test1.in   | data/test1.out  |
      | data/test2.in   | data/test2.out  |
      | data/test3.in   | data/test3.out  |
      | data/test4.in   | data/test4.out  |
      | data/test5.in   | data/test5.out  |
      | data/test6.in   | data/test6.out  |
      | data/test7.in   | data/test7.out  |
      | data/test8.in   | data/test8.out  |
      | data/test9.in   | data/test9.out  |
      | data/test10.in  | data/test10.out |
      | data/test11.in  | data/test11.out |
      | data/test12.in  | data/test12.out |
      | data/test13.in  | data/test13.out |
      | data/test14.in  | data/test14.out |
      | data/test15.in  | data/test15.out |
