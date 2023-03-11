Feature: Check if options work properly
  
  Scenario Outline: Running with invalid file input
    Given flp22-fun is available
    When flp22-fun is run with "<option>" option and "<file>" file
    Then return code is "1"

    Examples:
      | file             | option |
      | data/invalid1.in | -i     |
      | data/invalid1.in | -b     |
      | data/invalid1.in | -o     |

  Scenario Outline: Running with invalid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "<option>" option and "<file>" as stdin
    Then return code is "1"

    Examples:
      | file             | option |
      | data/invalid1.in | -i     |
      | data/invalid1.in | -b     |
      | data/invalid1.in | -o     |

  Scenario: Using -i option with valid file input
    Given flp22-fun is available
    When flp22-fun is run with "-i" option and "data/test1.in" file
    Then output matches file "data/test1.in"
    And return code is "0"

  Scenario: Using -i option with nonexistent file input
    Given flp22-fun is available
    When flp22-fun is run with "-i" option and "bimbam.bum" file
    Then return code is "1"

  Scenario: Using -i option with valid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "-i" option and "data/test1.in" as stdin
    Then output matches file "data/test1.in"
    And return code is "0"

  Scenario: Using -b option with valid file input
    Given flp22-fun is available
    When flp22-fun is run with "-b" option and "data/test1.in" file
    Then output matches file "data/test1.out"
    And return code is "0"

  Scenario: Using -b option with valid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "-b" option and "data/test1.in" as stdin
    Then output matches file "data/test1.out"
    And return code is "0"

  Scenario: Using -o option with valid file input
    Given flp22-fun is available
    When flp22-fun is run with "-o" option and "data/test1.in" file
    Then output matches file "data/test1.out"
    And return code is "0"

  Scenario: Using -o option with valid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "-o" option and "data/test1.in" as stdin
    Then output matches file "data/test1.out"
    And return code is "0"

  Scenario: Not passing any options
    Given flp22-fun is available
    When flp22-fun is run with no options
    Then return code is "1"

  Scenario: Passing unkown option
    Given flp22-fun is available
    When flp22-fun is run with "-x" option and "bimbam.bum" file
    Then return code is "1"
