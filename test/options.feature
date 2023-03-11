Feature: Check if options work properly
  
  # TODO: Table for invalid files and run with all
  Scenario: Using -i option with valid file input
    Given flp22-fun is available
    When flp22-fun is run with "-i data/test1.in" arguments
    Then output matches file "data/test1.in"
    And return code is "0"

  Scenario: Using -i option with invalid file input
    Given flp22-fun is available
    When flp22-fun is run with "-i data/invalid1.in" arguments
    Then return code is "1"

  Scenario: Using -i option with nonexistent file input
    Given flp22-fun is available
    When flp22-fun is run with "-i bimbam.bum" arguments
    Then return code is "1"

  Scenario: Using -i option with valid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "-i" option and "data/test1.in" as stdin
    Then output matches file "data/test1.in"
    And return code is "0"

  Scenario: Using -i option with invalid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "-i" option and "data/invalid1.in" as stdin
    Then return code is "1"

  Scenario: Not passing any options
    Given flp22-fun is available
    When flp22-fun is run with no arguments
    Then return code is "1"

  Scenario: Passing unkown option
    Given flp22-fun is available
    When flp22-fun is run with "-x bimbam.bum" arguments
    Then return code is "1"
