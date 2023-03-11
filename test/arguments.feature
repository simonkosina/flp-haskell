Feature: Check if options work properly
  
  Scenario Outline: Run with invalid file input
    Given flp22-fun is available
    When flp22-fun is run with "<option>" option and "<in>" file
    Then return code is "1"

    Examples:
      | in                | option |
      | data/invalid1.in  | -i     |
      | data/invalid1.in  | -b     |
      | data/invalid1.in  | -o     |
      | data/invalid2.in  | -i     |
      | data/invalid3.in  | -i     |
      | data/invalid4.in  | -i     |
      | data/invalid5.in  | -i     |
      | data/invalid6.in  | -i     |
      | data/invalid7.in  | -i     |
      | data/invalid8.in  | -i     |
      | data/invalid9.in  | -i     |
      | data/invalid10.in | -i     |
      | data/invalid11.in | -i     |
      | data/invalid12.in | -i     |
      | data/invalid13.in | -i     |
      | data/invalid14.in | -i     |
      | data/invalid15.in | -i     |
      | data/invalid16.in | -i     |
      | data/invalid17.in | -i     |
      | data/invalid18.in | -i     |
      | data/invalid19.in | -i     |
      | data/invalid20.in | -i     |
      | data/invalid21.in | -i     |
      | data/invalid22.in | -i     |

  Scenario Outline: Run with invalid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "<option>" option and "<in>" as stdin
    Then return code is "1"

    Examples:
      | in                | option |
      | data/invalid1.in  | -i     |
      | data/invalid1.in  | -b     |
      | data/invalid1.in  | -o     |
      | data/invalid2.in  | -i     |
      | data/invalid3.in  | -i     |
      | data/invalid4.in  | -i     |
      | data/invalid5.in  | -i     |
      | data/invalid6.in  | -i     |
      | data/invalid7.in  | -i     |
      | data/invalid8.in  | -i     |
      | data/invalid9.in  | -i     |
      | data/invalid10.in | -i     |
      | data/invalid11.in | -i     |
      | data/invalid12.in | -i     |
      | data/invalid13.in | -i     |
      | data/invalid14.in | -i     |
      | data/invalid15.in | -i     |
      | data/invalid16.in | -i     |
      | data/invalid17.in | -i     |
      | data/invalid18.in | -i     |
      | data/invalid19.in | -i     |
      | data/invalid20.in | -i     |
      | data/invalid21.in | -i     |
      | data/invalid22.in | -i     |

  Scenario Outline: Run with valid file input
    Given flp22-fun is available
    When flp22-fun is run with "<option>" option and "<in>" file
    Then output matches file "<out>"
    And return code is "0"

    Examples:
      | in            | out            | option |
      | data/test1.in | data/test1.in  | -i     |
      | data/test1.in | data/test1.out | -b     |
      | data/test1.in | data/test1.out | -o     |

  Scenario Outline: Run with valid stdin input
    Given flp22-fun is available
    When flp22-fin is run with "<option>" option and "<in>" as stdin
    Then output matches file "<out>"
    And return code is "0"

    Examples:
      | in            | out             | option |
      | data/test1.in | data/test1.in   | -i     |
      | data/test1.in | data/test1.out  | -b     |
      | data/test1.in | data/test1.out  | -o     |

  Scenario Outline: Run with nonexistent file input
    Given flp22-fun is available
    When flp22-fun is run with "<option>" option and "bimbam.bum" file
    Then return code is "1"

    Examples:
      | option |
      | -i     |
      | -b     |
      | -o     |

  Scenario: Not passing any options
    Given flp22-fun is available
    When flp22-fun is run with no options
    Then return code is "1"

  Scenario: Passing unkown option
    Given flp22-fun is available
    When flp22-fun is run with "-x" option and "bimbam.bum" file
    Then return code is "1"
