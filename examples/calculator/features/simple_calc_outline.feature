Feature: Simple Calculation Outline
  In order to avoid silly mistakes
  As a math idiot
  I want to do math operations

  Scenario Outline: Do math operations
    Given I have entered <input_1> into the calculator
    And I have entered <input_2> into the calculator
    When I press <button>
    Then the result should be <output> on the screen

    Examples:
     | input_1 | input_2 | button   | output |
     | 20      | 30      | add      | 50     |
     | 2       | 5       | add      | 7      |
     | 0       | 40      | add      | 40     |
     | 10      | 10      | multiply | 100    |
