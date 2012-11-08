Feature: Simple Calculation 
  In order to avoid silly mistakes
  As a math idiot
  I want to do math operations

  Scenario: Add two numbers
    Given I have entered 50 into the calculator
    And I have entered 70 into the calculator
    When I press add
    Then the result should be 120 on the screen

  Scenario: Multiply two numbers
    Given I have entered 50 into the calculator
    And I have entered 70 into the calculator
    When I press multiply
    Then the result should be 3500 on the screen
