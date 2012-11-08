Feature: Simple Calculation with Background 
  In order to avoid silly mistakes
  As a math idiot
  I want to do math operations

  Background:
    Given I have entered 888 into the calculator
    And I have entered 888 into the calculator

  Scenario: Add two numbers
    When I press add
    Then the result should be 1776 on the screen

  Scenario: Multiply two numbers
    When I press multiply
    Then the result should be 788544 on the screen
