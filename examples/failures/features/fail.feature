Feature: Reporting Failures
  In order to make silly mistakes visible
  As an user
  I want to be told about failing steps

  Scenario: Fail a given step
    Given a step that works
    When I come across a failing step
    Then this step must be disabled
    And this one too

  Scenario: Fail a given step with an exception
    Given a step that works
    When this step throw an exception and fail
    Then this step must be disabled
    And this one too
