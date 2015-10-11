Feature: Editor features
  In order test editor features
  As an user
  I want replace text and test table2text

  Scenario: Replace text
    Given a text like that:
      """
      Hello dolly
      -----------
      """
    When I replace 'Hello' with 'Bye'
    Then the text will be like that:
      """
      Bye dolly
      -----------
      """

  Scenario: Table2Text
    Given a users this table like that:
      | roberto  | openshine |
      | telemaco | gnome     |
    When I press table2text
    Then i've a text like that:
      """
      robertoopenshine
      telemacognome
      
      """
  Scenario Outline: Table2Text
    Given a users this table like that:
      | <input_1>  | <input_2> |
    When I press table2text
    Then i've a text like that:
      """
      <output>

      """

    Examples:
      | input_1   | input_2   | output              |
      | roberto   | openshine | robertoopenshine    |

