Feature: PYLS configuration

  Background: Server initialized
    Given I have workspace folder "pyls"
    And I open file "pyls/test.py"
    And I call "lsp"
    Then the "pyls" status will become "initialized"

  Scenario: Server xref
    Given I place the cursor after "print x"
    When I call "lsp-find-definition"
    And the cursor should be before "x"
