Feature: PYLS configuration

  Background: Server initialized
    Given I have workspace folder "pyls"
    And I open file "pyls/test.py"
    And I call "lsp"
    Then the "pyls" status will become "initialized"

  Scenario: xref definitions
    Given I place the cursor after "print x"
    When I call "lsp-find-definition"
    Then the cursor should be before "x = 10"

  Scenario: xref references
    Given I place the cursor before "x = 10"
    When I select item "2" from the next xref call
    And I call "lsp-find-references"
    Then the cursor should be after "print "

  Scenario: imenu
    Given I go to point "0"
    When I goto imenu "Function" -> "fn1"
    Then the cursor should be before "def fn1():"
