#Erklärungen zu den Anmerkungen von Regeln in elm-review

##Regel `NoForbiddenFeatures`

###Temporär verboten
Diese Funktionen werden später in der Vorlesung behandelt und dürfen erst danach im Code verwendet werden:
- `List.map`


###Generell verboten

- `andThen`: Diese Funktion wird in der Vorlesung nicht vorgestellt. Stattdessen soll die Aufgabe mit anderen Funktionen gelöst werden.
- `Html.class`: Im Rahmen der Vorlesung sollen stattdessen Inline-Style-Attribute via `Html.style` verwendet werden.
