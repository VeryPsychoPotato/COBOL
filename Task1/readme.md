Create cobol module (or amend/expand PGM1) with new functionality.
Input: function code, number1, number2
Output: statusCode and reasonCode (use USSTO1CZ), result

Implement input validation for:

function code
'1' - composition of two numbers
'2' - deduction (higher number - lower)
'3' - multiplication
'4' - factorial for number1. If factorial is higher than result can store, produce error (statusCode + reasonCode)
'5' - factorial for number2. If factorial is higher than result can store, produce error (statusCode + reasonCode)
number1, number2 - must be not higher than PIC 99.
