      */*1----------------------------------------------------------1*/
      */*1 Program/module-description:                              1*/
      */*1 --------------------------                               1*/
      */*1                                                          1*/
      */*1 Program-id    : KVTEST1                                  1*/
      */*1 Program-title : Input/Output                             1*/
      */*1                                                          1*/
      */*1 Programmer    : BF 9798 Kernius Vildziunas               1*/
      */*1 First version : 1                                        1*/
      */*1                                                          1*/
      */*1----------------------------------------------------------1*/


      */*2----------------------------------------------------------2*/
      */*2 Changelog:                                               2*/
      */*2 ---------                                                2*/
      */*2----------------------------------------------------------2*/


      */*3----------------------------------------------------------3*/
      */*3 Special-conditions:                                      3*/
      */*3 ------------------                                       3*/
      */*3                                                          3*/
      */*3 None                                                     3*/
      */*3----------------------------------------------------------3*/

       id division.
       program-id.                     KVTEST1.
       environment division.
       configuration section.
       special-names.                  decimal-point is comma.

       data division.
       working-storage section.

       01 n pic 99.
       01 i pic 99.
       01 fact pic 9(4).

      *----------------------------------------------------------*
       linkage section.
      *----------------------------------------------------------*

       01 In-Data.
           02 mi.
        COPY KVTEST1A.

       01 Out-Data.
           02 fo.
        COPY USSTO1CZ.
        COPY KVTEST1C.
           02 mo.
        COPY KVTEST1B.


      *----------------------------------------------------------*
       procedure division using In-Data Out-Data.
      *----------------------------------------------------------*
       mainflow.

           initialize Out-Data.


           if functionCode IS NUMERIC

           evaluate true
                    when functionCode = 1
                       perform composition
                    when functionCode = 2
                       perform deduction
                    when functionCode = 3
                      perform multiplication
                    when functionCode = 4
                      move number1 to n
                      perform factorial
                   when functionCode = 5
                       move number2 to n
                       perform factorial
                   when other
                       set INVALID-INPUT in ReasonCode in fo to true
                       set STD-ERROR in statusCode in fo to true


           end-evaluate

           else
                set INVALID-INPUT in ReasonCode in fo to true
                set STD-ERROR in statusCode in fo to true
           end-if.

           goback.
     ***************************************************************
       composition.
     ***************************************************************

           COMPUTE Output-String = number1 + number2
              on size error perform setError.

     ***************************************************************
       deduction.
     ***************************************************************

           if number1 > number2 then
              COMPUTE Output-String = number1 - number2
                 on size error perform setError
           else
              COMPUTE Output-String = number2 - number1
                 on size error perform setError
           end-if.

     ***************************************************************
       multiplication.
     ***************************************************************

           COMPUTE Output-String = number1 * number2
                on size error perform setError.

     **************************************************************
       factorial.
     ***************************************************************

           move 0 to i
           move 1 to fact
           perform until i = n
             add 1 to i
             multiply i by fact
               on size error perform setError
      *        stop run
             end-multiply
           end-perform.
           move fact to Output-String.

      ***************************************************************
       setError.
      ***************************************************************
           set STD-ERROR in statusCode in fo to true
           set OUTPUT-OVERFLOW in modReasonCode in fo to true.



       end program KVTEST1.
