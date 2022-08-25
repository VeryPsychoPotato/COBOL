      */*1----------------------------------------------------------1*/
      */*1 Program/module-description:                              1*/
      */*1 --------------------------                               1*/
      */*1                                                          1*/
      */*1 Program-id    : KVTEST2                                  1*/
      */*1 Program-title :   PGM3 working with dates                1*/
      */*1                                                          1*/
      */*1 Task description:                                        */*1
      */*1         Confluence -> Cobol tasks MF5                    1*/
      */*1                                                          1*/
      */*1 Programmer    : BF 9798 Kernius Vildziunas               1*/
      */*1 First version : 01082022                                 1*/
      */*1                                                          1*/
      */*1----------------------------------------------------------1*/
      */*1 Input: functionCode from 1 to 4                          1*/
      */*1                                                          1*/
      */*1 Output: statusCode: from USSTO1CZ                        1*/
      */*1         reasonCode: from USSTO1CZ                        1*/
      */*1         Output-Date: Date or count of days/months        1*/
      */*2----------------------------------------------------------2*/
      */*2 Changelog:                                               2*/
      */*2                                                          2*/
      */*2 Date:     Programmer (E-nr, name):            Change-no: 2*/
      */*2                                                          2*/
      */*2 01082022  BF9798      Kernius Vildziunas        0000000  2*/
      */*2           New program                                    2*/
      */*2----------------------------------------------------------2*/


      */*3----------------------------------------------------------3*/
      */*3 Special-conditions:                                      3*/
      */*3 ------------------                                       3*/
      */*3                                                          3*/
      */*3 None                                                     3*/
      */*3----------------------------------------------------------3*/

       id division.
       program-id.                     KVTEST2.
       environment division.
       configuration section.
       special-names.                  decimal-point is comma.

       data division.
       working-storage section.

       01  Call-Variable.
           05 DATE-CHECK               pic  x(08) value 'USDDC04 '.

       01  WS-FIELDS.
          05  WS-CURRENT-DATE.
             10  WS-CURRENT-YEAR    PIC  9(4).
             10  WS-CURRENT-MONTH   PIC  9(2).
             10  WS-CURRENT-DAY     PIC  9(2).
           05  WS-CURRENT-TIME.
              10  WS-CURRENT-HOUR    PIC  9(2).
              10  WS-CURRENT-MINUTE  PIC  9(2).
              10  WS-CURRENT-SECOND  PIC  9(2).
              10  WS-CURRENT-MS      PIC  9(2).
           05  WS-DIFF-FROM-GMT       PIC S9(4).

       01 WS-start.
          05 Year-Start.
             10 S-Year PIC 9(4).
             10 S-Month PIC 9(2).
             10 S-Day PIC 9(2).

       01 Previous-Month PIC 9(2).
       01 Days-Count PIC 9(3).
       01 Current-date PIC 9(8).
       01 Date-String PIC x(8).

       COPY USDDC04Z.
      *----------------------------------------------------------*
       linkage section.
      *----------------------------------------------------------*

       01 In-Data.
           02 mi.
        COPY KVTEST2A.

       01 Out-Data.
           02 fo.
        COPY USSTO1CZ.
        COPY KVTEST1C.
           02 mo.
        COPY KVTEST2B.


      *----------------------------------------------------------*
       procedure division using In-Data Out-Data.
      *----------------------------------------------------------*
       mainflow.

           initialize Out-Data.

           perform InputValidation.
           perform GetDateYYYYMMDD.

              evaluate true
                   when functionCode = 1
                      perform CurrentDateDay
                   when functionCode = 2
                      perform PreviousDateMonth
                   when functionCode = 3
                      perform CurrentDateFormat
                   when functionCode = 4
                      perform DaysCountFromStart
              end-evaluate.
              goback.






      ***************************************************************
       InputValidation.
      ***************************************************************
           if functionCode >= 1 AND <= 4 AND functionCode IS NUMERIC
                continue
           else
                set INVALID-INPUT in ReasonCode in fo to true
                set STD-ERROR in statusCode in fo to true
           end-if.



      ***************************************************************
       CurrentDateDay.
      ***************************************************************
           move function current-date to ws-fields.
           move ws-current-day to Output-Dates.
           move 'Current day ' to Output-Text.



      ***************************************************************
       PreviousDateMonth.
      ***************************************************************
           move function current-date to ws-fields.
           if ws-current-month = 01
              move 12 to Output-Text
           else
              compute Previous-month = ws-current-month - 01.
           move Previous-Month to Output-Dates.
           move 'Previous month was ' to Output-Text.


      ***************************************************************
       CurrentDateFormat.
      ***************************************************************
           move function current-date to ws-fields.
           move ws-current-date to Output-Dates.
           move 'Current date in YYYYMMDD ' to Output-Text.



      ***************************************************************
       DaysCountFromStart.
      ***************************************************************
      *    move function current-date to ws-current-date-fields.
      *    move ws-current-date to Current-Date.
      *    COMPUTE days-count = function integer-of-date(Current-Date)
      *         - function integer-of-date(20220101).
      *    move days-count to Output-Dates.

           initialize USDDC04-PARM

           string
               year-start in ws-start(7:2) DELIMITED BY SIZE
               year-start in ws-start(5:2) DELIMITED BY SIZE
               year-start in ws-start(1:4) DELIMITED BY SIZE
           into FROM-DATE in USDDC04-PARM

           string
                ws-current-date in WS-FIELDS(7:2) DELIMITED BY SIZE
                ws-current-date in WS-FIELDS(5:2) DELIMITED BY SIZE
                ws-current-date in WS-FIELDS(1:4) DELIMITED BY SIZE
           into TO-DATE in USDDC04-PARM

           move 1 to FUNC-CODE in USDDC04-PARM
           call DATE-CHECK using USDDC04-PARM.
           move NO-OF-DAYS in USDDC04-PARM to Output-Dates.
           move 'Days since January 1' to Output-Text.


      ************************************************************
       GetDateYYYYMMDD.
      ************************************************************
           move function current-date to ws-fields.
           move ws-current-year in ws-current-date
              to s-year in year-start.
           move 01
              to s-month in year-start.
           move 01
              to s-day in year-start.



       end program KVTEST2.
