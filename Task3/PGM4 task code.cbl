      */*1----------------------------------------------------------1*/
      */*1 Program/module-description:                              1*/
      */*1 -------------------------------------------------------- 1*/
      */*1                                                          1*/
      */*1 Program-id    : KVTEST3                                  1*/
      */*1 Program-title :   PGM4 working with dates                1*/
      */*1                                                          1*/
      */*1 Task description:                                        1*/
      */*1         Confluence -> Cobol tasks MF5                    1*/
      */*1                                                          1*/
      */*1 Programmer    : BF 9798 Kernius Vildziunas               1*/
      */*1 First version : 05082022                                 1*/
      */*1                                                          1*/
      */*1----------------------------------------------------------1*/
      */*1 Input: PGMID                                             1*/
      */*1        PGM_KEY                                           1*/
      */*1                                                          1*/
      */*1 Output: price for of each ISIN                           1*/
      */*1            ISIN with max and min prices.                 1*/
      */*1 Modules called                                           1*/
      */*1   USEVENT0 - Log traces and errors                       1*/
      */*1   USEVBLK  - Build blocks to USEVENT0                    1*/
      */*1   USTRACE0 - Check if traces is active                   1*/
      */*1                                                          1*/
      */*1 Description                                              1*/
      */*1   The module performs the following functions:           1*/
      */*1     1. Checks if trace is active.                        1*/
      */*1     2. Log trace information.                            1*/
      */*1     3. Depending on the command the restart table are    1*/
      */*1        checked for restart, updated or deleted.          1*/
      */*1     4. statusCode/reasonCode are raised according below. 1*/
      */*1                                                          1*/
      */*1 StatusCode/reasonCode                                    1*/
      */*1   statusCode      reasonCode                             1*/
      */*1----------------------------------------------------------1*/
      */*1   STD-OK          0                                      1*/
      */*1   STD-WARNING     PROGRAM-RESTARTED                      1*/
      */*1                   NO-RESTART-DATA                        1*/
      */*1   STD-ERROR       ERROR-COMMAND                          1*/
      */*1                   ERROR-RESTARTDATA-LENGTH               1*/
      */*1                   DATABASE-ERROR                         1*/
      */*1                   DEADLOCK                               1*/
      */*1                   UNAVAILABLE-RESOURCE                   1*/
      */*1   STD-SEVERE      -                                      1*/
      */*2----------------------------------------------------------2*/
      */*2 Changelog:                                               2*/
      */*2                                                          2*/
      */*2 Date:     Programmer (E-nr, name):            Change-no: 2*/
      */*2                                                          2*/
      */*2 01082022  BF9798      Kernius Vildziunas        0000000  2*/
      */*2           New program                                    2*/
      */*2----------------------------------------------------------2*/
      */*---------------------------------------------------------- 3*/
      */*3 Special-conditions:                                      3*/
      */*3----------------------------------------------------------3*/
      */*3                                                          3*/
      */*3 None                                                     3*/
      */*3----------------------------------------------------------3*/

       id division.
       program-id.                     KVTEST3.
       environment division.
       configuration section.
       special-names.                  decimal-point is comma.

       data division.
       working-storage section.

       COPY LBSLAT0Z.

        01  scannerCallTilVariabel.
           05 CHECK-TRACE              pic  x(08) value 'USTRACE0'.
           05 LOG-EVENT                pic  x(08) value 'USEVENT0'.
           05 READ-PARAM               pic  x(08) value 'FIM044P'.

        01  WS-STATIC.

           05 SPI.
              10 SPICODE               pic  x(02) value 'UT'.
              10 SUBSPICODE            pic  x(02) value 'MO'.

        01  ws.
           05 pgmTraceCode             pic  x(01).
              88 LOG-NO-TRACE                     value '0'.
              88 LOG-PROGRAM                      value '1' thru '9'.
              88 LOG-CALL                         value '2' thru '9'.
              88 LOG-DATA-ACCESS                  value '3' thru '9'.
              88 LOG-PARAGRAPH                    value '4' thru '9'.

           05 logCode                  pic  x(128).

           05 signChar9                pic  +(08)9.

           05                          binary.
              10 leadingSpaces         pic s9(04).

       01 isinTable.
           05 isin pic x(15)
              occurs 4 times
             indexed by isin-index.

       01 isinValue                    pic x(15).


       01 dclDB2Array.
          05 isin-price occurs 4 times indexed by isin-price-index.
             10  FOKD              PIC X(12).
             10  KRS-HDL       PIC S9(7)V9(8).

          exec sql
                 declare isinCursor cursor for
                 select FOKD
                      ,KRS_HDL
                 from TVD_KURS_STAM_REAL A
                 where FOKD =:isinValue

                for fetch only
           end-exec

      *****************************************************************
      * Interface to FIM044P
      *****************************************************************
         01  Fim044-areal.
       COPY FIM044CZ.


      ***************************************************************
      * Interface to USEVENT0 and USEVBLK
      ****************************************************************

       COPY USEVNTCZ.
       COPY USEVN2CZ.

       COPY USEVNBCZ.

       01  eventBlockType              pic  x(30).
           88 EVENT-BLOCK-INPUT                   value 'INPUT'.
           88 EVENT-BLOCK-OUTPUT                  value 'OUTPUT'.
      *    88 EVENT-BLOCK-OYTIME                  value 'OYTIM0CZ'.
      *    88 EVENT-BLOCK-SQLCA                   value 'SQLCA'.
      *    88 EVENT-BLOCK-TABLE-RESTART           value 'TABLE-RESTART'.
           88 EVENT-BLOCK-FIM044P                 value 'USDDC04'.
           88 EVENT-BLOCK-SELECT-ISIN             value 'VDKSSRCZ'.
      *****************************************************************
      * DB2 variables and data definition
      *****************************************************************

           exec sql include sqlca
           end-exec.

       01  sqlAid                      redefines sqlca.
       COPY USSQLACZ.
       COPY FIPGPMCZ.
      *COPY UTRST1CZ.
       COPY VDKSSRCZ.

      *****************************************************************
       linkage section.
      *****************************************************************


       01 In-Data.
           02 fi.
       COPY USSTI0CZ.
           02 mi.
       COPY KVTEST3A.


       01 Out-Data.
           02 fo.
       COPY USSTO1CZ.
           02 mo.
       COPY KVTEST3B.
      *COPY UTSTR0CZ.


      *****************************************************************
       procedure division using In-Data Out-Data.
      *****************************************************************
       mainflow.

           perform initializeProgram.
           perform fetchParametersFIM044.
           perform unstringIsinList.
           perform getData.
           perform exitProgram.


      *****************************************************************
       fetchParametersFIM044.
      *****************************************************************
           initialize FIM044-AREAL
           move '01' to version in FIM044I
           move 'V' to funktion in FIM044I
           move 'ISIN-LIST' to pgm-key in FIM044I
           move 'TESTPGM' to PGMID in FIM044I
      *    move PGM_KEY in mi to pgm-key in FIM044I
      *    move PGMID in mi to pgmid in FIM044I

           if LOG-CALL in pgmTraceCode in ws
              move 'Invokecallfim044 - 001' to logLocation
              string 'Before calling FIM044P'
                 delimited by size
                 into logDescription
              end-string
              set LOG-CALL in traceLogSeverity to true
              set EVENT-BLOCK-FIM044P in eventBlockType to true
              perform appendEventBlocks
              perform logEvent
           end-if

           call 'FIM044X' using Fim044-areal
              on exception
                 move 'Invokecallfim044 - 002' to logLocation
                 string 'Program FIM044X not found'
                    delimited by size
                    into logDescription
                 end-string
                 go to errorexit
           end-call

           if LOG-CALL in pgmTraceCode in ws
              move 'Invokecallfim044 - 003' to logLocation
              string 'After calling FIM044X'
                 delimited by size
                 into logDescription
              end-string
              set LOG-CALL in traceLogSeverity to true
              set EVENT-BLOCK-FIM044P in eventBlockType to true
              perform appendEventBlocks
              perform logEvent
           end-if.


      *****************************************************************
       initializeProgram.
      *****************************************************************

           initialize ws
                    Out-Data
                    useventCommData

           if hierarchyTraceCode in fi > zeroes
              move pgmNavn in S-L-A-T to logApplication
              call CHECK-TRACE using useventComm
                                     pgmTraceCode in ws
                 on exception
                    continue
              end-call
           end-if

           initialize useventCommData

           call 'USEVBLK' Using evbTable

           if LOG-PROGRAM in pgmTraceCode in ws
              move 'initializeProgram - 001' to logLocation
              move 'Program started' to logDescription
              set LOG-PROGRAM in traceLogSeverity to true
              set EVENT-BLOCK-INPUT in eventBlockType to true
              perform appendEventBlocks
              perform logEvent
           end-if.
      *
      *    initialize dclTable-restart
      *
      *    move runId in fi to runId in dclTable-restart
      *    move programId in mi to programId in dclTable-restart
      *    move runDate in mi to runDate in dclTable-restart
      *    .
      *****************************************************************
      *validateProgramInput.
      *****************************************************************


      *       set ERROR-COMMAND in modReasonCode in mo to true
      *       move 'validateProgramInput - 001' to logLocation
      *       string 'Invalid command. '
      *              'Command = "'
      *               command in fi '"'
      *          delimited by size
      *          into logDescription
      *       end-string
      *       perform errorExit
      *    end-if
      *
      *    if restartDataLen in fi > length of restartData in fi
      *       set ERROR-RESTARTDATA-LENGTH in modReasonCode in mo
      *        to true
      *       move 'validateProgramInput - 002' to logLocation
      *       move length of restartData in fi to signChar9 in ws
      *       string 'Length error. '
      *              'restartDataLen in fi > '
      *               signChar9 in ws
      *          delimited by size
      *          into logDescription
      *       end-stringf
      *       perform errorExit
      *    end-if
      *    .
      *****************************************************************
       unstringIsinList.
      *****************************************************************

           unstring pgm-param in fim044o delimited by '05'
              into isin(1), isin(2), isin(3), isin(4)
           end-unstring.
      *****************************************************************
       getData.
      *****************************************************************

           initialize DCLTVD-KURS-STAM-REAL
           set isin-index to 1
           set isin-prices-index to 1
           move -1 to KRS-HDL in max-isin-prices
           move 99999 to KRS-HDL in min-isin-prices

      *****************************************************************
      * Retrieves FOKD and KURS_HDL value from TVD_KURS_STAM_REAL
      * table
      *****************************************************************
           perform until isin-index > 4
              move isin(isin-index) to isinValue
              initialize DCLTVD-KURS-STAM-REAL
              perform openCursor
              perform fetchCursor
              move FOKD in DCLTVD-KURS-STAM-REAL
                 to FOKD in isin-prices(isin-prices-index)
              move KRS-HDL in DCLTVD-KURS-STAM-REAL
                 to KRS-HDL in isin-prices(isin-prices-index)

      *****************************************************************
     ** Min/Max price value calculation
      *****************************************************************

              if KRS-HDL in DCLTVD-KURS-STAM-REAL >
                 KRS-HDL in max-isin-prices
              move KRS-HDL in DCLTVD-KURS-STAM-REAL
                 to KRS-HDL in max-isin-prices
              move FOKD in DCLTVD-KURS-STAM-REAL
                 to FOKD in max-isin-prices
              end-if


             if KRS-HDL in DCLTVD-KURS-STAM-REAL <
                KRS-HDL in min-isin-prices
              move KRS-HDL in DCLTVD-KURS-STAM-REAL
                 to KRS-HDL in min-isin-prices
             move FOKD in DCLTVD-KURS-STAM-REAL
                 to FOKD in min-isin-prices
              end-if

              set isin-index up by 1
              set isin-prices-index up by 1

              perform closeCursor

           end-perform.

      *****************************************************************
       fetchCursor.
      *****************************************************************
           exec sql
                fetch isinCursor
                into :DCLTVD-KURS-STAM-REAL.FOKD,
                     :DCLTVD-KURS-STAM-REAL.KRS-HDL
           end-exec.

      *****************************************************************
       openCursor.
      *****************************************************************
           exec sql
              open isinCursor
           end-exec.

      *****************************************************************
       closeCursor.
      *****************************************************************
           exec sql
              close isinCursor
           end-exec.

      *****************************************************************
       clearOriginalFields.
      *****************************************************************

           if originalProgramName in fo not = spaces
              move spaces to originalProgramName in fo
              move spaces to originalStatusCode in fo
              move zeroes to originalReasonCode in fo
              move spaces to originalErrorType in fo
              move spaces to originalErrorCode in fo
           end-if
           .

      *****************************************************************
       errorExit.
      *****************************************************************

           set STD-ERROR in statusCode in fo to true

           if logCode in useventComm = spaces
              move reasonCode in fo to logCode in useventComm
           end-if

           set EVENT-BLOCK-INPUT in eventBlockType to true
           perform appendEventBlocks

           perform logEvent

           perform exitProgram
           .

      *****************************************************************
       appendEventBlocks.
      *****************************************************************

           evaluate true
              when EVENT-BLOCK-INPUT in eventBlockType
                 call 'USEVBLK' using evbTable
                       by content    'LBSLATCZ '
                       by content     length of S-L-A-T
                       by reference   S-L-A-T
                       by content    'USSTI0CZ '
                       by content     length of fi
                       by reference   fi
                       by content    'UTSTI1CZ '
      *                by content     length of ai
      *                by reference   ai
                       by content    'KVTEST3A '
                       by content     length of mi
                       by reference   mi

              when EVENT-BLOCK-OUTPUT in eventBlockType
                 call 'USEVBLK' using evbTable
                       by content    'USSTO1CZ '
                       by content     length of fo
                       by reference   fo
                       by content    'UTSTO0CZ '
      *                by content     length of ao
      *                by reference   ao
                       by content    'KVTEST3B '
                       by content     length of fo
                       by reference   fo

               when EVENT-BLOCK-FIM044P in eventBlockType
                  call 'USEVBLK' using evbTable
                        by content     'FIM044CZ '
                        by content     length of FIM044-AREAL
                        by reference   FIM044-AREAL

               when EVENT-BLOCK-SELECT-ISIN  in eventBlockType
                 call 'USEVBLK' using evbTable
                       by content    'VDKSSRCZ'
      *                by content     length of DCLTVD-KURS-STAM-REAL
      *                by reference   DCLTVD-KURS-STAM-REAL
     **
      *       when EVENT-BLOCK-SQLCA in eventBlockType
      *          call 'USEVBLK' using evbTable
      *                by content    'SQLCA '
      *                by content     length of SQLCA
      *                by reference   SQLCA
      *
      *       when EVENT-BLOCK-TABLE-RESTART in eventBlockType
      *          call 'USEVBLK' using evbTable
      *                by content    'UTRST1PZ '
      *                by content     length of dclTable-restart
      *                by reference   dclTable-restart
      *                by content    'UTRST1IZ '
      *                by content     length of indTable-restart
      *                by reference   indTable-restart
           end-evaluate
           .
      *****************************************************************
       exitProgram.
      *****************************************************************

      *    move 'EXAMPLE' to charField in ao

           if STD-UNINITIALIZED in statusCode in fo
              set STD-OK in statusCode in fo to true
              perform clearOriginalFields
           else
              if originalProgramName in fo = spaces
                 move pgmNavn in S-L-A-T to originalProgramName in fo
                 move statusCode in fo to originalStatusCode in fo
                 move reasonCode in fo to originalReasonCode in fo
              end-if
           end-if

           if LOG-PROGRAM in pgmTraceCode in ws
              if STD-ERROR in statusCode in fo
              or STD-SEVERE in statusCode in fo
                 move 'exitProgram - 001' to logLocation
                 move 'Program ended in error' to logDescription
              else
                 move 'exitProgram - 002' to logLocation
                 move 'Program ended normally' to logDescription
              end-if
              set LOG-PROGRAM in traceLogSeverity to true
              set EVENT-BLOCK-OUTPUT in eventBlockType to true
              perform appendEventBlocks
              perform logEvent
           end-if

           goback
           .
      *****************************************************************
      *clearOriginalFields.
      *****************************************************************

      *    if originalProgramName in fo not = spaces
      *       move spaces to originalProgramName in fo
      *       move spaces to originalStatusCode in fo
      *       move zeroes to originalReasonCode in fo
      *       move spaces to originalErrorType in fo
      *       move spaces to originalErrorCode in fo
      *    end-if
      *    .
      *****************************************************************
      *DB2errorExit.
      *****************************************************************
      *
      *    evaluate true
      *       when SQL-UNAVAIL-RESOURCE in sqlAid
      *          set UNAVAILABLE-RESOURCE in reasonCode in fo to true
      *       when SQL-DEADLOCK in sqlAid
      *          set DEADLOCK in reasonCode in fo to true
      *       when SQL-WARNING in sqlAid
      *          set DATABASE-ERROR in reasonCode in fo to true
      *       when other
      *          set DATABASE-ERROR in reasonCode in fo to true
      *    end-evaluate
      *
      *    if originalProgramName in fo = spaces
      *       set ERROR-TYPE-DB2 in originalErrorType in fo to true
      *       move sqlcode in sqlca to originalSqlcode in fo
      *    end-if
      *
      *    set LOG-SQL in errorLogType to true
      *
      *    move sqlcode in sqlca to signChar9 in ws
      *    move signChar9 in ws to logCode in useventComm
      *
      *    set EVENT-BLOCK-SQLCA in eventBlockType to true
      *    perform appendEventBlocks
      *
      *    perform errorExit
      *    .
   .
      *****************************************************************
       logEvent.
      *****************************************************************

           if errorLogType = spaces
              set LOG-APPLICATION in errorLogType to true
           end-if

           evaluate true
              when traceLogSeverity numeric
                 set LOG-TRACE in traceLogType To true
                 move pgmTraceCode in ws to logCode in useventComm
              when LOG-TRACE in traceLogType
                 move pgmTraceCode in ws to logCode in useventComm
              when STD-WARNING in statusCode in fo
              or   STD-ERROR in statusCode in fo
              or   STD-SEVERE in statusCode in fo
                 set LOG-WARNING in errorLogSeverity to true
              when other
                 set LOG-INFORMATION in errorLogSeverity to true
           end-evaluate

           move SPI in WS-STATIC to logSpi
           move PGMNAVN in S-L-A-T to logApplication

           if logCode in useventComm = spaces
           or logCode in useventComm(1:1) not = spaces
              continue
           else
              inspect logCode in useventComm
                 tallying leadingSpaces in ws
                 for leading spaces
              move logCode in useventComm(leadingSpaces + 1:)
                to logCode in ws
              move logCode in ws
                to logCode in useventComm
           end-if

           perform
              varying evbIndex
              from    0 by 9
              until   evbIndex > 0
              and     evbIndex >= evbCount

              call LOG-EVENT using useventComm
                    by value       evbNPtr(evbIndex + 1)
                    by reference   evbLen(evbIndex  + 1)
                    by value       evbPtr(evbIndex  + 1)
                    by value       evbNPtr(evbIndex + 2)
                    by reference   evbLen(evbIndex  + 2)
                    by value       evbPtr(evbIndex  + 2)
                    by value       evbNPtr(evbIndex + 3)
                    by reference   evbLen(evbIndex  + 3)
                    by value       evbPtr(evbIndex  + 3)
                    by value       evbNPtr(evbIndex + 4)
                    by reference   evbLen(evbIndex  + 4)
                    by value       evbPtr(evbIndex  + 4)
                    by value       evbNPtr(evbIndex + 5)
                    by reference   evbLen(evbIndex  + 5)
                    by value       evbPtr(evbIndex  + 5)
                    by value       evbNPtr(evbIndex + 6)
                    by reference   evbLen(evbIndex  + 6)
                    by value       evbPtr(evbIndex  + 6)
                    by value       evbNPtr(evbIndex + 7)
                    by reference   evbLen(evbIndex  + 7)
                    by value       evbPtr(evbIndex  + 7)
                    by value       evbNPtr(evbIndex + 8)
                    by reference   evbLen(evbIndex  + 8)
                    by value       evbPtr(evbIndex  + 8)
                    by value       evbNPtr(evbIndex + 9)
                    by reference   evbLen(evbIndex  + 9)
                    by value       evbPtr(evbIndex  + 9)
                 on exception
                    continue
              end-call

              if LOG-SEVERE in errorLogSeverity
                 set LOG-ERROR in errorLogSeverity to true
              end-if

              set LOG-CONTINUED in errorLogType to true
           end-Perform

           initialize useventCommData
           call 'USEVBLK' Using evbTable
           .




       end program KVTEST3.
