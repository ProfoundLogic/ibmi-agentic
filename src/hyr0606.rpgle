     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2019')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0606')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Shipment Lot Inquiry                                      *
      *          BY: William Earl (MCA)                                        *
      *        DATE: August 2019                                               *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  03/21/2020  MCA  Expand record sequence from 3,0 to 5,0.              *
      **************************************************************************

     FHYD0606   CF   E             WORKSTN
     F                                     SFILE(SFL01:wk_Sfl01RRN)
     FHYLSGLD1  IF   E           K DISK
     FHYPSGDT   IF   E           K DISK

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0606         PR                  EXTPGM('HYR0606')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  RtnCode                       2A

      /copy hyr0606_pr.rpgle

     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')                   Send program message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

     D QMHRMVPM        PR                  EXTPGM('QMHRMVPM')                   Remove program msgs
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  out_MsgKey                    4A   CONST
     D  out_MsgToRmv                 10A   CONST
     D  io_MsgErrCode                10I 0

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0606         PI
     D  in_PgmMode                    1A   CONST
     D  in_ShipGrp#                   9S 0 CONST
     D  in_Turn#                      9S 0 CONST
     D  in_TurnSeq#                   3S 0 CONST
     D  in_RcdSeq#                    5S 0 CONST
     D  out_RtnCode                   2A

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_ShipGrp#     S              9S 0
     D wk_Turn#        S              9S 0
     D wk_TurnSeq#     S              3S 0
     D wk_RcdSeq#      S              5S 0
     D wk_RtnCode      S              2A
     D wk_Include      S              1N   INZ(*OFF)
     D wk_IN12         S                   LIKE(*IN12)
     D wk_IN25         S                   LIKE(*IN25)
     D wk_Sfl01RRN     S              5P 0
     D wk_Sfl01PC      S              5P 0
     D wk_Sfl01PS      S              3P 0 INZ(12)
     D wk_DspPr        S              1N   INZ(*OFF)
     D wk_OptPr        S              1N   INZ(*OFF)
     D wk_ClrDone      S              1N   INZ(*OFF)
     D wk_Error        S              1N   INZ(*OFF)
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D @ULOT#          S                   LIKE(LDLOT#)                         SFL01 keys
     D @USSCC          S                   LIKE(LDSSCC)                            "   "
     D @DLOT#          S                   LIKE(LDLOT#)                            "   "
     D @DSSCC          S                   LIKE(LDSSCC)                            "   "

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D InqMode         C                   '2'
     D ShutDownMode    C                   '9'
     D @DSP            C                   ' 5'
     D @DSP2           C                   '5 '
     D InqHdg          C                   'Shipment Lot Inquiry'
     D InqOpts1        C                   '5=Display'
     D F3Txt           C                   'F3=Exit'
     D F5Txt           C                   'F5=Refresh'
     D F12Txt          C                   'F12=Cancel'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode
     C                   EVAL      wk_ShipGrp# = in_ShipGrp#
     C                   EVAL      wk_Turn# = in_Turn#
     C                   EVAL      wk_TurnSeq# = in_TurnSeq#
     C                   EVAL      wk_RcdSeq# = in_RcdSeq#
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

     C                   IF        wk_PgmMode = ShutDownMode
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  wk_PgmMode=ShutDownM

     C                   DO

     C                   EXSR      $InzSR
     C                   IF        *IN03
     C                   LEAVE
     C                   ENDIF                                                  *IN03

     C                   EXSR      $PageDownSf1

     C                   DOW       not *IN03

      *
      * Display Subfile
      *
     C                   WRITE     MSFCTL01
      *
      * Check for No Records to Display
      *
     C                   IF        wk_Sfl01RRN = *Zero
     C                   WRITE     SFLNOR01
     C                   ELSE
     C                   EVAL      *IN27 = *ON
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   EVAL      *IN26 = *OFF

     C                   WRITE     SFLCMD01
     C                   EXFMT     SFLCTL01

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EXSR      $RMVPM

      *
      * Valid Command Key?
      *
     C                   IF        *IN25

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Check for function key use with options present.
      *
     C                   EVAL      wk_Error = *OFF

     C                   IF        *IN06  or  *IN17  or  *IN18  or  *IN91  or
     C                                *IN92

     C                   IF        wk_Sfl01RRN <> *Zero

      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf1

      *
      * If function key pressed, and options are present, cannot do
      * function, send error message.
      *
     C                   IF        wk_Error
     C                   IF        *IN06  or  *IN17  or  *IN18
     C                   EVAL      wK_MsgID = 'GUM0005'
     C                   ELSE
     C                   EVAL      wK_MsgID = 'GUM0004'
     C                   ENDIF                                                  *IN06 or
     C                   EVAL      wk_MsgData = *Blanks
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero

     C                   ENDIF                                                  *IN06 or

     C                   SELECT

      *
      * Page Down Key?
      *
     C                   WHEN      *IN91
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN91 = *OFF

      *
      * Page Up Key?
      *
     C                   WHEN      *IN92
     C                   EXSR      $PageUpSf1
     C                   EVAL      *IN92 = *OFF

      *
      * Refresh?
      *
     C                   WHEN      *IN05
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

      *
      * Top?
      *
     C                   WHEN      *IN17
     C                   EXSR      $Top1
     C                   EVAL      *IN17 = *OFF

      *
      * Bottom?
      *
     C                   WHEN      *IN18
     C                   EXSR      $Bottom1
     C                   EVAL      *IN18 = *OFF

     C                   ENDSL

      *
      * Home Key pressed?, set cursor on position field.
      *
     C                   IF        *IN94
     C                   EVAL      *IN42 = *ON
     C                   ENDIF                                                  *IN94

     C                   ELSE

      *
      * Enter Key
      *
      * No Records in Subfile?, If subset inquiry mode, return to calling
      * program.  Otherwise, try to refresh subfile.
      *
     C                   IF        wk_Sfl01RRN = *Zero

     C                   EXSR      $Top1
     C                   IF        wk_Sfl01RRN = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   ELSE

      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf1

      *
      * If errors detected, redisplay screen.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * If no options selected, return to calling program.
      *
     C                   IF        not wk_OptPr
     C                   LEAVE
     C                   ENDIF                                                  not wk_OptPr

      *
      * Read and Process Subfile
      *
     C                   EXSR      $ProcessSf1

     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     C                   ENDDO

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   RETURN

     C/EJECT
      **************************************************************************
      *****                S  U  B  R  O  U  T  I  N  E  S                 *****
      **************************************************************************
      **************************************************************************
      * Send message to program message queue.                                 *
      **************************************************************************
     CSR   $SndPM        BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Remove messages from program message queue.                            *
      **************************************************************************
     CSR   $RmvPM        BEGSR

     C                   CALLP(E)  QMHRMVPM ('*' : *Zero : *Blanks : '*ALL' :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page down subfile SFL01 - main subfile.                               *
      **************************************************************************
     CSR   $PageDownSf1  BEGSR

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C     key_LD1       SETLL     HYLSGLD1
     C                   IF        not %equal(HYLSGLD1)

     C                   EXSR      $ClearSfl01

     C                   IF        *IN91
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  *IN91
     C                   ELSE

      *
      * Set starting point in queue
      *   If Page Down, start at next page
      *   If Refresh, start at top
      *   If Search, start at position to...
      *   Otherwise, initial build or page up, start at BOF
      *
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY        SETGT     HYLSGLD1
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY        SETLL     HYLSGLD1
     C                   OTHER                                                  Enter,...
     C     key_LD1       SETLL     HYLSGLD1
     C                   ENDSL

     C                   ENDIF                                                  not %equal

     C                   DOU       %eof(HYLSGLD1)  or  wk_Include
     C     key_LD1       READE     HYLSGLD1
     C                   IF        not %eof(HYLSGLD1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGLD1)
     C                   ENDDO                                                  %eof(HYLSGLD1) or

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof
     C                   IF        not *IN92
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   IF        not *IN91  and  not *IN05
     C     key_LD1       SETLL     HYLSGLD1
     C                   IF        %equal(HYLSGLD1)
     C                   EXSR      $Bottom1
     C                   ELSE
     C                   EVAL      S@MORE = 'Bottom'
     C                   ENDIF                                                  %equal(HYLSGLD1)
     C                   LEAVESR
     C                   ENDIF                                                  not *IN91 and
     C                   ENDIF                                                  not *IN92

      *
      * If refresh, get last record.
      *
     C                   IF        *IN05
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C     key_LD1       SETGT     HYLSGLD1
     C                   DOU       %eof(HYLSGLD1)  or  wk_Include
     C     key_LD1       READPE    HYLSGLD1
     C                   IF        not %eof(HYLSGLD1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGLD1)
     C                   ENDDO                                                  %eof(HYLSGLD1) or
     C                   IF        %eof
     C                   EVAL      S@MORE = 'Bottom'
     C                   LEAVESR
     C                   ENDIF                                                  %eof
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   ENDIF                                                  %eof

     C                   DOW       not %eof(HYLSGLD1)

      *
      * If subfile is full, quit.
      *
     C                   IF        wk_Sfl01PC = wk_Sfl01PS
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01PC=wk_Sfl01P

      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF

      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSfl01

      *
      * Increment Page Counter, Rel Rec Nbr
      *
     C                   EVAL      wk_Sfl01PC = wk_Sfl01PC + 1
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1

     C                   WRITE     SFL01

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01PC = 1
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @ULOT# = LDLOT#
     C                   EVAL      @USSCC = LDSSCC
     C                   ENDIF                                                  wk_Sfl01PC=1
     C                   EVAL      @DLOT# = LDLOT#
     C                   EVAL      @DSSCC = LDSSCC

      *
      * Get next shipment lot record.
      *
     C                   DOU       %eof(HYLSGLD1)  or  wk_Include
     C     key_LD1       READE     HYLSGLD1
     C                   IF        not %eof(HYLSGLD1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGLD1)
     C                   ENDDO                                                  %eof(HYLSGLD1) or

     C                   ENDDO                                                  not %eof(HYLSGLD1)

      *
      * Set More... or Bottom   subfile messge.
      *
     C                   IF        %eof(HYLSGLD1)
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSGLD1)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear subfile 01, initialize relative record and page count.           *
      **************************************************************************
     CSR   $ClearSfl01   BEGSR

     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN27 = *OFF
     C                   WRITE     SFLCTL01
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   EVAL      wk_Sfl01PC = *Zero

     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page up subfile SFL01 - main subfile.                                 *
      **************************************************************************
     CSR   $PageUpSf1    BEGSR

     C     PGUKEY        SETLL     HYLSGLD1
     C                   DOU       %eof(HYLSGLD1)  or  wk_Include
     C     key_LD1       READPE    HYLSGLD1
     C                   IF        not %eof(HYLSGLD1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGLD1)
     C                   ENDDO                                                  %eof(HYLSGLD1) or

      *
      * If BOF, send Top of the List message
      *
     C                   IF        %eof

     C                   EVAL      wk_MsgID = 'GUM0003'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01PS + 1

     C                   ENDIF                                                  %eof

     C                   DOW       not %eof(HYLSGLD1)
      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSfl01

      *
      * If subfile is full, quit.
      *
     C                   IF        wk_Sfl01PC = wk_Sfl01PS
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01PC=wk_Sfl01P

      *
      * Increment Page Counter, Decrement Rel Rec Nbr
      *
     C                   EVAL      wk_Sfl01PC = wk_Sfl01PC + 1
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN - 1
     C                   WRITE     SFL01

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01RRN = wk_Sfl01PS
     C                   EVAL      @DLOT# = LDLOT#
     C                   EVAL      @DSSCC = LDSSCC
     C                   ENDIF                                                  wk_Sfl01RRN=wk_Sfl01
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @ULOT# = LDLOT#
     C                   EVAL      @USSCC = LDSSCC

      *
      * Get previous shipment lot record.
      *
     C                   DOU       %eof(HYLSGLD1)  or  wk_Include
     C     key_LD1       READPE    HYLSGLD1
     C                   IF        not %eof(HYLSGLD1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGLD1)
     C                   ENDDO                                                  %eof(HYLSDLD1) or

     C                   ENDDO                                                  not %eof

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        %eof(HYLSGLD1)
     C                   EVAL      S@MORE = 'Top'
     C                   IF        wk_Sfl01PC <> wk_Sfl01PS
     C                   EVAL      @ULOT# = *Loval
     C                   EVAL      @USSCC = *Loval
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  wk_Sfl01PC<>wk_Sfl01
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSGLD1)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSfl01  BEGSR

     C                   EVAL      S1LOT# = LDLOT#
     C                   EVAL      S1QSTC = LDQSTC
     C                   EVAL      S1SSCC = LDSSCC

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01 main subfile.                                             *
      **************************************************************************
     CSR   $Filter1      BEGSR

     C                   EVAL      wk_Include = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Edit options subfile SFL01 - main subfile.                            *
      **************************************************************************
     CSR   $EditSf1      BEGSR

      *
      *  Reset indicators.
      *
     C                   EVAL      wk_Error = *OFF
     C                   EVAL      wk_DspPr = *OFF
     C                   EVAL      wk_OptPr = *OFF

      *
      * Save function key indicators.
      *
     C                   EVAL      wk_IN25 = *IN25

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01

     C                   DOW       not %eof(HYD0606)

      *
      * Reset error indicators and turn on SFLNXTCHG indicator.
      *
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN28 = *ON

      *
      * If a valid function key, turn on error indicator.
      *
     C                   IF        wk_IN25
     C                   EVAL      *IN41 = *ON
     C                   ENDIF                                                  wk_IN25

      *
      * Displays present?
      *
     C                   IF        S1OPT = @DSP  or  S1OPT = @DSP2

     C                   EVAL      S1OPT = @DSP
     C                   EVAL      wk_DspPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

     C                   IF        S1OPT <> *Blanks

      *
      *  Set Cursor on First Error
      *
     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   UPDATE    SFL01
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EXSR      $SndPM

     C                   ELSE

      *
      * Ignore Blank Options, Turn off Error & SFLNXTCHG
      *
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   UPDATE    SFL01

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@DSP or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01

     C                   ENDDO                                                  not %eof(HYD0606)

      *
      * Turn off error indicator.
      *
     C                   EVAL      *IN41 = *OFF

      *
      * Restore function key indicators.
      *
     C                   EVAL      *IN25 = wk_IN25

      *
      * If options present, and Page Down or Page Up requested, set
      * error flag.
      *
     C                   IF        *IN25
     C                   IF        wk_DspPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_DspPr
     C                   ELSE
     C                   IF        wk_DspPr
     C                   EVAL      wk_OptPr = *ON
     C                   ENDIF                                                  wk_DspPr
     C                   ENDIF                                                  *IN25

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process options for subfile SFL01.                                     *
      **************************************************************************
     CSR   $ProcessSf1   BEGSR

     C                   EVAL      *IN12 = *OFF

     C                   READC     SFL01

     C                   DOW       not %eof(HYD0606)  and  not *IN03

      *
      *  F12=Cancel Key
      *
     C                   IF        not *IN12

     C                   SELECT

     C                   WHEN      S1OPT = @DSP
     C                   EXSR      $Display

     C                   ENDSL

     C                   IF        not *IN03
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                   *IN12
     C                   UPDATE    SFL01
     C                   ENDIF                                                   not *IN03

     C                   EVAL      S@RCNO = wk_Sfl01RRN

     C                   ELSE

      *
      * Cancel requested - save remaining options.
      *
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL01

     C                   ENDIF                                                  not *IN12

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0606)  and  not *IN03
     C                   EVAL      wk_IN12 = *IN12
     C                   READC     SFL01
     C                   EVAL      *IN12 = wk_IN12
     C                   ENDIF                                                  not %eof(HYD0606)

     C                   ENDDO                                                  not %eof(HYD0606)

     C                   EVAL      *IN12 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL01 from the top.                                    *
      **************************************************************************
     CSR   $Top1         BEGSR

     C                   EVAL      @ULOT# = *Loval
     C                   EVAL      @USSCC = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   IF        *IN17
     C                   EVAL      S@MORE = 'Top'
     C                   ENDIF                                                  *IN17

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Display subfile SFL01 from the bottom.                                 *
      **************************************************************************
     CSR   $Bottom1      BEGSR

     C                   EVAL      @ULOT# = *Hival
     C                   EVAL      @USSCC = *Hival
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageUpSf1
     C                   EVAL      S@MORE = 'Bottom'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display shipment lot record.                                           *
      **************************************************************************
     CSR   $Display      BEGSR

     C                   EXSR      $RtvSGLD
     C                   IF        wk_Error
     C                   LEAVESR
     C                   ENDIF                                                  wk_Error

      *
      * Populate screen fields.
      *
     C                   EXSR      $MoveToScrn3

      *
      * Initialize screen indicators.
      *
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN93 = *OFF

     C                   DOU       *IN03  or  *IN12

      *
      * Format Display screen.
      *
     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT3A
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RMVPM

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Enter key pressed?
      *
     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move data to "display" screen fields.                                  *
      **************************************************************************
     CSR   $MoveToScrn3  BEGSR

     C                   EVAL      F3GRP# = %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      F3SHTO = %triml(%editc(GDSHTO:'Z')) +
     C                               '-' + GDNAME
     C                   EVAL      F3TURN = %triml(%editc(wk_Turn#:'Z'))
     C                   EVAL      F3TSEQ = %editc(wk_TurnSeq#:'X') + '-' +
     C                               %editc(wk_RcdSeq#:'X')
     C                   EVAL      F3ORD# = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      F3ITEM = GDITEM
     C                   EVAL      F3IMDS = GDIMDS
     C                   EVAL      F3QSTC = %triml(%editc(%dec(GDQSTC:9:0):'J'))
     C                   EVAL      F3PLOC = GDPLOC
     C                   SELECT
     C                   WHEN      GDDVRT > *Zero  and  GDDVRT < 100
     C                   EVAL      F3LANE = %triml(%editc(GDDVRT:'Z'))
     C                   WHEN      GDDVRT = *Zero
     C                   EVAL      F3LANE = '-'
     C                   OTHER
     C                   EVAL      F3LANE = '**'
     C                   ENDSL
     C                   EVAL      F3LOT# = LDLOT#
     C                   EVAL      F3LOTQ = %triml(%editc(%dec(LDQSTC:9:0):'J'))
     C                   EVAL      F3SSCC = %triml(%editc(LDSSCC:'Z'))

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve shipment lot record.                                          *
      **************************************************************************
     CSR   $RtvSGLD      BEGSR

     C     key_LD2       CHAIN     HYLSGLD1
     C                   IF        not %found(HYLSGLD1)
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYLSGD1'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %editc(wk_ShipGrp#:'X') + '/' +
     C                               %editc(wk_Turn#:'X') +'/' +
     C                               %editc(wk_TurnSeq#:'X') + '/' +
     C                               %editc(wk_RcdSeq#:'X') + '/' +
     C                               %trimr(S1LOT#) + '/' +
     C                               %editc(S1SSCC:'X')
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Display request ignored.'
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HYLSGLD1)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR


     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - each call.                                    *
      **************************************************************************
     CSR   $InzSR        BEGSR

     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN12 = *OFF

     C                   SELECT
     C                   WHEN      wk_PgmMode = InqMode
     C                   EVAL      C1HDG1 = InqHdg
     C                   EVAL      F3HDG1 = InqHdg
     C                   EVAL      C1OPTS1 = InqOpts1
     C                   EVAL      F1FKEYS = F3Txt + '   ' + F5Txt + '   ' +
     C                               F12Txt
     C                   OTHER
     C                   EVAL      *IN03 = *ON
     C                   ENDSL
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   EVAL      F3HDG1 = center(F3HDG1)

     C                   EVAL      *IN93 = *OFF

     C     key_GD        SETLL     HYPSGDT
     C                   DOU       %eof(HYPSGDT)  or  GDRSEQ = wk_RcdSeq#
     C     key_GD        READE     HYPSGDT
     C                   ENDDO                                                  %eof(HYPSGDT) or
     C                   IF        %eof(HYPSGDT)
     C                   EVAL      GDSHTO = *Zero
     C                   EVAL      GDNAME = *Blanks
     C                   EVAL      GDORD# = *Zero
     C                   EVAL      GDITEM = *Blanks
     C                   EVAL      GDIMDS = *Blanks
     C                   EVAL      GDQSTC = *Zero
     C                   EVAL      GDPLOC = *Blanks
     C                   ENDIF                                                  %eof(HYPSGDT)

     C                   EVAL      C1GRP# = %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      C1SHTO = '#' + %triml(%editc(GDSHTO:'Z')) +
     C                               '-' + GDNAME
     C                   EVAL      C1ORD# = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      C1ITEM = '#' + %trimr(GDITEM) + '-' + GDIMDS
     C                   EVAL      C1QSTC = %triml(%editc(%dec(GDQSTC:9:0):'J'))
     C                   EVAL      C1PLOC = GDPLOC
     C                   EVAL      C1RSEQ = %editc(wk_RcdSeq#:'X')

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define  Key Lists
      *
     C     key_GD        KLIST                                                  HYPSGDT
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_TurnSeq#

     C     key_LD1       KLIST                                                  HYLSGLD1
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_TurnSeq#
     C                   KFLD                    wk_RcdSeq#

     C     key_LD2       KLIST                                                  HYLSGLD1
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_TurnSeq#
     C                   KFLD                    wk_RcdSeq#
     C                   KFLD                    S1LOT#
     C                   KFLD                    S1SSCC

     C     RFSKEY        KLIST                                                  SFL01 - HYLSGLD1
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_TurnSeq#
     C                   KFLD                    wk_RcdSeq#
     C                   KFLD                    @ULOT#
     C                   KFLD                    @USSCC

     C     PGUKEY        KLIST                                                  SFL01 - HYLSGLD1
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_TurnSeq#
     C                   KFLD                    wk_RcdSeq#
     C                   KFLD                    @ULOT#
     C                   KFLD                    @USSCC

     C     PGDKEY        KLIST                                                  SFL01 - HYLSGLD1
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_TurnSeq#
     C                   KFLD                    wk_RcdSeq#
     C                   KFLD                    @DLOT#
     C                   KFLD                    @DSSCC

      *
      * Initialize Work Fields
      *
     C                   EVAL      S@PGMQ = '*'
     C                   EVAL      S@RCNO = *Zero
     C                   EVAL      P@PGNM = ps_Pgnm
     C                   EVAL      P@JOBN = ps_Job

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID
     C                   IF        wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'SS'
     C                   EVAL      C1ENV = *Blanks
     C                   EVAL      F1ENV = *Blanks
     C                   ELSE
     C                   EVAL      C1ENV = '*TEST*'
     C                   EVAL      F1ENV = '*TEST*'
     C                   ENDIF                                                  wk_DatabaseID='HY'

     CSR                 ENDSR

     C/EJECT
