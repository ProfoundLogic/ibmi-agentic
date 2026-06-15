     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2024')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0626')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Shipment Detail Mixed/Partial Label Print                 *
      *          BY: William Earl (MCA)                                        *
      *        DATE: February 2024                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      **************************************************************************

     FHYD0626A  CF   E             WORKSTN
     F                                     SFILE(SFL01A:wk_Sfl01RRN)
     FHYLSGDT3  IF   E           K DISK
     FHYPSGLD   UF   E           K DISK    USROPN
     FHYPSCWK   O    E           K DISK    BLOCK(*NO) PREFIX(SC:2) USROPN
     FHYW0602   IF   E           K DISK    USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0626         PR                  EXTPGM('HYR0626')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  LblType                       1A   CONST                                P=Partial, M=Mixed
     D  RtnCode                       2A

      /COPY QPRPSRC,HYR0622

      /COPY QPRPSRC,HYR9930

      /COPY QPRPSRC,CHKDIGIT

     D SelOK           PR             2A                                        Check if select ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  SSCCBC                       20A   CONST
     D  Order#                        8S 0 CONST
     D  Item#                        15A   CONST
     D  LblType                       1A   CONST

     D LotsNeeded      PR             1N                                        Check if lots needed
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST
     D  SSCC                          9S 0 CONST

     D LblsNeeded      PR             1N                                        Check if lbls needed
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  OrdCtl#                       8S 0 CONST
     D  Line#                         3S 0 CONST
     D  Blanket#                      3S 0 CONST
     D  Lbl1                          1A   CONST
     D  Lbl2                          1A   CONST
     D  Lbl3                          1A   CONST

     D TVNeeded        PR             1N                                        Check Tote Vfy neede
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST

     D PltzNeeded      PR             1N
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Order#                        8S 0 CONST
     D  Line#                         3S 0 CONST
     D  Blanket#                      3S 0 CONST
     D  SSCC                          9S 0 CONST

     D ShipOrPick      PR             4A
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  Order#                        8S 0 CONST
     D  Line#                         3S 0 CONST

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
     D HYR0626         PI
     D  in_PgmMode                    1A   CONST
     D  in_ShipGrp#                   9S 0 CONST
     D  in_BillTo#                    7S 0 CONST
     D  in_ShipTo#                    7S 0 CONST
     D  in_DropShip#                  7S 0 CONST
     D  in_LblType                    1A   CONST                                P=Partial, M=Mixed
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
     D wk_SSCCSrlRef   S              9P 0 DTAARA(SSCCSRLREF)                   SSCC serial no.
     D wk_LDA          S           1024A   DTAARA(*LDA)                         *LDA

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_ShipGrp#     S              9S 0
     D wk_BillTo#      S              7S 0
     D wk_ShipTo#      S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_LblType      S              1A
     D wk_RtnCode      S              2A
     D wk_EMPL         S              5S 0
     D wk_ToteVfy      S              1N   INZ(*OFF)
     D wk_GCSTNM       S             30A
     D wk_GCSTYP       S              1A
     D wk_MoreAvail    S              1N   INZ(*OFF)
     D wk_Include      S              1N   INZ(*OFF)
     D wk_IN25         S                   LIKE(*IN25)
     D wk_Sfl01RRN     S              5P 0
     D wk_SelPr        S              1N   INZ(*OFF)
     D wk_OptPr        S              1N   INZ(*OFF)
     D wk_ClrDone      S              1N   INZ(*OFF)
     D wk_SelDft       S              1N   INZ(*OFF)
     D wk_DftOrd#      S                   LIKE(GDORD#)
     D wk_DftItem      S                   LIKE(GDITEM)
     D wk_SelChk       S              2A
     D wk_LblQty       S              7P 0
     D wk_LblUOM       S              2A
     D wk_SSCC         S              9P 0
     D wk_SSCCBC       S             20A
     D wk_ChkDgt       S              1A
     D wk_GDTURN       S              9S 0
     D wk_GDTSEQ       S              3S 0
     D wk_GDITEM       S             15A
     D wk_GDIMDS       S             30A
     D wk_GDPLOC       S             17A
     D wk_GDQSTC       S             13P 4
     D wk_ODWH         S              3S 0
     D wk_IWFTCL       S              5A
     D wk_IMPCLS       S              4A
     D wk_IMLOT        S              1A
     D wk_IMUOMS       S              2A
     D wk_IMUOMW       S              2A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN1       S             13P 5
     D wk_IMUDN4       S             13P 5
     D wk_TDKEY1       S             15A
     D wk_BDITEM       S             15A
     D wk_BDUPC12      S             12A
     D wk_LotCnt       S              5P 0
     D wk_Carton#      S              5P 0
     D wk_Bulk         S              1N
     D wk_UnitWgt      S              9P 5
     D wk_SelItem#     S             15A
     D wk_SelOrder#    S              8S 0
     D wk_PrtLabel     S              1N   INZ(*OFF)
     D wk_TotLDQSTC    S             13P 4
     D wk_EMFNAM       S             18A
     D wk_EMLNAM       S             18A
     D wk_RcdCnt       S              7P 0
     D wk_RC0622       S             10I 0
     D wk_Error        S              1N   INZ(*OFF)
     D wk_Pgm01        S              1N   INZ(*OFF)                            HYR0622 called
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D wk_GDDVRT       S                   like(GDDVRT)
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D sv_QOPK         S                   LIKE(GDQOPK)
     D @UTURN          S                   LIKE(GDTURN)                         SFL01 keys
     D @UITEM          S                   LIKE(GDITEM)                            "   "
     D @UORD#          S                   LIKE(GDORD#)                            "   "
     D @UORL#          S                   LIKE(GDORL#)                            "   "
     D @UBLN#          S                   LIKE(GDBLN#)                            "   "
     D @URSEQ          S                   LIKE(GDRSEQ)                            "   "
     D @USSCCBC        S                   LIKE(GDSSCCBC)                          "   "
     D @DTURN          S                   LIKE(GDTURN)                            "   "
     D @DITEM          S                   LIKE(GDITEM)                            "   "
     D @DORD#          S                   LIKE(GDORD#)                            "   "
     D @DORL#          S                   LIKE(GDORL#)                            "   "
     D @DBLN#          S                   LIKE(GDBLN#)                            "   "
     D @DRSEQ          S                   LIKE(GDRSEQ)                            "   "
     D @DSSCCBC        S                   LIKE(GDSSCCBC)                          "   "

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D PrintMode       C                   '1'
     D ShutDownMode    C                   '9'
     D @SEL            C                   ' 1'
     D @SEL2           C                   '1 '
     D MixedLblHdg     C                   'Mixed-Carton Label Selection'
     D PrtlLblHdg      C                   'Partial-Carton Label Selection'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode
     C                   EVAL      wk_ShipGrp# = in_ShipGrp#
     C                   EVAL      wk_BillTo# = in_BillTo#
     C                   EVAL      wk_ShipTo# = in_ShipTo#
     C                   EVAL      wk_DropShip# = in_DropShip#
     C                   EVAL      wk_LblType = in_LblType
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

     C                   IF        wk_PgmMode = ShutDownMode
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF

     C                   DO

     C                   EXSR      $InzSR

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip#;
      /End-Free

     C                   IF        wk_RcdCnt > *Zero
     C                   EXSR      $BldSfl01
     C                   ELSE
     C                   EXSR      $ClearSfl01
     C                   EVAL      *IN93 = *OFF
     C                   LEAVE
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C                   DOW       not *IN12

      *
      * Display Subfile
      *
     C                   WRITE     MSFCTL01A
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
     C                   EXFMT     SFLCTL01A

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EXSR      $RMVPM

      *
      * Valid Command Key?
      *
     C                   IF        *IN25

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ENDIF                                                  *IN12

     C                   EVAL      wk_Error = *OFF

     C                   SELECT

      *
      * Refresh?
      *
     C                   WHEN      *IN05
     C                   EXSR      $BldSfl01
     C                   EVAL      *IN05 = *OFF

      *
      * Select all?
      *
     C                   WHEN      *IN21
     C                   IF        wk_Sfl01RRN <> *Zero
     C                   EVAL      wk_SelDft = *ON
     C                   EXSR      $BldSfl01
     C                   EVAL      wk_SelDft = *OFF
     C                   EVAL      *IN21 = *OFF
     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero
     C                   ITER

     C                   ENDSL

     C                   ELSE

      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf1

      *
      * If all options are valid, process the selections.
      *
     C                   IF        not wk_Error

      *
      * If user pressed Enter and the subfile is empty, exit program.
      *
     C                   IF        wk_Sfl01RRN = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

      *
      * If user pressed Enter, but no options are present, or at least two
      * items have not been selected for a mixed-carton label, show error.
      *
     C                   IF        not wk_OptPr  or
     C                               (wk_LblType = 'M'  and
     C                                wk_SelItem# <> '*MIXED')
     C                   SELECT
     C                   WHEN      wk_LblType = 'P'
     C                   EVAL      wK_MsgID = 'ERR1456'
     C                   EVAL      wk_MsgData = *Blanks
     C                   WHEN      wk_LblType = 'M'
     C                   EVAL      wK_MsgID = 'ERR1457'
     C                   EVAL      wk_MsgData = *Blanks
     C                   ENDSL
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  not wk_OprPr or

      *
      * Read and Process Subfile
      *
     C                   EXSR      $ProcessSf1
     C                   IF        not wk_MoreAvail
     C                   LEAVE
     C                   ENDIF                                                  not wk_MoreAvail
     C                   ITER

     C                   ELSE
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  not wk_Error

     C                   ENDIF                                                  *IN25=*ON

     C                   ENDDO                                                  not *IN12

     C                   ENDDO

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   RETURN

     C/EJECT
      **************************************************************************
      *****                S  U  B  R  O  U  T  I  N  E  S                 *****
      **************************************************************************
      **************************************************************************
      * Send message to this program's message queue.                          *
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
      * Send message to calling program's message queue.                       *
      **************************************************************************
     CSR   $SndPM2       BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*PGMBDY' : 1 : in_MsgKey :
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
      *  Build subfile SFL01 - main subfile.                                   *
      **************************************************************************
     CSR   $BldSfl01     BEGSR

     C                   EXSR      $ClearSfl01

     C                   EVAL      wk_DftOrd# = *Zero
     C                   EVAL      wk_DftItem = *Blanks

     C     key_GD1       SETLL     HYLSGDT3

     C                   DOU       %eof(HYLSGDT3)

     C     key_GD1       READE     HYLSGDT3

     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)

     C                   EXSR      $Filter1
     C                   IF        not wk_Include
     C                   ITER
     C                   ENDIF                                                  not wk_Include

      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   IF        wk_SelDft
     C                   IF        (wk_DftOrd# = *Zero  or
     C                                GDORD# = wk_DftOrd#)  and
     C                             (wk_LblType = 'M'  or
     C                             (wk_LblType = 'P'  and
     C                              (wk_DftItem = *Blanks  or
     C                               GDITEM = wk_DftItem)))
     C                   EVAL      S1OPT = @SEL
     C                   EVAL      wk_DftOrd# = GDORD#
     C                   EVAL      wk_DftItem = GDITEM
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                  wk_DftOrd#=*Zero or
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                  wk_SelDft
     C                   IF        S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ELSE
     C                   EVAL      *IN28 = *ON
     C                   ENDIF                                                  S1OPT=*Blanks

      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSfl01

      *
      * Increment Rel Rec Nbr, write subfile record.
      *
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1

     C                   WRITE     SFL01A

     C                   ENDDO                                                  %eof(HYLSGDT3)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear subfile 01, initialize relative record and page count.           *
      **************************************************************************
     CSR   $ClearSfl01   BEGSR

     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN27 = *OFF
     C                   WRITE     SFLCTL01A
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero

     C                   EVAL      *IN41 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSfl01  BEGSR

      /Free
       wk_ODWH = *Zero;
       wk_IWFTCL = *Blanks;
       EXEC SQL select ODWH into :wk_ODWH
                from OEORDT
                where ODORD#=:GDORD# and ODORL#=:GDORL# and
                      ODBLN#=:GDBLN#;
       EXEC SQL select IWFTCL into :wk_IWFTCL
                from HDIWHS
                where IWITEM=:GDITEM and IWWHS=:wk_ODWH;
       IF wk_IWFTCL = 'AMMOB';
         *IN61 = *ON;
       ELSE;
         *IN61 = *OFF;
       ENDIF;                                                                // wk_IWFTCL='AMMOB'
       S1IN61 = *IN61;
      /End-Free

     C                   EVAL      S1TURN = GDTURN
     C                   EVAL      S1TSEQ = GDTSEQ
     C                   EVAL      S1RSEQ = GDRSEQ
     C                   EVAL      S1OCTL = GDOCTL
     C                   EVAL      S1ORD# = GDORD#
     C                   EVAL      S1ORL# = GDORL#
     C                   EVAL      S1BLN# = GDBLN#
     C                   EVAL      S1ITEM = GDITEM
     C                   EVAL      S1ITEMX = GDITEM
     C                   EVAL      S1IMDS = GDIMDS
     C                   EVAL      S1QOPN = GDQOPN
     C                   EVAL      S1QOPK = GDQOPK
     C                   EVAL      S1QOPKX = GDQOPK
      *
     C                   IF        GDQOPK <> GDQOPKO
     C                   EVAL      *IN62 = *ON
     C                   ELSE
     C                   EVAL      *IN62 = *OFF
     C                   ENDIF
      *
     C                   EVAL      S1QSTC = GDQSTC
     C                   EVAL      S1QSTCX = GDQSTC
     C                   EVAL      S1CNVY = GDCNVY
     C                   EVAL      S1TOTE = GDTOTE
     C                   EVAL      S1MXPO = GDMXPO
     C                   EVAL      S1MXLT = GDMXLT
     C                   EVAL      S1PO# = GDPO#
     C                   EVAL      S1PLOCX = GDPLOC
     C                   EVAL      S1SSCCBC = GDSSCCBC
     C                   EVAL      S1SSCC = GDSSCC
     C                   SELECT
     C                   WHEN      GDQOPK = *Zero
     C                   EVAL      S1STS = *Blanks
     C                   WHEN      GDQSTC = *Zero
     C                   EVAL      S1STS = ShipOrPick (GDGRP#:GDTURN:
     C                               GDORD#:GDORL#)
     C                   WHEN      GDQSTC < GDQOPK
     C                   EVAL      S1STS = 'Prtl'
     C                   WHEN      TVNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      S1STS = 'Tote'
     C                   WHEN      LotsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ:GDSSCCBC:GDSSCC)
     C                   EVAL      S1STS = 'Lot'
     C                   WHEN      LblsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDOCTL:GDORL#:GDBLN#:GDLBL1:GDLBL2:GDLBL3)
     C                   EVAL      S1STS = 'Label'
     C                   WHEN      PltzNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDORD#:GDORL#:GDBLN#:GDSSCC)
     C                   EVAL      S1STS = 'Pallt'
     C                   OTHER
     C                   EVAL      S1STS = *Blanks
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01 main subfile.                                             *
      **************************************************************************
     CSR   $Filter1      BEGSR

     C                   EVAL      wk_Include = *ON

      *
      * Must be fully shipped.
      *
     C                   IF        GDQSTC = *Zero  or  GDQSTC <> GDQOPK
     C                   EVAL      wk_Include = *OFF
     C                   ENDIF                                                  GDQSTC=*Zero or

      *
      * Must not have already printed a label.
      *
     C                   IF        GDSSCC <> *Zero
     C                   EVAL      wk_Include = *OFF
     C                   ENDIF                                                  GDSSCC<>*Zero

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
     C                   EVAL      wk_SelPr = *OFF
     C                   EVAL      wk_OptPr = *OFF
     C                   EVAL      wk_ClrDone = *OFF
     C                   EVAL      wk_SelItem# = *Blanks
     C                   EVAL      wk_SelOrder# = *Zero

      *
      * Save function key indicators.
      *
     C                   EVAL      wk_IN25 = *IN25

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01A

     C                   DOW       not %eof(HYD0626A)

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
      * Selects present?
      *
     C                   IF        (S1OPT = @SEL  or  S1OPT = @SEL2)

     C                   EVAL      S1OPT = @SEL

     C                   EVAL      wk_SelChk =
     C                               SelOK(wk_ShipGrp#:S1TURN:S1TSEQ:S1SSCCBC:
     C                                     S1ORD#:S1ITEM:wk_LblType)

     C                   IF        wk_SelChk = *Zeros

     C                   EVAL      wk_SelPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   UPDATE    SFL01A

     C                   IF        wk_SelItem# = *Blanks
     C                   EVAL      wk_SelItem# = S1ITEM
     C                   ELSE
     C                   IF        wk_SelItem# <> S1ITEM  and
     C                               wk_SelItem# <> '*MIXED'
     C                   EVAL      wk_SelItem# = '*MIXED'
     C                   ENDIF                                                  wk_SelItem#<>S1ITEM
     C                   ENDIF                                                  wk_SelItem#=*Blanks

     C                   IF        wk_SelOrder# = *Zero
     C                   EVAL      wk_SelOrder# = S1ORD#
     C                   ELSE
     C                   IF        wk_SelOrder# <> S1ORD#  and
     C                               wk_SelOrder# <> -1
     C                   EVAL      wk_SelOrder# = -1
     C                   ENDIF                                                  wk_SelOrder#<>S1ORD#
     C                   ENDIF                                                  wk_SelOrder#=*Zero

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1244'
     C                   EVAL      wk_MsgData = wk_SelChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   UPDATE    SFL01A

     C                   ENDIF                                                  wk_SelChk=*Zeros

     C                   ELSE

     C                   IF        S1OPT <> *Blanks

      *
      *  Set Cursor on First Error
      *
     C                   IF        not wk_Error
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   UPDATE    SFL01A
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EXSR      $SndPM

     C                   ELSE

      *
      * Ignore Blank Options, Turn off Error & SFLNXTCHG
      *
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN61 = S1IN61
     C                   UPDATE    SFL01A

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@SEL or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01A

     C                   ENDDO                                                  not %eof(HYD0626A)

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
     C                   IF        wk_SelPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_SelPr or
     C                   ELSE
     C                   IF        wk_SelPr
     C                   EVAL      wk_OptPr = *ON
     C                   ENDIF                                                  wk_SelPr or
     C                   ENDIF                                                  *IN25

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process options for subfile SFL01.                                     *
      **************************************************************************
     CSR   $ProcessSf1   BEGSR

     C                   READC     SFL01A

     C                   DOW       not %eof(HYD0626A)

     C                   IF        S1OPT = @SEL
     C                   EVAL      *IN28 = *ON
     C                   EXSR      $ProcSelects
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                   S1OPT=@SEL
     C                   EVAL      *IN61 = S1IN61
     C                   UPDATE    SFL01A

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0626A)
     C                   READC     SFL01A
     C                   ENDIF                                                  not %eof(HYD0626A)

     C                   ENDDO                                                  not %eof(HYD0626A)

     C                   IF        wk_PrtLabel
     C                   EXSR      $PrtLabel
     C                   IF        wk_MoreAvail
     C                   EXSR      $BldSfl01
     C                   ENDIF                                                  wk_MoreAvail
     C                   EVAL      wk_PrtLabel = *OFF
     C                   ELSE
     C                   EVAL      wk_MoreAvail = *ON
     C                   ENDIF                                                  wk_PrtLabel

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process selects.                                                       *
      **************************************************************************
     CSR   $ProcSelects  BEGSR

     C                   EVAL      wk_LblQty = *Zero
     C                   EVAL      wk_LblUOM = *Blanks

      /Free
       wk_GDDVRT = 0;

       EXEC SQL insert into HYW0602
                       (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                        W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                        W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                values(:ps_Job#, :wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                       :wk_DropShip#, :S1TURN, :S1TSEQ, :S1RSEQ,
                       :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);

       EXEC SQL select GDDVRT
                  into :wk_GDDVRT
                  from HYPSGDT
                 where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                       GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                       GDOCTL=:S1OCTL and GDORD#=:S1ORD# and
                       GDORL#=:S1ORL# and GDBLN#=:S1BLN#;
      /End-Free

     C                   IF        wk_LblType = 'P'
     C                   EVAL      wk_LblQty = wk_LblQty + S1QSTC
     C                   IF        wk_LblUOM = *Blanks
      /Free
       wk_IMUOMS = *Blanks;
       EXEC SQL select IMUOMS into :wk_IMUOMS
                from HDIMST a
                where IMITEM=:S1ITEM;
       wk_LblUOM = wk_IMUOMS;
      /End-Free
     C                   ENDIF                                                  wk_LblUOM=*Blanks
     C                   ENDIF                                                  wk_LblType='P'

     C                   EVAL      wk_PrtLabel = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print label.                                                           *
      **************************************************************************
     CSR   $PrtLabel     BEGSR

      /Free
       wk_SSCC = *Zero;
       EXSR $WrtHYPSCWK;

       EXEC SQL update HYPSGDT a
                 set GDLBL1='Y', GDSSCC=:wk_SSCC, GDSSCCBC=:wk_SSCCBC
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDSSCCBC=' ' and
                 exists (select * from HYW0602 b where
                 b.W1JOB#=:ps_Job# and b.W1GRP#=a.GDGRP# and
                 b.W1BLTO=a.GDBLTO and b.W1SHTO=a.GDSHTO and
                 b.W1DSHP=a.GDDSHP and b.W1OCTL=a.GDOCTL and
                 b.W1ORD#=a.GDORD# and b.W1ORL#=a.GDORL# and
                 b.W1BLN#=a.GDBLN# and b.W1RSEQ=a.GDRSEQ);
       EXEC SQL delete from HYW0602
                where W1JOB#=:ps_Job#;

       wk_Pgm01 = *ON;
       wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:wk_SSCCBC:
                                 wk_LblType:*ON:*ON:wk_LblQty:wk_LblUOM);

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDQSTC>0 and GDQSTC=GDQOPK and GDSSCC=0;
       IF wk_RcdCnt = *Zero;
         wk_MoreAvail = *OFF;
       ELSE;
         wk_MoreAvail = *ON;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free

      *
      * Send "Mixed label printed" or "Partial label printed" message.
      *
     C                   EVAL      wk_MsgID = 'MSG0377'
     C                   IF        wk_LblType = 'M'
     C                   EVAL      wk_MsgData = 'Mixed carton'
     C                   EVAL      %subst(wk_MsgData:15:30) = '.'
     C                   ELSE
     C                   EVAL      wk_MsgData = 'Partial carton'
     C                   EVAL      %subst(wk_MsgData:15:30) = ' for item ' +
     C                               %trimr(wk_SelItem#) + '.'
     C                   ENDIF                                                  wk_LblType='M'
     C                   IF        wk_MoreAvail
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EXSR      $SndPM2
     C                   ENDIF                                                  wk_MoreAvail

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write HYPSCWK records.                                                 *
      **************************************************************************
     CSR   $WrtHYPSCWK   BEGSR


     C                   IF        not %open(HYW0602)
     C                   OPEN      HYW0602
     C                   ENDIF                                                  not %open(HYW0602)

     C                   IF        not %open(HYPSGLD)
     C                   OPEN      HYPSGLD
     C                   ENDIF                                                  not %open(HYPSGLD)

     C                   IF        not %open(HYPSCWK)
     C                   OPEN      HYPSCWK
     C                   ENDIF                                                  not %open(HYPSCWK)

     C                   IF        wk_SSCC = *Zero
     C     *LOCK         IN        wk_SSCCSrlRef
     C                   EVAL      wk_SSCCSrlRef = wk_SSCCSrlRef + 1
     C                   OUT       wk_SSCCSrlRef
     C                   EVAL      wk_SSCC = wk_SSCCSrlRef
     C                   ENDIF                                                  wk_SSCC=*Zero
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(wk_SSCC:'X')
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt

     C     key_W1        SETLL     HYW0602
     C                   DOU       %eof(HYW0602)
     C     key_W1        READE     HYW0602
     C                   IF        %eof(HYW0602)
     C                   ITER
     C                   ENDIF                                                  %eof(HYW0602)

      /Free

       wk_GDDVRT = 0;
       wk_GDTURN = *Zero;
       wk_GDTSEQ = *Zero;
       wk_GDITEM = *Blanks;
       wk_GDIMDS = *Blanks;
       wk_GDPLOC = *Blanks;
       wk_GDQSTC = *Zero;
       IF wk_LblType <> 'F';
         EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDIMDS,
                         GDPLOC, GDQSTC, GDDVRT
                   into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDIMDS,
                        :wk_GDPLOC, :wk_GDQSTC, :wk_GDDVRT
                  from HYPSGDT
                  where GDGRP#=:W1GRP# and GDBLTO=:W1BLTO and
                   GDSHTO=:W1SHTO and GDDSHP=:W1DSHP and
                   GDOCTL=:W1OCTL and GDORD#=:W1ORD# and
                   GDORL#=:W1ORL# and GDBLN#=:W1BLN# and
                   GDRSEQ=:W1RSEQ;
       ELSE;
         EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDIMDS,
                         GDPLOC, GDQSTC, GDDVRT
                   into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDIMDS,
                        :GDPLOC, :wk_GDQSTC, :wk_GDDVRT
                  from HYPSGDT
                  where GDGRP#=:W1GRP# and GDBLTO=:W1BLTO and
                   GDSHTO=:W1SHTO and GDDSHP=:W1DSHP and
                   GDOCTL=:W1OCTL and GDORD#=:W1ORD# and
                   GDORL#=:W1ORL# and GDBLN#=:W1BLN# and
                   GDSSCCBC=:wk_SSCCBC;
       ENDIF;                                                                // wk_LblType<>'F'
       IF wk_GDTURN = *Zero;
         ITER;
       ENDIF;                                                                // wk_GDTURN=*Zero

       wk_IMPCLS = *Blanks;
       wk_IMLOT = *Blanks;
       wk_IMUOMS = *Blanks;
       wk_IMUOMW = *Blanks;
       wk_IMIMWG = *Zero;
       wk_IMUDN1 = *Zero;
       wk_IMUDN4 = *Zero;
       EXEC SQL select IMPCLS, IMLOT, IMUOMS, IMUOMW,
                       IMIMWG, IMUDN1, IMUDN4
                 into :wk_IMPCLS, :wk_IMLOT, :wk_IMUOMS, :wk_IMUOMW,
                      :wk_IMIMWG, :wk_IMUDN1, :wk_IMUDN4
                from HDIMST
                where IMITEM=:wk_GDITEM;

       wk_TDKEY1 = *Blanks;
       EXEC SQL select TDKEY1 into :wk_TDKEY1
                from GUPTDAT
                where TDTABL='BULKPRDCLS' and TDKEY1=:wk_IMPCLS;
       IF wk_TDKEY1 = wk_IMPCLS;
         wk_Bulk = *ON;
       ELSE;
         wk_Bulk = *OFF;
       ENDIF;                                                                // wk_TDKEY1=wk_IMPCLS

       IF wk_Bulk;
         wk_UnitWgt = wk_IMUDN4;
       ELSE;
         wk_UnitWgt = wk_IMIMWG;
       ENDIF;                                                                // wk_Bulk

       wk_BDITEM = *Blanks;
       wk_BDUPC12 = *Blanks;
       EXEC SQL select BDITEM, BDUPC12
                 into :wk_BDITEM, :wk_BDUPC12
                from BARDATA
                where BDITEM=:wk_GDITEM;

       wk_LotCnt = *Zero;
       EXEC SQL select count(*) into :wk_LotCnt
                from HYPSGLD
                where LDGRP#=:W1GRP# and LDTURN=:wk_GDTURN and
                 LDTSEQ=:wk_GDTSEQ and LDRSEQ=:W1RSEQ;

       wk_Carton# = *Zero;
       EXEC SQL select coalesce(max(S1CTN#),0) into :wk_Carton#
                from HYPSCWK
                where S1ORD#=:W1ORD# and S1TURN=:wk_GDTURN;
       wk_Carton# = wk_Carton# + 1;
      /End-Free

     C                   EVAL      SCORD# = W1ORD#
     C                   EVAL      SCSLOC = %scanrpl('-':'':wk_GDPLOC)
     C                   EVAL      SCORL# = W1ORL#
     C                   EVAL      SCBLN# = W1BLN#
     C                   EVAL      SCSRL# = wk_SSCC
     C                   EVAL      SCITEM = wk_GDITEM
     C                   EVAL      SCIMDS = wk_GDIMDS
     C                   EVAL      SCUOM = wk_IMUOMS
     C                   EVAL      SCUOMQ = wk_GDQSTC
     C                   EVAL      SCQPER = wk_IMUDN1
     C                   EVAL      SCEAQ = wk_GDQSTC * wk_IMUDN1
     C                   EVAL      SCUOMW = wk_IMUOMW
     C                   EVAL      SCUWGT = wk_UnitWgt
     C                   EVAL(H)   SCEWGT = wk_GDQSTC * wk_UnitWgt
     C                   EVAL      SCUPC = wk_BDUPC12
     C                   EVAL      SCLTYP = wk_LblType
     C                   EVAL      SCCTN# = wk_Carton#
     C                   EVAL      SCTURN = wk_GDTURN
     C                   EVAL      SCPSRL# = *Zero
     C                   EVAL      SCPEMTY = *Blank
     C                   EVAL      SCPEMPL = *Zero
     C                   IF        wk_GDDVRT = 999
     C                   EVAL      SCEMTY = 'D'                                 DigiPick
     C                   ELSE
     C                   EVAL      SCEMTY = 'B'                                 BoxLogix
     C                   ENDIF
     C                   IF        wk_EMPL <> *Zero
     C                   EVAL      SCEMPL = wk_EMPL
     C                   ELSE
     C                   EVAL      SCEMPL = 2
     C                   ENDIF                                                  wk_EMPL<>*Zero
     C
     C                   EVAL      SCJOBN = ps_Job#
     C                   EVAL      SCTSTP = %timestamp
     C                   IF        wk_LotCnt = *Zero
     C                   EVAL      SCLOT# = *Blanks
     C                   WRITE     HYRSCWK
     C                   ELSE
     C                   EVAL      wk_TotLDQSTC = *Zero
     C     key_LD        SETGT     HYPSGLD
     C                   DOU       %eof(HYPSGLD)
     C     key_LD        READPE    HYPSGLD
     C                   IF        %eof(HYPSGLD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGLD)
     C                   IF        LDSSCC <> *Zero  and  LDSSCC <> wk_SSCC
     C                   UNLOCK    HYPSGLD
     C                   ITER
     C                   ENDIF                                                  LDSSCC<>*Zero and
     C                   IF        LDRSEQ <> W1RSEQ
     C                   UNLOCK    HYPSGLD
     C                   ITER
     C                   ENDIF                                                  LDRSEQ<>W1RSEQ
     C                   EVAL      SCLOT# = LDLOT#
     C                   EVAL      SCUOMQ = LDQSTC
     C                   EVAL      SCEAQ = LDQSTC * wk_IMUDN1
     C                   EVAL(H)   SCEWGT = LDQSTC * wk_UnitWgt
     C                   WRITE     HYRSCWK
     C                   EVAL      LDSSCC = wk_SSCC
     C                   UPDATE    HYRSGLD
     C                   EVAL      wk_TotLDQSTC = wk_TotLDQSTC + LDQSTC
     C                   IF        wk_TotLDQSTC >= wk_GDQSTC
     C                   LEAVE
     C                   ENDIF                                                  wk_TotLDQSTC>=wk_GDQ
     C                   ENDDO                                                  %eof(HYPSGLD)
     C                   ENDIF                                                  wk_LotCnt=*Zero

     C                   ENDDO                                                  %eof(HYW0602)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   IF        wk_Pgm01
     C                   CALLP(E)  sd_HYR0622
     C                   ENDIF                                                  wk_Pgm01

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - each call.                                    *
      **************************************************************************
     CSR   $InzSR        BEGSR

     C                   SELECT
     C                   WHEN      wk_LblType = 'M'
     C                   EVAL      C1HDG2 = MixedLblHdg
     C                   WHEN      wk_LblType = 'P'
     C                   EVAL      C1HDG2 = PrtlLblHdg
     C                   ENDSL

     C                   EVAL      C1HDG2 = center(C1HDG2)

     C                   EVAL      C1GRP# = %triml(%editc(wk_ShipGrp#:'Z'))

      /Free
       wk_GCSTNM = *Blanks;
       wk_GCSTYP = *Blank;
       EXEC SQL select GCSTNM, GCSTYP into :wk_GCSTNM, :wk_GCSTYP
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
      /End-Free

     C                   EVAL      C1SHTO = '#' +
     C                               %triml(%editc(wk_ShipTo#:'Z')) + '-' +
     C                               wk_GCSTNM
     C                   IF        wk_DropShip# <> *Zero
     C                   EVAL      %subst(C1SHTO:39:5) = ' (DS)'
     C                   ENDIF                                                  wk_DropShip#<>*Zero

     C                   EVAL      C1NOTE = *Blanks

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                      a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                      exists (select * from OEORDT b join HDIWHS c on
                      b.ODITEM=c.IWITEM and b.ODWH=c.IWWHS where
                      b.ODORD#=a.GDORD# and b.ODORL#=a.GDORL# and
                      b.ODBLN#=a.GDBLN# and c.IWFTCL='AMMOB');
       IF wk_RcdCnt > *Zero;
         %subst(C1NOTE:6:7) = x'32' + 'AMMOB' + x'20';
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   IN        wk_LDA
     C                   IF        %subst(wk_LDA:36:5) = 'EMPL:'
     C                   EVAL      wk_EMPL = %dec(%subst(wk_LDA:41:5):5:0)
      /Free
       wk_EMFNAM = *Blanks;
       wk_EMLNAM = *Blanks;
       EXEC SQL select EMFNAM, EMLNAM into :wk_EMFNAM, :wk_EMLNAM
                from HREMPL
                where EMEMPL=:wk_EMPL;
      /End-Free
     C                   EVAL      C1ENAM = 'Shipper: ' +
     C                               %subst(wk_EMFNAM:1:1) + ' ' + wk_EMLNAM
     C                   ELSE
     C                   EVAL      wk_EMPL = *Zero
     C                   EVAL      C1ENAM = *Blanks
     C                   ENDIF                                                  %subst(wk_LDA:36:5)=

     C                   IF        %subst(wk_LDA:93:8) = '*TOTEVFY'
     C                   EVAL      wk_ToteVfy = *ON
     C                   ELSE
     C                   EVAL      wk_ToteVfy = *OFF
     C                   ENDIF                                                  %subst(wk_LDA:93:8)

     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN93 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define  Key Lists
      *
     C     key_GD1       KLIST                                                  HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#

     C     key_W1        KLIST                                                  HYW0602
     C                   KFLD                    ps_Job#
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#

     C     key_LD        KLIST                                                  HYPSGLD
     C                   KFLD                    W1GRP#
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ

     C     RFSKEY1       KLIST                                                  SFL01 - HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UTURN
     C                   KFLD                    @UITEM
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     PGUKEY1       KLIST                                                  SFL01 - HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UTURN
     C                   KFLD                    @UITEM
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     PGDKEY1       KLIST                                                  SFL01 - HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @DTURN
     C                   KFLD                    @DITEM
     C                   KFLD                    @DORD#
     C                   KFLD                    @DORL#
     C                   KFLD                    @DBLN#
     C                   KFLD                    @DRSEQ
     C                   KFLD                    @DSSCCBC

      *
      * Initialize Work Fields
      *
     C                   EVAL      S@PGMQ = '*'
     C                   EVAL      P@PGNM = ps_Pgnm
     C                   EVAL      P@JOBN = ps_Job
     C                   EVAL      P@USER = ps_User

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'
     C                   EVAL      C1ENV = *Blanks
     C                   WHEN      wk_DatabaseID = 'SS'
     C                   EVAL      C1ENV = 'SS'
     C                   OTHER
     C                   EVAL      C1ENV = '*TEST*'
     C                   ENDSL

     CSR                 ENDSR

     P/EJECT
      **************************************************************************
      * Determine whether detail record can be selected for current function.  *
      **************************************************************************
     P SelOK           B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D SSCCBC                        20A   CONST
     D Order#                         8S 0 CONST
     D Item#                         15A   CONST
     D LblType                        1A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_Order#       S                   LIKE(GDORD#)
     D wk_Item#        S                   LIKE(GDITEM)
     D wk_LblType      S              1A
     D wk_RtnCode      S              2A
     D wk_GDLOT        S                   LIKE(GDLOT)
     D wk_GDQSTC       S                   LIKE(GDQSTC)
     D wk_RcdCnt       S              7P 0
     D wk_LDQSTC       S                   LIKE(LDQSTC)

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_LblType = LblType
     C                   EVAL      wk_RtnCode = *Zeros

      *
      * 01-UCC128 label has already been printed.
      *
     C                   IF        wk_SSCCBC <> *Blanks
     C                   EVAL      wk_RtnCode = '01'
     C                   LEAVE
     C                   ENDIF                                                  wk_SSCCBC<>*Blanks

      /Free
       //
       // 02-Lot number has not yet been entered.
       //
       wk_GDLOT = *Blanks;
       wk_GDQSTC = *Zero;
       EXEC SQL select GDLOT, GDQSTC into :wk_GDLOT, :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDSSCCBC=' ';
       IF wk_GDLOT <> *Blanks  and  wk_GDLOT <> *Zeros;
         wk_RcdCnt = *Zero;
         wk_LDQSTC = *Zero;
         EXEC SQL select count(*), coalesce(sum(LDQSTC),0)
                   into :wk_RcdCnt, :wk_LDQSTC
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDSSCC=0;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           wk_RtnCode = '02';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero or
       ENDIF;                                                                // wk_GDLOT<>*Blanks
      /End-Free

      *
      *   03-All items selected must on the same order.
      *
     C                   IF        wk_Order# <> wk_SelOrder#
     C                               and  wk_SelOrder# <> *Zero
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVE
     C                   ENDIF                                                  wk_Order#<>wk_SelOrd

      *
      *   04-All items selected for a partial carton label must be the same.
      *
     C                   IF        wk_LblType = 'P'                             Partial Label
     C                   IF        wk_Item# <> wk_SelItem#
     C                               and  wk_SelItem# <> *Blanks
     C                   EVAL      wk_RtnCode = '04'
     C                   LEAVE
     C                   ENDIF                                                  wk_Item#<>wk_SelItem
     C                   ENDIF                                                  wk_LblType='P'

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P SelOK           E

     P/EJECT
      **************************************************************************
      * Determine whether lot number entry is required.                        *
      **************************************************************************
     P LotsNeeded      B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST
     D SSCCBC                        20A   CONST
     D SSCC                           9S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_SSCC         S                   LIKE(GDSSCC)
     D wk_LotsNeeded   S              1N
     D wk_GDIMDS       S             30A
     D wk_GDLOT        S              2A
     D wk_GDQSTC       S             13P 4
     D wk_LDQSTC       S             13P 4
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_LotsNeeded = *OFF

      /Free
       wk_GDIMDS = *Blanks;
       wk_GDLOT = *Blanks;
       wk_GDQSTC = *Zero;
       EXEC SQL select GDIMDS, GDLOT, GDQSTC
                 into :wk_GDIMDS, :wk_GDLOT, :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDTURN=:wk_Turn# and GDTSEQ=:wk_TurnSeq# and
                 GDRSEQ=:wk_RcdSeq# and GDSSCCBC=:wk_SSCCBC;
       IF wk_GDLOT <> *Blanks  and
           (wk_GDLOT <> *Zeros or
            %subst(wk_GDIMDS:1:5)='SAMMO' or
            %subst(wk_GDIMDS:1:5)='SFRON');
         wk_RcdCnt = *Zero;
         wk_LDQSTC = *Zero;
         EXEC SQL select count(*), coalesce(sum(LDQSTC),0)
                   into :wk_RcdCnt, :wk_LDQSTC
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDRSEQ=:wk_RcdSeq#;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           wk_LotsNeeded = *ON;
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero or
       ENDIF;                                                                // wk_GDLOT<>*Blanks
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_LotsNeeded

     P LotsNeeded      E

     P/EJECT
      **************************************************************************
      * Determine whether labels have not yet been generated.                 *
      **************************************************************************
     P LblsNeeded      B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D OrdCtl#                        8S 0 CONST
     D Line#                          3S 0 CONST
     D Blanket#                       3S 0 CONST
     D Lbl1                           1A   CONST
     D Lbl2                           1A   CONST
     D Lbl3                           1A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_OrdCtl#      S                   LIKE(GDOCTL)
     D wk_Line#        S                   LIKE(GDORL#)
     D wk_Blanket#     S                   LIKE(GDBLN#)
     D wk_Lbl1         S                   LIKE(GDLBL1)
     D wk_Lbl2         S                   LIKE(GDLBL2)
     D wk_Lbl3         S                   LIKE(GDLBL3)
     D wk_LblsNeeded   S              1N

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_OrdCtl# = OrdCtl#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_Lbl1 = Lbl1
     C                   EVAL      wk_Lbl2 = Lbl2
     C                   EVAL      wk_Lbl3 = Lbl3
     C                   EVAL      wk_LblsNeeded = *OFF

     C                   IF        wk_Lbl1 <> 'Y'
     C                   EVAL      wk_LblsNeeded = *ON
     C                   ENDIF                                                  wk_Lbl1='Y'

     C                   ENDDO

     C                   RETURN    wk_LblsNeeded

     P LblsNeeded      E

     P/EJECT
      **************************************************************************
      * Determine whether totes need verified.                                 *
      **************************************************************************
     P TVNeeded        B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_TVNeeded     S              1N
     D wk_GDTOTE       S             10A
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_TVNeeded = *OFF

     C                   IF        not wk_ToteVfy
     C                   LEAVE
     C                   ENDIF                                                  not wk_ToteVfy

      /Free
       wk_GDTOTE = *Blanks;
       EXEC SQL select GDTOTE
                 into :wk_GDTOTE
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDTURN=:wk_Turn# and GDTSEQ=:wk_TurnSeq# and
                 GDRSEQ=:wk_RcdSeq#;
       IF wk_GDTOTE = *Blanks  or  %subst(wk_GDTOTE:1:3) = 'PLT';
         LEAVE;
       ENDIF;                                                                // wk_GDTOTE=*Blanks

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 GDTURN=:wk_Turn# and GDTSEQ=:wk_TurnSeq# and
                 GDRSEQ=:wk_RcdSeq# and
                 a.GDQSTC>(select coalesce(sum(b.TSQSCN),0)
                 from HYPSGTS b where b.TSGRP#=a.GDGRP# and
                 b.TSTURN=a.GDTURN and b.TSTSEQ=a.GDTSEQ and
                 b.TSRSEQ=a.GDRSEQ);
       IF wk_RcdCnt > *Zero;
         wk_TVNeeded = *ON;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_TVNeeded

     P TVNeeded        E

     P/EJECT
      **************************************************************************
      * Determine whether item has been palletized.                           *
      **************************************************************************
     P PltzNeeded      B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Order#                         8S 0 CONST
     D Line#                          3S 0 CONST
     D Blanket#                       3S 0 CONST
     D SSCC                           9S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Order#       S                   LIKE(GDORD#)
     D wk_Line#        S                   LIKE(GDORL#)
     D wk_Blanket#     S                   LIKE(GDBLN#)
     D wk_SSCC         S                   LIKE(GDSSCC)
     D wk_PltzNeeded   S              1N
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_PltzNeeded = *OFF

     C                   IF        wk_GCSTYP <> 'T'
     C                   LEAVE
     C                   ENDIF                                                  wk_GCSTYP<>'T'

     C                   IF        wk_SSCC = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_SSCCBC=*Zero

      /Free
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSCWK
                where S1ORD#=:wk_Order# and S1ORL#=:wk_Line# and
                 S1BLN#=:wk_Blanket# and S1SRL#=:wk_SSCC and
                 S1PSRL#=0;
       IF wk_RcdCnt > *Zero;
         wk_PltzNeeded = *ON;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_PltzNeeded

     P PltzNeeded      E

     P/EJECT
      **************************************************************************
      * Determine whether the status is 'Ship' or 'Pick'                       *
      **************************************************************************
     P ShipOrPick      B
     D                 PI             4A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D Order#                         8S 0 CONST
     D Line#                          3S 0 CONST

     D wk_Status       S              4A
     D wk_ShipGrp#     S                   like(GDGRP#)
     D wk_Turn#        S                   like(GDTURN)
     D wk_Order#       S                   like(GDORD#)
     D wk_Line#        S                   like(GDORL#)
     D wk_PackLane     S             10A
     D wk_PickStatus   S              1A

      /Free

       wk_Status = 'Ship';
       wk_ShipGrp# = ShipGrp#;
       wk_Turn# = Turn#;
       wk_Order# = Order#;
       wk_Line# = Line#;

       wk_PackLane = *Blanks;
       EXEC SQL select GDDVRT
                  into :wk_PackLane
                  from HYPSGDT
                 where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                  GDORD#=:wk_Order# and GDORL#=:wk_Line#
                 fetch first row only;

       if wk_PackLane = '999';
         EXEC SQL select PICKITMST
                    into :wk_PickStatus
                    from PICKBATDP
                   where PICKTURN = :wk_Turn# and PICKORD = :wk_Order# and
                         PICKORDL = :wk_Line#;
         if wk_PickStatus = *blanks;
           wk_Status = 'Pick';
         endif;
       endif;

       return wk_Status;

      /End-Free

     P ShipOrPick      E

     P/EJECT
