     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
    H/TITLE IS215R3- DOCUMENT SPLITTING                             *
    H*****************************************************************
    H* INDICATOR USAGE                                               *
    H*  40    GENERAL CHAIN                                          *
    H*  60    GENERAL READ IND.                                      *
    H*****************************************************************
    H* RETURN CODES :
     *
     ***FROM: IDXPGM (ISIS01-99 SERIES)- SPLIT PGM IDXPGM WITH ISIS AS PRE.
     *
     *          "C"   F12 PRESSED                           *CMDKEY
     *          "3"   F3  PRESSED                           *CMDKEY
     *
     ***TO:   CALLING PROGRAM
     *
     *          "C"   F12 PRESSED FROM IDXPGM               *CMDKEY
     *          "3"   F3  PRESSED FROM IDXPGM               *CMDKEY
     *          "R"   RETAIN RCD ON QUEUE AFTER INDEX!      *PROCESS
     *
    H*****************************************************************
     * REVISIONS
     *                                                               *
     * 04/15/13 C:062 Redmine: 2212  CSI                             *
     *                Call split server process                      *
     *                                                               *
     * 01/27/00 C:113 PROJECT: DR095                                 *
     *                Changed to use the Work With Key Types program *
     *                (SC092R) for key type searches.                *
     *                                                               *
     * 01/04/00 C:113 PROJECT: SUPPORT
     *                Corrected date conversion.
     *
     *****************************************************************
    F*
     Fimdtl     if   e           k disk
     Fimiell    if   e           k disk
     F                                     infds(filds1)
     Fimiql     if   e           k disk
     Fimdol1    if   e           k disk
     Fimbml     if   e           k disk
    F*** BEGIN ADD *** 01/27/00 ***********************************************
    F*
    F** Key Type File
     Fsmktp     if   e             disk
    F*** END   ADD *** 01/27/00 ***********************************************
    D*
     D                 ds
    D*
     D iesdt                          8  0
     D  xiey                          4  0 overlay(iesdt)
     D  xiemd                         4  0 overlay(iesdt:5)
    D*
     D                 ds
    D*
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D                 ds
     D xxrecf                        10
     D  xxscr#                        4    overlay(xxrecf:2)
    D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*
     D filds1          ds
     D  fil1             *file
     D  rec1             *record
     D  op1              *opcode
     D  sts1             *status
     D  rtn1             *routine
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D $$dcid          s             12
     D $$doct          s              8
     D $$fold          s             80
     D $$qid           s             10
     D $$sdt           s              8
     D $$stm           s              6
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D ##sdt           s              8  0
     D ##stm           s              6  0
     D ##view          s              1
     D #@rtc           s              1
     D idxpgm          s             10
     D q@rtc           s              1
     D q@rtrn          s              1
     D qqrtcd          s              1
     D qqrtn           s              6
     D sc#pgs          s              6  0
     D scbchi          s             12
     D scbdsc          s             40
     D scdcid          s             12
     D scddsc          s             40
     D scdoct          s              8
     D scpth           s             40
     D scqdsc          s             40
     D scsdat          s              8  0
     D scsrep          s              8
     D scstim          s              6  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxflr           s              8
     D xxkey           s             40
     D xxseq#          s              5  0
     D xxsflr          s             12
     D xx9a            s              9
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    I*
    C*
    C*   KEY LIST FOR THE DOCUMENT TYPE  FILE
    C*
     C     dtky          klist
     C                   kfld                    $$doct
    C*
    C*
    C*   KEY LIST FOR THE IMAGE QUEUE  FILE
    C*
     C     iqky          klist
     C                   kfld                    $$qid
    C*
    C*
    C*   KEY LIST FOR THE INDEXING FILE
    C*
     C     ieky          klist
     C                   kfld                    $$qid
     C                   kfld                    $$dcid
     C                   kfld                    ##sdt
     C                   kfld                    ##stm
    C*
    C*
    C*
    C*
    C*
     C     ieky1         klist
     C                   kfld                    $$qid
     C                   kfld                    scdcid
    C*
    C*
    C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
    C*
     C     doky          klist
     C                   kfld                    scdcid
    C*
    C*
    C*
     C     bmky1         klist
     C                   kfld                    dobchi
    C*
    C*   KEY LIST FOR THE DOCUMENT TRANSACTION FILE
    C*
     C     doky4         klist
     C                   kfld                    bmbchi
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     *** BEGIN ADD *** 01/04/00 ***********************************************
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *** END   ADD *** 01/04/00 ***********************************************
    C*** BEGIN ADD *** 01/27/00 ***********************************************
     C     acdpl1        plist
     C                   parm      '2'           ##call
     C     *inkc         parm      *off          ##kc
     C     *inkl         parm      *off          ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      '2'           ##view
    C*** END   ADD *** 01/27/00 ***********************************************
    C*
     *------------------------*
     *   PARAMETER LIST FOR CALLED PROGRAM
     *------------------------*
    C*
     C     *entry        plist
     C                   parm                    $$qid
     C                   parm                    $$dcid
     C                   parm                    $$sdt
     C                   parm                    $$stm
     C                   parm                    $$doct
     C                   parm                    q@rtc
     C                   movel     q@rtc         #@rtc
     C                   clear                   q@rtc
    C*
     C                   time                    dsdttm
    C*
    C*
     C                   move      $$stm         ##stm
    C*
    C*** COVERT DATE TO YYYYMMDD FORMAT FOR FILE CHAINS ETC.
    C*
     C                   move      $$sdt         srdti
     C                   call      'ISCVTDT'     dtyymd
     C                   movel     srdto         ##sdt
    C*
    C*
    C*
     *------------------------*
     *** RESET POINT IN PROGRAM
     *------------------------*
     C     top           tag
    C*
    C*   INITIALIZE ROUTINE * * *
     C                   exsr      init
    C*
     C     strt          tag
    C*
     C     vwrqs         tag
    C*
     C     dtky          chain     pdt01                              60
    C*
     C                   movel     dtdoct        scdoct
     C                   movel     dtdesc        scddsc
     C                   clear                   idxpgm
     C                   movel     dtidxp        idxpgm
     C                   movel     'ISIS'        idxpgm
    C*
     C     iqky          chain     piq01                              60
     C                   if        *in60 = *off
     C                   movel     iqdesc        scqdsc
     C                   else
     C                   clear                   scqdsc
     C                   endif
    C*
    C*
     C     bmky1         chain     pbm01                              60
     C                   if        *in60 = *off
     C                   movel     bmbdsc        scbdsc
     C                   else
     C                   clear                   scbdsc
     C                   endif
    C*
    C****  GET THE RECORD TO PROCESS HERE ***
    C*
     C     ieky          setll     pie01                              60
     C     nxtie         tag
     C                   clear                   idxpgm
     C                   movel     dtidxp        idxpgm
     C                   movel     'ISIS'        idxpgm
     C     ieky          reade     pie01                                  60
     C                   if        *in60 = *off
    C*** IF WE GET A HIT , THIS RCD IS AVAIL.
    C*
    C*
     C                   movel     iedcid        scdcid
    C*
    C********* SCSDAT IS MDY ****
    C*
     *** BEGIN CHG *** 01/04/00 ***********************************************
    C********** XIEMD     MULT 10000     SCSDAT
    C**********           ADD  XIEY      SCSDAT
     C                   eval      srdti = iesdt
     C                   call      'ISCVTDT'     dtmdyy
     C                   movel     srdto         scsdat
     *** END   CHG *** 01/04/00 ***********************************************
     C                   move      iestm         scstim
     C                   movel     iesrep        scsrep
    C*
     C                   else
    C* * * NO HIT.... THIS SHOULD NOT HAPPEN....
     C                   clear                   scsdat
     C                   clear                   scsdat
     C                   clear                   scstim
     C                   clear                   scsrep
    C*
     C                   move      'E'           q@rtc
     C                   goto      term
     C                   endif
    C*
    C***
     C     doky          chain     pdo01                              60
     C                   if        *in60 = *off
    C*
     C                   movel     doflr         xxflr
     C                   movel     dosflr        xxsflr
     C     xxflr         cat       '/'           xx9a
     C     xx9a          cat       xxsflr        scpth
    C*
     C                   move      dopgs         sc#pgs
     C                   movel     dobchi        scbchi
    C*
    C*
     C                   else
    C*
    C*
     C                   clear                   scpth
    C*
     C                   clear                   sc#pgs
     C                   clear                   scbchi
     C                   endif
    C*
    C*** DONT DO VIEW IF COMING FROM INDEX PGM #@RTC = 'I'
    C*
     C                   if        #@rtc <> 'I'
     *
     *** CALL PROGRAM TO PROCESS THE VIEW REQUEST
     *
     C                   if        iefl01 = 'F'
    C*
    C*
    C*** CALL THE DSPFAXC PROGRAM TO DISPLAY THE FAX, ELSE
     C                   call      'DSPFAXC'
     C                   parm                    scdcid
     C                   parm      scpth         $$fold
     C                   parm      *blanks       q@rtrn
     *
     C                   else
    C*** IMAGE DISPLAY
     C                   clear                   qqrtn
    C*
     C                   call      'IS211C'
     C                   parm                    scdcid
     C                   parm                    xxflr
     C                   parm                    xxsflr
     C                   parm                    scdoct
     C                   parm                    scddsc
     C                   parm                    qqrtn
     *
     *
     *
     *
     C                   endif
     *
     C                   endif
     *
     *** CALL INDEX PROGRAM TO PERFORM INDEX OF THE DOCUMENT
     *
     C     idxdsp        tag
     C                   if        iefl01 = 'F'
     C                   movel     'F'           qqrtcd
     C                   else
     C                   movel     'I'           qqrtcd
     C                   endif
     *
     C                   call      idxpgm
     C                   parm                    scdcid
     C                   parm                    scpth
     C                   parm                    scdoct
     C                   parm                    scddsc
     C                   parm                    $$qid
     C                   parm                    scqdsc
     C                   parm                    scbchi
     C                   parm                    sc#pgs
     C                   parm                    scsdat
     C                   parm                    scstim
     C                   parm                    scsrep
     C                   parm                    xxkey
     C                   parm                    xxseq#
     C                   parm                    qqrtcd
     *
     **** PROCESS THE COMMAND KEYS 1ST!!!!
     *
     *
     **** ALLOW A F KEY TO RETURN AND ALLOW SELECT OF OTHER DOC TYPE.
     *
     C                   if        qqrtcd = '3'
     C                             or qqrtcd = 'C'
     *
    C*** ON RTRN WITH F3(3) OR F12(C) , NEED TO RETURN TO CALLING PGM WITH
    C* RETRN CODE INFO...
     C                   movel     qqrtcd        q@rtc
     C                   goto      term
     *
    C*
     C                   else
    C*
     *** IF THE PGM RTNS A RQS FOR A DIFFERENT KEY PROGRAM (INDEXPGM)
     C                   if        qqrtcd = 'K'
     **
     C                   exsr      newidx
     C*** Begin Add *** 04/15/13 *******************************************
     C                   Else
     C*
     C* Call split server to process document
     C*
     C                   call      'IS430C'
     C*** End   Add *** 04/15/13 *******************************************
     *
     C                   endif
     *
    C* * * PROCESSED NORMALLY (*) WILL OCCUR HERE ***
     C                   movel     qqrtcd        q@rtc
     C                   endif
     *
    C*
     C     term          tag
     C                   return
    C*
    C*** END OF PROGRAM
     C     endpgm        tag
     C                   move      *on           *inlr
    C*
     *====================================================================*
     * * *   INIT SUBROUTINE                                    I N I T ***
     *====================================================================*
     C     init          begsr
    C*
    C*
     C                   endsr
    C*
     *====================================================================*
     * * *   NEWIDX SUBROUTINE                              N E W I D X ***
     *====================================================================*
     C     newidx        begsr
    C*
     * * * DISPLAY LIST OF INDEX TYPES TO CHOOSE.
     *
     *** BEGIN DEL *** 01/27/00 ***********************************************
    C**********           CALL 'IS213R'
    C**********           PARM *BLANKS   QXIPGM 10
    C**********           PARM *BLANKS   QXRTCD  1
    C**********
    C**********           MOVELQXIPGM    IDXPGM
    C**********           MOVEL'ISIS'    IDXPGM
     *** END   DEL *** 01/27/00 ***********************************************
    C*** BEGIN ADD *** 01/27/00 ***********************************************
     C                   call      'SC092R'      acdpl1
    C*
     C                   if        *inkc = *on
     C                             or *inkl = *on
     C                             or ##rrn = *zeros
    C*
     C                   if        *inkc = *on
     C                   movel     '3'           q@rtc
     C                   endif
    C*
     C                   if        *inkl = *on
     C                   move      'C'           q@rtc
     C                   else
     C                   move      'P'           q@rtc
     C                   endif
    C*
     C                   goto      term
    C*
     C                   else
    C*
     C     ##rrn         chain     pkt01                              60
     C                   movel     ktsplt        idxpgm
     C                   endif
    C*
    C*** END   ADD *** 01/27/00 ***********************************************
     C                   goto      idxdsp
    C*
     C                   endsr
     *====================================================================*
