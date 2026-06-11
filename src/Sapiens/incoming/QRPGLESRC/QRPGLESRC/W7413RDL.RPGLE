     H Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H*
     H/TITLE W7413RDL - Print Department cover letter for new/renewals.
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program will print a department letter for a new   **
     H**     or renewal cover letter to then be included in the      **
     H**     resolved PDF policy page document.                      **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7413C2 - Polcy Page Resolve CL                         **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     W4116C - Department Letters                             **
     H**                                                             **
     H**   PROGRAMMER: C:65                                          **
     H**   CREATED:    06/13/15     PROJECT: WC2277                  **
     H**                                                             **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     FWSDLL     UF A E           K DISK
     FWSDLL3    O    E           K DISK
     F                                     RENAME(PDL01:PDL02)
     FWMDOL     IF   E           K DISK
     FSDCMLA    IF   E           K DISK    rename(pcm01:pcm0a)
     D PSDS          ESDS                  EXTNAME(SZQ1P)


     D hiCo#           s                   like(ELCo#)
     D hiFnd           s                   like(ELFnd)
     D xxReq#          s                   like(DLReq#)
     D xxDPCD          s                   like(DLDPCD)
     D xxCo#           s                   like(ELCo#)
     D xxFnd           s                   like(ELFnd)
     D xxEmp#          s                   like(ELEmp#)
     D xxDiv           s                   like(ELDiv)
     D xxFyr           s                   like(ELFyr)
     D xxDEPT          s                   like(DODEPT)
     D xxRMCD          s                   like(DORMCD)
     d xxSeq#          s              5  0 inz(0)
     d xxQue           s             10
     D i               s              3  0
     D
     D                 ds
     D  cd                     1     35  0 dim(5) ascend
     D  emcad1               001    007  0
     D  emcad2               008    014  0
     D  emcad3               015    021  0
     D  emcad4               022    028  0
     D  emcad5               029    035  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  ZZKEY                  1     40
     D  ZZCO#                  1      3  0
     D  ZZFND                  4      6  0
     D  ZZEMP#                 7     15  0
     D  ZZDIV                 16     20  0
     D
     IPDL01
     I              DLREQ#                      HIREQ#
     I              DLCO#                       HICO#
     I              DLFND                       HIFND
     I              DLFYR                       HIFYR
     I              DLFEMP                      HIFEMP
     I              DLFDIV                      HIFDIV
     I              DLTEMP                      HITEMP
     I              DLTDIV                      HITDIV
     I              DLAEFL                      HIAEFL
     I              DLDPCD                      HIDPCD
     I              DLFLDR                      HIFLDR
     I              DLDCID                      HIDCID
     I              DL#CPY                      HI#CPY
     I              DLQTFL                      HIQTFL
     I              DLSTAT                      HISTAT
     I              DLIUSR                      HIIUSR
     I              DLUSER                      HIUSER
     I              DLPGM                       HIPGM
     I              DLIPGM                      HIIPGM
     I              DLTIME                      HITIME
     I              DLITIM                      HIITIM
     I              DLDT                        HIDT
     I              DLIDT                       HIIDT
     C*
     C*  Admin/Darts Control File Key List
     C*
     C     keyWMEXL      KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    xxFND
    C*
    C*  KEY LIST FOR EMPLOYER MASTER FILE
    C*
     C     keyWMEML      klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
    C*  KEY LIST FOR EMPLOYER DETAIL FILE
    C*
     C     keyWDELL      klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C*
     C*  POLICY DEPT LETTER WORKFILE
     C*
     C     DLKY          KLIST
     C                   KFLD                    HICO#
     C                   KFLD                    HIFND
     C*
     C*  DEPT/DOC MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    xxDEPT
     C                   KFLD                    xxDPCD
     C                   KFLD                    qqFOLD
     C                   KFLD                    qqDCID
     C*
     C* Darts File
     C*
     C     keySDCMLA     KLIST
     C                   KFLD                    q#Key
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
    C*   ENTRY PARAMETERS
    C*
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqque            10
     C                   parm                    qqmode            1
     C                   parm                    qqhold            4
     C*
     C                   Move      qqco#         xxCo#
     C                   Move      qqfnd         xxFnd
     C                   Move      qqemp#        xxEmp#
     C                   Move      qqdiv         xxDiv
     C                   Move      qqfyr         xxFyr
     C*
     C                   Clear                   qqdcid           12
     C                   Clear                   qqfold           80
     C*
     C     keyWMEXL      Chain     pex00                              60
     C*
     C                   If        *in60 = *off
     C*
    C*  CHECK FOR PREVIOUS FUND YEAR (In same manner as policy page does)
    C*
     C                   Eval      xxfyr = xxfyr - 1
     C     keyWMEML      Chain     pem00                              60
     C     keyWDELL      Chain     pel00                              60
    C*
     C                   If        *in60 = *off
     C                   Eval      i = 1
     C     elpfdt        Lookup    cd(i)                              60  60
    C*
     C                   If        *in60 = *on
     C                             and elptdt < cd(i)
     C                   Move      *off          *in60
     C                   Endif
    C*
     C                   Endif
    C*
     C                   If        *in60 = *off
     C                   Move      *on           *in25
    C*
     C                   Else
     C                   Move      *off          *in25
     C                   Endif
    C*
     C                   Eval      xxfyr = xxfyr + 1
     C     keyWDELL      Chain     pel00                              60
     C*
     C* Set document to be passed to department letter program and return for printing.
     C*
     C                   If        *in25 = *on
     C                   Movel(p)  EXRCDC        qqdcid
     C                   Movel(p)  EXRCFD        qqfold
     C                   Move      EXRCDP        xxDPCD
     C                   Else
     C                   Movel(p)  EXNCDC        qqdcid
     C                   Movel(p)  EXNCFD        qqfold
     C                   Move      EXNCDP        xxDPCD
     C                   Endif
     C*
     C* Reprint the cover letter if called with Mode of 'R'.
     C*
     C                   If        qqMode = 'R'
     C                   MOVE      'PS'          xxDEPT            2
     C     DOKEY         Chain     PDO01
     C*
     C                   If        %found
     C                   Move      DORMCD        xxRMCD
      *
      *   ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       ZZKEY
     C                   MOVE      xxCO#         ZZCO#
     C                   MOVE      xxFND         ZZFND
     C                   MOVE      xxEMP#        ZZEMP#
     C                   MOVE      xxDIV         ZZDIV
     C                   MOVEL(P)  ZZKEY         Q#KEY            40
     C
     C     keySDCMLA     Setgt     PCM0A
     C     keySDCMLA     Readpe    PCM0A
     C                   Dow       not %eof and CMRMCD <> xxRMCD
     C     keySDCMLA     Readpe    PCM0A
     C                   Enddo
     C*
     C                   If        not %eof
     C*
     C                   Move      CMSEQ#        xxSEQ#
     C*
     C                   Eval      xxQue = qqque
     C                   If        xxQue = *blanks
     C                   Eval      xxQue = 'QPRINT'
     C                   Endif
     C*
     C                   Eval      CmdStr = 'SYPRTDOC DOC(' + %trim(CMDCID) +
     C                             ') FLR(' + %trim(CMFOLD) + ') OUTQ(' +
     C                             %trim(xxQue) + ') JOBQ(*NO)'
     C
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR          255
     C                   PARM                    LENGTH
     C*
     C* Release the spool file if output routing indicates release status.
     C*
     C                   If        qqhold = '*NO'
     C*
     C                   MOVE      *BLANKS       CMDSTR
     C                   Eval      CMDSTR = 'RLSSPLF FILE(QSYSPRT) SPLNBR(*' +
     C                              'LAST)'
     C                   Z-ADD     225           LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C                   Endif                                                  If qqhold = '*ON'
     C
     C                   Endif                                                  If not %eof
     C
     C                   Endif                                                  If %found
     C
     C                   Endif                                                  If qqMode
     C*
     C* Process and print the cover letter.
     C*
     C                   If        xxSeq# = 0 or qqMode <> 'R'
     C*
     C* Write a work file record for the department letter.
     C*
     C                   If        qqdcid <> ' ' and qqfold <> ' '
     C                             and xxDPCD <> ' '
     C*
     C                   EXSR      WRTUPD
     C*
     C* Call department letter override CL to generate the document/output.
     C*
     C                   EXSR      SUBMIT
     C*
     C                   Endif                                                  If qqdcid <> ' '
     C*
     C                   Endif                                                  If xxSeq# = 0
     C*
     C                   Endif                                                  If *IN60 = *OFF
     C*
     C                   Move      *on           *inlr
     C*
      *====================================================================*
      * * *   WRITE/UPDATE WORKFILE WSDLP - POLICY DEPT LETTER WORKFILE
      *====================================================================*
     C     WRTUPD        BEGSR
     C*
     C*  CALL CNTROL TO OBTAIN A 'REQUEST NUMBER'
     C*
     C                   EXSR      CNTROL
     C*
     C*  THIS IS A NEW REQUEST, INITIALIZE WORKFILE FIELDS
     C*
     C                   EXSR      INITAF
     C*
     C                   Z-ADD     xxCO#         DLCO#
     C                   Z-ADD     xxFND         DLFND
     C                   Z-ADD     xxFYR         DLFYR
     C                   Z-ADD     xxEMP#        DLFEMP
     C                   Z-ADD     xxDIV         DLFDIV
     C                   Z-ADD     xxEMP#        DLTEMP
     C                   Z-ADD     xxDIV         DLTDIV
     C                   MOVE      'Y'           DLAEFL
     C                   Z-ADD     xxREQ#        DLREQ#
     C*
     C                   MOVE      xxDPCD        DLDPCD
     C                   MOVE      qqFold        DLFLDR
     C                   MOVE      qqDCID        DLDCID
     C                   Z-ADD     1             DL#CPY
     C                   MOVE      ' '           DLQTFL
     C                   MOVE      'A'           DLSTAT
     C                   MOVEL     Q1USER        DLIUSR
     C                   MOVEL     Q1PGM         DLIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLIDT
     C                   Z-ADD     DSTIME        DLITIM
     C                   MOVEL(p)  QQQUE         DLOUTQ
     C*
     C                   WRITE     PDL02
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   INITIALIZE ADDITIONAL INPUT FILE FIELDS
      *====================================================================*
     C     INITAF        BEGSR
     C                   CLEAR                   PDL02
     C                   ENDSR
     C*
     C*****************************************************************
     C* CNTROL - SUBROUTINE TO MAINTAIN A UNIQUE REQUEST NUMBER
     C*****************************************************************
     C     CNTROL        BEGSR
     C*
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C*
     C     DLKY          CHAIN     PDL01                              65
     C*
     C     *IN65         IFEQ      *OFF
     C*
     C     HIREQ#        IFLT      99999
     C     HIREQ#        ADD       1             HIREQ#
     C                   ELSE
     C                   Z-ADD     1             HIREQ#
     C                   END
     C*
     C*  UPDATE CONTROL RECORD
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C                   Z-ADD     DSTIME        HITIME
     C                   MOVEL     Q1USER        HIUSER
     C                   MOVEL     Q1USER        HIPGM
     C*
     C                   UPDATE    PDL01
     C*
     C                   ELSE
     C*
     C*  WRITE THE CONTROL RECORD FOR DEPARTMENT REQUESTS
     C*
     C                   Z-ADD     1             HIREQ#
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C                   Z-ADD     *HIVAL        HIFYR
     C                   Z-ADD     *HIVAL        HIFEMP
     C                   Z-ADD     *HIVAL        HIFDIV
     C                   Z-ADD     *HIVAL        HITEMP
     C                   Z-ADD     *HIVAL        HITDIV
     C                   MOVE      *BLANKS       HIAEFL
     C                   MOVE      *BLANKS       HIDPCD
     C                   MOVE      *BLANKS       HIFLDR
     C                   MOVE      *BLANKS       HIDCID
     C                   Z-ADD     *HIVAL        HI#CPY
     C                   MOVE      *BLANKS       HIQTFL
     C                   MOVE      'A'           HISTAT
     C                   MOVE      Q1USER        HIIUSR
     C                   MOVE      Q1PGM         HIIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C                   Z-ADD     DSTIME        HIITIM
     C                   WRITE     PDL01
     C                   END
     C*
     C                   Z-ADD     HIREQ#        XXREQ#
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SUBMIT SUBROUTINE              ** Subroutine **   S U B M I T *
     C*****************************************************************
     C     SUBMIT        BEGSR
     C*
     C*   ASSIGN PARM FIELDS, THEN CALL OVERRIDE CLP
     C*
     C                   MOVE      xxREQ#        QQREQ#            5
     C                   MOVE      xxDPCD        QQDPCD            5
     C                   MOVEL(p)  Q1CUSR        QQRQSTR          10
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         QQDATE            8
     C*
     C                   CALL      'W4116C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQREQ#
     C                   PARM                    QQDPCD
     C                   PARM                    QQFold
     C                   PARM                    QQDCID
     C                   PARM                    QQRQSTR
     C                   PARM                    QQDATE
     C*
     C* Release the spool file if output routing indicates release status.
     C*
     C                   If        qqhold = '*NO'
     C*
     C                   MOVE      *BLANKS       CMDSTR
     C                   Eval      CMDSTR = 'RLSSPLF FILE(QSYSPRT) SPLNBR(*' +
     C                              'LAST)'
     C                   Z-ADD     225           LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C                   Endif
     C*
     C                   ENDSR
     C*
