     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*
     H* API - Produce CMS Claim letters based on the auto claim letter
     H*       control file.
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA049R1                                           *
     H*    CREATED: 02/02/11  C:235 PROJ: 114WXO                      *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/06/25 C:333  Project WC2998                               *
     H*                  Changes to CMS Settlement Reporting.         *
     H*                  - Recompiled.                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     FWAMLL     IF   E           K DISK
     F                                     RENAME(PML01:PML03)
     FWDCLL     UF A E           K DISK
     FWDCLL3    O    E           K DISK
     F                                     RENAME(PCL01:PCL02)
     FWMCMLH    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWDD1L     IF   E           K DISK
     FWDD1P     IF A E             DISK    RENAME(PD101:PD10P)
     D*
     D@FLAG            s               n
     D#mnths           s              3  0
     D*
     D@SDATE           s               d   datfmt(*usa)
     D@frdt            s               d   datfmt(*usa)
     D@exdt            s               d   datfmt(*usa)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     IPCL01
     I              CLREQ#                      HIREQ#
     I              CLCO#                       HICO#
     I              CLFND                       HIFND
     I              CLFYR                       HIFYR
     I              CLFCLM                      HIFCLM
     I              CLEMP#                      HIEMP#
     I              CLDIV                       HIDIV
     I              CLDPCD                      HIDPCD
     I              CLFLDR                      HIFLDR
     I              CLDCID                      HIDCID
     I              CL#CPY                      HI#CPY
     I              CLSTAT                      HISTAT
     I              CLIUSR                      HIIUSR
     I              CLUSER                      HIUSER
     I              CLPGM                       HIPGM
     I              CLIPGM                      HIIPGM
     I              CLDT                        HIDT
     I              CLTIME                      HITIME
     I              CLITIM                      HIITIM
     I              CLIDT                       HIIDT
     I*
     C*****************************************************************
     C* Key  Lists
     C*****************************************************************
     C*
     C*
     C*
     C     CLKY          KLIST
     C                   KFLD                    HICO#
     C                   KFLD                    HIFND
     C*
     C     CLKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    MLDPCD
     C                   KFLD                    MLFLDR
     C                   KFLD                    MLDCID
     C*
     C     CLKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    MLDPCD
     C                   KFLD                    XXREQ#
     C*
     C*
     C     CMLKY         KLIST
     C                   KFLD                    MLCO#             3 0
     C                   KFLD                    MLFND             3 0
     C*
     C     CMLKY1        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C*
     C*****************************************************************
     C* Mainline
     C*****************************************************************
     C*
     C                   READ      PML03                                60
     C                   DOW       *IN60=*OFF
     C*
     c     cmlky         setll     PCM01
     C     CMLKY         READE     PCM01                                  98
     C                   DOW       *IN98=*OFF
     C     CMSTAT        IFEQ      'O'
     C     CMSS#         CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CDDTOB        IFEQ      *ZEROS
     C     CDSEX         OREQ      *BLANKS
     C*
     C                   eval      @FLAG = *off
     C     CMLKY1        SETGT     PD101
     C     CMLKY1        READPE    PD101                                  62
     C     *IN62         DOWEQ     *OFF
     C     D1TYPE        IFEQ      'I'
     C     D1RSDT        ANDEQ     *ZEROS
     C                   eval      @FLAG = *on
     c                   LEAVE
     C                   ENDIF                                                  D1TYPE=I
     C     CMLKY1        READPE    PD101                                  62
     C                   ENDDO
     C*
     c     @FLAG         IFEQ      *OFF
     C     MLRMTH        IFEQ      *ZEROS
     C*
     C     MLEMTH        IFEQ      *ZEROS
     C                   EXSR      WRTUPD
     C                   EXSR      SUBMIT
     C                   EXSR      WRTLOG
     C                   ENDIF                                                  MLEMTH =0
     C*
     C     MLEMTH        IFGT      01
     C*
     C*******Diffrence between SYSTEM DATE and Claim Intial Date(CMIDT)************************
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         XXFRDT            8 0
     C                   TIME                    DSDTTM
     C*
     c                   move      dsdate        @sdate
     c                   move      xxfrdt        @frdt
     C     @SDATE        subdur    @frdt         #MNTHS:*months
     C                   IF        #MNTHS>MLEMTH
     C                   EXSR      WRTUPD
     C                   EXSR      SUBMIT
     C                   EXSR      WRTLOG
     C                   ENDIF                                                  #MNTHS>MLEMTH
     C                   ENDIF                                                  MLEMTH>1
     C*
     C                   Endif                                                MLRMTH>0
     C                   ELSE                                                 flag =*on
     C*
     C     MLRMTH        IFGT      *ZEROS
     C*
     C*******Diffrence between SYSTEM DATE and Extract Date(D1EXDT)*****************************
     C*
     C                   Z-ADD     D1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         XXEXDT            8 0
     C                   TIME                    DSDTTM
     C*
     c                   move      dsdate        @sdate
     c                   move      xxexdt        @exdt
     C     @SDATE        subdur    @exdt         #MNTHS:*months
     C                   IF        #MNTHS>=MLRMTH
     C                   EXSR      WRTUPD
     C                   EXSR      SUBMIT
     C                   EXSR      WRTLOG
     C                   ENDIF                                                  #MNTHS>=MLRMTH
     C                   ENDIF                                                  MLRMTH>0
     C                   ENDIF                                                  @FLAG=0
     C*
     C                   ENDIF                                                  CDDTOB=0
     C                   ENDIF                                                  *IN99
     C                   ENDIF                                                  CMSTAT=O
     C     CMLKY         READE     PCM01                                  98
     C                   ENDDO
     C*
     C                   READ      PML03                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C* ***************************************************************
     C* WRTUPD SUBROUTINE              ** Subroutine **   W R T U P D *
     C* ***************************************************************
     C     WRTUPD        BEGSR
     C*
     C*  CALL CNTROL TO OBTAIN A 'REQUEST NUMBER'
     C*
     C                   EXSR      CNTROL
     C*
     C*  IF THIS IS A NEW REQUEST, INITIALIZE WORKFILE FIELDS
     C*
     C                   CLEAR                   PCL02
     C*
     C                   Z-ADD     CMCO#         CLCO#
     C                   Z-ADD     CMFND         CLFND
     C                   Z-ADD     CMFYR         CLFYR
     C                   Z-ADD     CMCASE        CLFCLM
     C                   Z-ADD     XXREQ#        CLREQ#
     C                   Z-ADD     CMEMP#        CLEMP#
     C                   Z-ADD     CMDIV         CLDIV
     C*
     C                   MOVE      MLDPCD        CLDPCD
     C                   MOVE      MLFLDR        CLFLDR
     C                   MOVE      MLDCID        CLDCID
     C     ML#CPY        IFNE      *ZEROS
     C                   Z-ADD     ML#CPY        CL#CPY
     C                   ELSE
     C                   Z-ADD     1             CL#CPY
     C                   END
      *
     C                   MOVE      'A'           CLSTAT
     C                   MOVEL     Q1USER        CLIUSR
     C                   MOVEL     Q1PGM         CLIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C*
     C                   Z-ADD     DSTIME        CLITIM
     C                   MOVE      MLOUTQ        CLOUTQ
     C*
     C                   WRITE     PCL02
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************
     C* CNTROL - SUBROUTINE TO MAINTAIN A UNIQUE REQUEST NUMBER
     C*****************************************************************
     C     CNTROL        BEGSR
     C*
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C*
     C     CLKY          CHAIN     PCL01                              65
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
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C*
     C                   Z-ADD     DSTIME        HITIME
     C                   Z-ADD     DSTIME        HIITIM
     C                   MOVEL     Q1USER        HIIUSR
     C                   MOVEL     Q1USER        HIUSER
     C                   MOVEL     Q1PGM         HIIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C*
     C                   UPDATE    PCL01
     C*
     C                   ELSE
     C*
     C*  WRITE THE CONTROL RECORD FOR DEPARTMENT REQUESTS
     C*
     C                   Z-ADD     1             HIREQ#
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C                   Z-ADD     *HIVAL        HIFYR
     C                   Z-ADD     *HIVAL        HIFCLM
     C                   Z-ADD     *HIVAL        HIEMP#
     C                   Z-ADD     *HIVAL        HIDIV
     C                   MOVE      *BLANKS       HIDPCD
     C                   MOVE      *BLANKS       HIFLDR
     C                   MOVE      *BLANKS       HIDCID
     C                   Z-ADD     *HIVAL        HI#CPY
     C                   MOVE      'A'           HISTAT
     C                   MOVE      Q1USER        HIIUSR
     C                   MOVE      Q1PGM         HIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C*
     C                   Z-ADD     DSTIME        HITIME
     C                   MOVE      Q1USER        HIIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C*
     C                   WRITE     PCL01
     C                   END
     C*
     C                   Z-ADD     HIREQ#        XXREQ#
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************
     C* SUBMIT SUBROUTINE              ** Subroutine **   S U B M I T *
     C*****************************************************************
     C     SUBMIT        BEGSR
     C*
     C*   ASSIGN PARM FIELDS, THEN CALL SUBMITTER CLP
     C*
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      XXREQ#        QQREQ#
     C                   MOVE      MLDPCD        QQDPCD
     C                   MOVE      MLFLDR        QQFLDR
     C                   MOVE      MLDCID        QQDCID
     C*
     C                   CALL      'W4110C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQREQ#            5
     C                   PARM                    QQDPCD            5
     C                   PARM                    QQFLDR           80
     C                   PARM                    QQDCID           12
     C*
     C                   ENDSR
     C*****************************************************************
     C* WRTLOG SUBROUTINE              ** Subroutine **   WRTLOG ******
     C*****************************************************************
     C     WRTLOG        BEGSR
     C*
     C*   ASSIGN PARM FIELDS, THEN CALL SUBMITTER CLP
     C*
     C                   clear                   pd10p
     C*
     C                   MOVE      CMCO#         D1CO#
     C                   MOVE      CMFND         D1FND
     C                   MOVE      CMFYR         D1FYR
     C                   MOVE      CMCASE        D1CASE
     C*
     C                   MOVE      CMSS#         D1SS#
     C                   movel     CMLSN         D1LSN
     C                   movel     CMFSN         D1FSN
     C                   movel     CMMID         D1MID
      *
     C*
     C                   MOVE      'I'           D1TYPE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1EXDT
     C*
     C                   MOVEL     SRDTO         D1IDT
     C                   Z-ADD     DSTIME        D1ITIM
     C                   MOVE      Q1USER        D1IUSR
     C                   MOVE      Q1PGM         D1IPGM
     C*
     C                   MOVEL     SRDTO         D1DT
     C                   Z-ADD     DSTIME        D1TIME
     C                   MOVE      Q1USER        D1USER
     C                   MOVE      Q1PGM         D1PGM
     C*
     C                   WRITE     PD10P
     C*
     C                   ENDSR
     C*
