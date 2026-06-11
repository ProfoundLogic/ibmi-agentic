     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0090R - ACORDIA BILL ENTRY CONTROL MODULE
     H************************************************************** **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program is the initial program in the bill entry   **
     H**     process.                                                **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU PROCESSOR                                          **
     H**   CALLS TO:                                                 **
     H**     CHKSTR  -  Verify States and Related Fields             **
     H**     F0006R  -  User Parm Maint                              **
     H**     F0006R2 -  Change Fund Update WMUPP & SMUPP             **
     H**     F0032R  -  V3 Provider Master Maintenance               **
     H**     F0070R  -  Subfile and Audit/Recon                      **
     H**     F0091R  -  Bill Entry Main Header                       **
     H**     F0092R  -  Bill Entry Main Header                       **
     H**     F0150C  -  EOMB Delete                                  **
     H**     F0534R  -  Mod Search With Provider                     **
     H**     F0541R  -  System Code Table Search                     **
     H**     F0570R  -  Case # Search                                **
     H**     F0571R  -  SS#/Case # Search                            **
     H**     F0573R  -  SS#/ACDT Search                              **
     H**     F0575R  -  Case History (Summary)                       **
     H**     F0578R1 -  Claim Alpha Search                           **
     H**     F0579R  -  Multi Cases/ Same SS# and ACDT Search        **
     H**     F0582R  -  Case/Provider Current Charges Search         **
     H**     SY600R  -  System Wide State Table File                 **
     H**     SC001R  -  Stand Alone Add a Remark                     **
     H**     SC010R  -  Remarks/Custom Data Fields                   **
     H**     SRDCK   -  Check Valid Date                             **
     H**     SRNAME  -  Build Combined Name                          **
     H**   PROGRAMMER: C:43                                          **
     H**   CREATED:    2/06/97      PROJECT: 87MAV                   **
     H*****************************************************************
     H**                                                             **
     H**    IMPORTANT NOTE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  **
     H**                                                             **
     H**    IF A FIELD IS ADDED OR DELETED FROM F0091F OR F0092F     **
     H**    THE FOLLOWING PROGRAMS MUST BE RECOMPILED:               **
     H**                                                             **
     H**    F0090R,  F0091R,   F0092R,  F0070R                       **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  08/17/99 C:62  Project: DR048 19307
     H*                 PASS SC010R W0070R IF WCMS KEY.
     H*
     H*  04/08/99 C:56  Support: MC095 #17568
     H*                 Once User Parms are accesed, load them to
     H*                 screen fields.  Change PARM lists to use
     H*                 SCCO#/SCSUB#/SCFND
     H*
     H*  09/14/98 C:79  Project: AL033
     H*                 Use flags from CHKSTR to determine if Pharmacy
     H*                 or Dental are installed
     H*
     H*  05/08/98 C:31  Project: MC145
     H*                 Yr 2000, RISC development
     H*                 1. Add MMI date to F0070R parm list.
     H*
     H*  03/10/98 C:53  Project: MC145
     H*                 CORRECT DATE PARM ISSUE
     H*
     H*  03/06/98 C:53  Project: MC145
     H*                 SWAP F0031R TO F0032R (NEW VERSION)
     H*
     H*  02/27/98 C:56  Project: MC145
     H*                 Get header information for existing charges
     H*                 only if they are not pended.
     H*
     H*  02/19/98 C:62  Project: MC159
     H*                 ** UPDATE PARM LIST FOR F0534R.
     H*
     H*  02/04/98 C:56  Project: MC087 seq# 9941
     H*             1. Replace the PARM PDZIP with P1ZIP in the calls
     H*                to F0091R, F0092R and F0070R.
     H*
     H*  01/22/98 C:53  PROJECT: MC145
     H*                 REMOVE DARTS CALL ON NEW CASE
     H*
     H*  01/14/98 C:59  PROJECT: MC145
     H*                 THERE IS A DEFAULT OF 430 MOVED INTO GEOZIP
     H*                 THIS IS ONLY VALID FOR OH, CONDITIONING COMMAND
     H*
     H*  11/19/97 C:53  PROJECT: MCMNT
     H*                 PASS TO F0070R NETWORK INFO FROM F0534R
     H*
     H*  11/06/97 C:53  Project: MC145
     H*              1. RMV F11 AND F15 AND INCORPRATE INTO F10
     H*
     H*  10/14/97 C:56  Project: MC087 #9058 support
     H*              1. Add parm to call of F0091R to indicate
     H*                 protection for Type of Bill and Diagnosis
     H*                 Codes.
     H*
     H*  04/10/97 C:31  Project: 87MAW
     H*                 Bill Entry Changes Phase 2
     H*                 1. Replaced Anthem Modifier with BWC ID #
     H*                 2. Added error message MCR2385 (BWC ID # Reqd)
     H*
     H*  04/24/97 C:53  Project: 87MAW
     H*              1. CORRECT F3 NOT EXITING PROPERLY
     H*
     H*  04/24/97 C:56  Project: 87MAW
     H*              1. ADD SCSYS TO CALL OF F0091R
     H*              2. GET VALUE OF 'TYPE OF BILL' FOR CALL TO F0091R
     H*              3. CHANGE VALUE FOR 'TYPE OF BILL' FOR CALL TO
     H*                 F0092R
     H*
     H*  03/01/97 C:56  Project: OH009
     H*                 ADD DEFAULT SECOND GOEGRAPHIC AREA OF 430
     H*                 IF GEOZIP DOES NOT EXIST IN TABLE FILE
     H*
     H***05/14/97 C:56  PROJECT: 87MAW
     H****************  ADD '$1000 MED ONLY PGM' WINDOW.
     H*
     H*  07/22/97 C:56  PROJECT: 87MAW
     H*                 ADD SCDT09 AS PARMS TO THE CALLS OF F0070R
     H*
     H*  08/01/97 C:56  PROJECT: 87MAW
     H*                 REPLACE ALL CASE SEARCHES (F6, F7, F8 AND F9)
     H*                 WITH CALL TO F0269R (F10).
     H*
     H*  08/08/97 C:59  PROJECT: 87MBA
     H*                 CORRECT WRITING OF RECORD LOCKOUT DATE
     H*
     H*  08/22/97 C:32  PROJECT: 87MCJ
     H*                 ADD PREVIOUS EOMB# TO F0091F SCR01
     H*
     H*  08/25/97 C:58  PROJECT: 87MCJ
     H*                 WHEN USER CHOOSES F11 BWC NUMBER NEEDS TO BE
     H*                 CLEARED, ORIGINAL WAS LEFT CAUSING CONFLICT
     H*                 BETWEEN PROVIDER AND BWC NUMBER
XXX  H*****************************************************************
     H*                                                              **
     H*  71  ON IS INTERFACE WITH WORKERS COMP
     H*  91  ON IS ALLOW PROVIDER MASTER MAINT FUNCTION KEY
     H*  99  ON IS SHOW / PROCESS AUDIT RECON
     H*****************************************************************
     H** FILE USAGE :                                                **
     H* XXXXXX  X   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.   **C
     H* F0090F  C   SCR01 : IDENTIFY THE (NEW) BILL TO BE ENTERED.   **C
     H*****************************************************************
     FF0090F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFMF1L     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     FFMFNL     IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FFADCL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     F***FTABL   IF  E           K        DISK
     FFMSML     IF   E           K DISK
     FFMCMLA    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFXLOP     UF A E           K DISK
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*** BEGIN ADD *** 05/14/97 **************************************
     F***WMEIL1  IF  E           K        DISK
     F*** END ADD *** 05/14/97 ****************************************
      /COPY SCOPYPR01D
     D* ?????? *********  CK4     1   5 78               CMD KEYS FOR SCR04
     D* ?????? ********   CK4A    1   6 78               CMD KEYS FOR SCR04
     D HD5             S             13    DIM(3) CTDATA PERRCD(1)              COL HEADINGS DESC
     D KY2             S              5  0 DIM(2)                               SCR1 ROLL INDEXS
     D*** BEGIN DEL *** 08/01/97 **************************************
     D**********          CK1A    1   5 78               CMD KEYS FOR SCR01
     D**********          CK1C    1   5 78               CMD KEYS FOR SCR01
     D**********          CK1E    1   5 78               CMD KEYS FOR SCR01
     D**********          CK1G    1   5 78               CMD KEYS FOR SCR01
     D*** END DEL *** 08/01/97 ****************************************
     D*** BEGIN ADD *** 08/01/97 **************************************
     D*** BEGIN CHG *** 11/06/97 **************************************
     D CK1A            S             78    DIM(3) CTDATA PERRCD(1)              CMD KEYS FOR SCR01
     D CK1C            S             78    DIM(3) CTDATA PERRCD(1)              CMD KEYS FOR SCR01
     D CK1E            S             78    DIM(3) CTDATA PERRCD(1)              CMD KEYS FOR SCR01
     D CK1G            S             78    DIM(3) CTDATA PERRCD(1)              CMD KEYS FOR SCR01
     D*** BEGIN CHG *** 11/06/97 **************************************
     D*** END ADD *** 08/01/97 ****************************************
     D SMSG            S             20    DIM(2) CTDATA PERRCD(1)              TYPES AVAIL
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCSUB#                 4      8  0
     D  QCFND                  9     11  0
     D  QCFYR                 12     14  0
     D  QCCASE                15     21  0
     D                 DS
     D  QWKEY                  1     40
     D  QWCO#                  1      3  0
     D  QWFND                  4      6  0
     D  QWFYR                  7      9  0
     D  QWCASE                10     16  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10) ASCEND                       GROUP VALID SYS
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D                 DS
     D  SCR                    1    100
     D                                     DIM(10)                              SCREEN VALUES
     D  ST7001                 1     10
     D  ST7002                11     20
     D  ST7003                21     30
     D  ST7004                31     40
     D  ST7005                41     50
     D  ST7006                51     60
     D  ST7007                61     70
     D  ST7008                71     80
     D  ST7009                81     90
     D  ST7010                91    100
     D                 DS
     D  ST                     1     20
     D                                     DIM(10)                              ADJ ST FROM SFBPF
     D  FBST01                 1      2
     D  FBST02                 3      4
     D  FBST03                 5      6
     D  FBST04                 7      8
     D  FBST05                 9     10
     D  FBST06                11     12
     D  FBST07                13     14
     D  FBST08                15     16
     D  FBST09                17     18
     D  FBST10                19     20
     D                 DS
     D  TAPE                   1     20
     D                                     DIM(10)                              ST TAPE FROM SFBPF
     D  FBTP01                 1      2
     D  FBTP02                 3      4
     D  FBTP03                 5      6
     D  FBTP04                 7      8
     D  FBTP05                 9     10
     D  FBTP06                11     12
     D  FBTP07                13     14
     D  FBTP08                15     16
     D  FBTP09                17     18
     D  FBTP10                19     20
     D                 DS
     D  BS                     1     30
     D                                     DIM(10)                              BILL SUB-TYPE CODES
     D  STBS01                 1      3
     D  STBS02                 4      6
     D  STBS03                 7      9
     D  STBS04                10     12
     D  STBS05                13     15
     D  STBS06                16     18
     D  STBS07                19     21
     D  STBS08                22     24
     D  STBS09                25     27
     D  STBS10                28     30
     D                 DS
     D  XXSTAT                 1      3
     D  XXTAPE                 1      1
     D  XXPHAR                 2      2
     D  XXDENT                 3      3
     D                 DS
     D  DC10A1                 1     10
     D  XXFIL1                 1      8
     D  XXDCHR                 9     10
     D                 DS
     D  DC10A2                 1     10
     D  XXFIL2                 1      6
     D  XXBTYP                 7      9
     D  XXPCDM                10     10
     D                 DS
     D  DC10A3                 1     10
     D  XXHTOS                 1      2
     D  XXTOB                  3      3
     D  XXFIL3                 4     10
     D                 DS
     D  XXPZIP                 1      9  0
     D  X1PZIP                 1      5  0
     D  X2PZIP                 6      9  0
     D*********** DS
     D***********                             1  120DSDTTM
     D***********                             1   60DSTIME
     D***********                             7  120DSDATE
     D*** BEGIN ADD *** 08/08/97 **************************************
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*** END ADD *** 08/08/97 **************************************
     D SCR91R        E DS                  EXTNAME(F0091F)
     D  @IN10        E                     EXTFLD(*IN10)
     D  @IN12        E                     EXTFLD(*IN12)
     D  @IN20        E                     EXTFLD(*IN20)
     D*PUIMOD*** BEGIN ADD
     D  PUIDUMMY40   E                     EXTFLD(SCCNTY)
     D*PUIMOD*** END ADD

     D SCR92R        E DS                  EXTNAME(F0092F)
     D  @XN10        E                     EXTFLD(*IN10)
     D  @XN12        E                     EXTFLD(*IN12)
     D  @XN20        E                     EXTFLD(*IN20)
     D  H92REC       E                     EXTFLD(HREC)
     D  H92FLD       E                     EXTFLD(HFLD)
     D*PUIMOD*** BEGIN ADD
     D  PUIDUMMY01   E                     EXTFLD(AV_KEYS)
     D  PUIDUMMY02   E                     EXTFLD(BTNDIARY)
     D  PUIDUMMY03   E                     EXTFLD(BTNHELP)
     D  PUIDUMMY04   E                     EXTFLD(BTNIMGQUE)
     D  PUIDUMMY05   E                     EXTFLD(BTNMESSAGE)
     D  PUIDUMMY06   E                     EXTFLD(BTNREFRESH)
     D  PUIDUMMY07   E                     EXTFLD(BTNSAVE)
     D  PUIDUMMY08   E                     EXTFLD(BTNSEARCH)
     D  PUIDUMMY09   E                     EXTFLD(BTNWRKJOB)
     D  PUIDUMMY10   E                     EXTFLD(BTNWRKPRNT)
     D  PUIDUMMY11   E                     EXTFLD(DRYICNURL)
     D  PUIDUMMY12   E                     EXTFLD(MSGICNURL)
     D  PUIDUMMY13   E                     EXTFLD(SCCNME)
     D  PUIDUMMY14   E                     EXTFLD(SCGZIP)
     D  PUIDUMMY15   E                     EXTFLD(SCNAME)
     D  PUIDUMMY16   E                     EXTFLD(SCPNME)
     D  PUIDUMMY17   E                     EXTFLD(SCXXXX)
     D  PUIDUMMY18   E                     EXTFLD(SCACDT)
     D  PUIDUMMY19   E                     EXTFLD(SCAST)
     D  PUIDUMMY20   E                     EXTFLD(SCCASE)
     D  PUIDUMMY22   E                     EXTFLD(SCDIV)
     D  PUIDUMMY23   E                     EXTFLD(SCEMP#)
     D  PUIDUMMY24   E                     EXTFLD(SCENAM)
     D  PUIDUMMY25   E                     EXTFLD(SCFEIN)
     D  PUIDUMMY26   E                     EXTFLD(SCFND)
     D  PUIDUMMY27   E                     EXTFLD(SCFY)
     D  PUIDUMMY28   E                     EXTFLD(SCMMI)
     D  PUIDUMMY29   E                     EXTFLD(SCMOD)
     D  PUIDUMMY30   E                     EXTFLD(SCPRDS)
     D  PUIDUMMY31   E                     EXTFLD(SCSCL#)
     D  PUIDUMMY32   E                     EXTFLD(SCSS#)
     D  PUIDUMMY33   E                     EXTFLD(SCSUB#)
     D  PUIDUMMY34   E                     EXTFLD(SCUSER)
     D  PUIDUMMY35   E                     EXTFLD(SCZIP)
     D  PUIDUMMY36   E                     EXTFLD(SCCNTY)
     D*PUIMOD*** END ADD
     D
     D
     D                 DS
     D  SCTITL                 1     12
     D  SCTTL1                 1      5
     D  SCTTL2                 6      9
     D  SCTTL3                10     12
      ***PY SCOPYPR01I
      /COPY SCOPYPR01C
      *
      *   KEY LIST FOR THE DIAGNOSIS CODE FILE -- FADCL
      *

     C     DCKEY         KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C                   KFLD                    SCCO#
     C*** END CHG *** 04/08/99 ****************************************
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*** BEGN ADD *** 05/14/97 ***************************************
     C****
     C*** KEY LIST FOR ELECTRONIC DATA INTERCHANGE FILE
     C****
     C****       EIKEY     KLIST
     C****                 KFLD           UPCO#
     C****                 KFLD           XXWCF#
     C****                 KFLD           CMFYR
     C****                 KFLD           CMCASE
     C*** END ADD *** 05/14/97 ****************************************
      *
     C     *ENTRY        PLIST
     C     SCPRDS        PARM                    QQPRDS           40
     C*
      *********
      /EJECT
     C*** BEG DEL *** 01/22/98 ******************************
      **************************************************************
      ** DARTS : IF NO CASE IS FOUND FOR SELECTED SS# / ACDT, THEN *
      **         DISPLAY "ADD A REMARK"                          ***
      **************************************************************
     C*********  DARTS     BEGSR
     C*********            MOVEL'F0090R'  QDPNAM    P
     C*********            MOVEL'NOCASE'  QDKEY     P
     C****BEGIN CHG *** 08/08/97 **************************************
     C*********            TIME           DSDTTM
     C*********            MOVE DSDATE    QDDT
     C*********            TIME           DFDTTM
     C*********            MOVE DFDATE    QDDT
     C****BEGIN CHG *** 08/08/97 **************************************
     C*********            CALL 'SC001R'
     C*********            PARM           QQPRDS 40
     C*********            PARM ' '       QQCALL  1
     C*********            PARM           QDKEY  40
     C*********            PARM           QDPNAM 10
     C*********            PARM *BLANKS   QDFOLD 80
     C*********            PARM *BLANKS   QDDCID 12
     C*********            PARM           QDDT    6
     C*********            PARM '??'      QDRMCD  5        A TO DO
     C*********            PARM '???'     QDCOM  60        A TO DO
     C*********            PARM '???'     QDDUSR 10        A TO DO
     C*********            PARM 'Y'       QDRFLD  1
     C*********            PARM 'Y'       QDRDOC  1
     C*********            PARM 'N'       QDRDT   1
     C*********            PARM 'Y'       QDRRMC  1
     C*********            PARM 'Y'       QDRCOM  1
     C*********            PARM 'N'       QDRADD  1
     C*********            PARM 'Y'       QDRDTY  1
     C*********            PARM 'Y'       QDRDUS  1
     C*********            ENDSR
     C*EJECT***
     C*** END DEL *** 01/22/98 ******************************
      **************************************************************
      ** DCINIT CLEARS THE PARMS TO 91R AND 92R AND LOADS        ***
      ** FADCP                                                   ***
      **************************************************************
     C     DCINIT        BEGSR
      * CONDITIONAL CALLS TO 91 AND 92 ???
     C                   CLEAR                   SCR91R
     C                   CLEAR                   SCR92R
     C                   CLEAR                   SCBFEI
     C                   CLEAR                   SCBMOD
     C                   CLEAR                   SCCRDT
     C                   CLEAR                   SCDFC
     C                   CLEAR                   SCDT09
     C                   CLEAR                   SCOFEI
     C                   CLEAR                   SCOMOD
     C                   CLEAR                   SCSFEI
     C                   CLEAR                   SCSMOD
     C                   CLEAR                   SCTFEI
     C                   CLEAR                   SCTMOD
     C                   CLEAR                   QQRTCD
     C                   CLEAR                   SCDT07
     C                   CLEAR                   SCDT08
     C                   CLEAR                   SCDYCV
     C                   CLEAR                   SCNOCV
     C                   CLEAR                   SCDT06
     C                   CLEAR                   SCADHR
     C                   CLEAR                   SCDCHR
     C                   CLEAR                   SCODT1
     C                   CLEAR                   SCOSFD
     C                   CLEAR                   SCOSTD
     C                   CLEAR                   SCVA01
     C                   CLEAR                   SCACRT
     C                   CLEAR                   SCDT18
     C                   CLEAR                   SCDT13
     C                   CLEAR                   SCDT14
     C                   CLEAR                   SCDT15
     C                   CLEAR                   SCDT16
     C                   CLEAR                   SCDT17
     C*** BEGIN ADD *** 10/14/97 **************************************
     C* THIS PARM WITH INDICATE IF NEED TO PROTECT BILL TYPE AND
     C* DIAGNOSIS CODES IN F0091R - WHEN CHARGES ALREADY EXISTS
     C                   MOVE      *BLANKS       QQPRTC
     C*** END ADD *** 10/14/97 ****************************************
      *
      **  CHAIN TO FADCL IF THE RECORD IS FOUND LOAD ALL SCREEN FIELDS
      **  FOR F0091F AND F0092F
      *
     C*** BEGIN DEL *** 02/27/98 **************************************
     C********** DCKEY     CHAINPDC01                60
     C********** *IN60     IFEQ *OFF
     C*** END DEL *** 02/27/98 ****************************************
     C*
     C*** BEGIN ADD *** 02/27/98 **************************************
     C     DCKEY         SETLL     PDC01
     C     DCKEY         READE     PDC01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DCSTAT        IFEQ      ' '
     C*** END ADD *** 02/27/98 ****************************************
      *
      **  FOR F0091F
      *
     C*** BEGIN ADD *** 10/14/97 **************************************
     C                   MOVE      'A'           QQPRTC
     C*** END ADD *** 10/14/97 ****************************************
     C*
     C*** BEGIN ADD *** 04/24/97 **************************************
     C                   MOVEL     XXBTYP        SCBTYP
     C*** END ADD *** 04/24/97 ****************************************
     C*** BEGIN ADD *** 08/22/97 **************************************
     C                   MOVE      DCPEOB        SCPEOB
     C*** END ADD *** 08/22/97 ****************************************
     C                   MOVEL     DCLIC#        SCMLI#
     C                   MOVEL     DCPHTY        SCPTYP
     C                   MOVEL     DCACCT        SCACCT
     C                   MOVEL     DCMRC#        SCMRC#
     C                   MOVEL     DCDIA1        SCDIA1
     C                   MOVEL     DCDIA2        SCDIA2
     C                   MOVEL     DCDIA3        SCDIA3
     C                   MOVEL     DCDIA4        SCDIA4
     C                   MOVEL     DCDIA5        SCDIA5
     C                   MOVEL     DCDIA6        SCDIA6
     C                   MOVEL     DCDIA7        SCDIA7
     C                   MOVEL     DCDIA8        SCDIA8
     C                   MOVEL     DCDIA9        SCDIA9
     C                   Z-ADD     DCTFEI        SCTFEI
     C                   Z-ADD     DCTMOD        SCTMOD
     C                   Z-ADD     DCOFEI        SCOFEI
     C                   Z-ADD     DCOMOD        SCOMOD
     C                   Z-ADD     DCSFEI        SCSFEI
     C                   Z-ADD     DCSMOD        SCSMOD
     C                   Z-ADD     DCBFEI        SCBFEI
     C                   Z-ADD     DCBMOD        SCBMOD
     C                   MOVE      DCIWRI        SCIWRI
     C                   MOVE      DCPMRI        SCPMRI
     C                   MOVE      DCEMPL        SCEMPL
     C                   Z-ADD     DCDFC         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDFC
     C                   Z-ADD     DCDT09        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT09
     C                   Z-ADD     DCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
      *
      **  FOR F0092F
      *
     C*** BEGIN DEL *** 04/24/97 **************************************
     C**********           MOVE XXBTYP    SCBTYP
     C*** END DEL *** 04/24/97 ****************************************
     C*
     C                   Z-ADD     DCDT07        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT07
     C                   Z-ADD     DCDT08        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT08
     C                   Z-ADD     DCDYCV        SCDYCV
     C                   Z-ADD     DCNOCV        SCNOCV
     C                   Z-ADD     DCDT06        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT06
     C                   Z-ADD     DCADHR        SCADHR
     C                   MOVE      DCADTY        SCADTY
     C                   MOVE      DCADSC        SCADSC
     C                   MOVE      XXDCHR        SCDCHR
     C                   MOVEL     DCPATS        SCPATS
     C                   MOVEL     DCOCD1        SCOCD1
     C                   Z-ADD     DCODT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCODT1
     C                   Z-ADD     DCOSFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOSFD
     C                   Z-ADD     DCOSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOSTD
     C                   MOVE      XXHTOS        SCHTOS
     C                   MOVE      XXTOB         SCTOB
     C                   MOVEL     DCVC01        SCVC01
     C                   Z-ADD     DCVA01        SCVA01
     C                   MOVE      DC10A4        SCACRT
     C                   MOVEL     DCADDI        SCADDI
     C                   MOVE      XXPCDM        SCPCDM
     C                   MOVEL     DC7A01        SC7A01
     C                   Z-ADD     DCDT13        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT13
     C                   MOVEL     DC7A02        SC7A02
     C                   Z-ADD     DCDT14        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT14
     C                   MOVEL     DC7A03        SC7A03
     C                   Z-ADD     DCDT15        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT15
     C                   MOVEL     DC7A04        SC7A04
     C                   Z-ADD     DCDT16        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT16
     C                   MOVEL     DC7A05        SC7A05
     C                   Z-ADD     DCDT17        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT17
     C                   MOVEL     DC7A06        SC7A06
     C                   Z-ADD     DCDT18        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT18
     C*** BEGIN DEL *** 02/27/98 **************************************
     C**********           ELSE
     C**********           MOVE 'Y'       SCEMPL
     C**********           ENDIF
     C*** END DEL *** 02/27/98 ****************************************
     C*
     C*** BEGIN ADD *** 02/27/98 **************************************
     C                   LEAVE
     C                   ENDIF
     C     DCKEY         READE     PDC01                                  60
     C                   ENDDO
     C   60              MOVE      'Y'           SCEMPL
     C*** END ADD *** 02/27/98 ****************************************
      *
     C                   ENDSR
      /EJECT
      **************************************************************
      **************************************************************
     C     OTHFKS        BEGSR
     C                   ENDSR
      /EJECT
      **************************************************************
      ** CALL THE API TO LOAD CO, SUB, GRP  DESCRIPTIONS         ***
      ** THIS IS A TO DO.                                        ***
      **************************************************************
     C     DSCAPI        BEGSR
     C                   ENDSR
      /EJECT
      **************************************************************
      ** ALL SCREEN FILE FIELD EDITS WILL BE CONTROLLED HERE     ***
      ** IF ANY FAILED EDITS, THEN *IN90 MUST BE SET ON          ***
      **************************************************************
     C     ERRCK1        BEGSR
      * XXFND CAN CHANGE AT WCMS SELECTION LISTS
      * XXFND <- UPFND AT THE START OF EVERY NEW EDIT
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           Z-ADDUPFND     XXFND
     C                   Z-ADD     SCFND         XXFND
     C*** END CHG *** 04/08/99 ****************************************
     C     *IN12         IFEQ      *ON
     C                   EXSR      FUNKEY
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   EXSR      EDITS
     C                   ENDIF
     C                   ENDSR
      /EJECT
      **************************************************************
      ** EDITS : VERIFY ALL SCREEN FIELDS                         **
      **************************************************************
     C     EDITS         BEGSR
     C*** BEGIN SCSS# AND SCACDT : CAN'T HAVE ONE WITHOUT THE OTHER
     C     SCSS#         IFEQ      *BLANK
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C     SCACDT        IFEQ      *ZERO
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN24
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN60
     C                   EXSR      ZCMLA
     C     *IN27         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C***************      EXSR DARTS                      DEL 01/22/98
     C                   GOTO      ENDEDT
     C                   ENDIF
     C* ZCML1 ??? WITHIN ZCMLA
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END                                                    END EQ *
      *
     C*** BEG CHG *** 04/08/99 ****************************************
     C********** CMCO#     IFNE UPCO#
     C********** CMSUB#    ORNE UPSUB#
     C********** CMFND     ORNE UPFND
     C     CMCO#         IFNE      SCCO#
     C     CMSUB#        ORNE      SCSUB#
     C     CMFND         ORNE      SCFND
     C*** END CHG *** 04/08/99 ****************************************
     C                   MOVE      *ON           *IN22
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      *
      * WE BUY THE SS# AND THE ACDT:
      *
     C*
     C* LOCKOUT RECORD
     C*
     C                   MOVE      SCSS#         LOSS#
     C                   MOVE      XXACDT        LOACDT
     C                   MOVE      CMCO#         LOCO#
     C                   MOVE      CMSUB#        LOSUB#
     C                   MOVE      CMFND         LOFND
     C                   MOVE      CMFYR         LOFYR
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        LOCASE
     C                   MOVE      CMCASE        @CCASE
     C*
     C     LOKEY         CHAIN(N)  PLO01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN29
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
     C*** END FIRST SCREEN FIELD SCSS#
      *
     C*** BEGIN SCREEN FIELD FEIN/MOD
     C*
     C     SCFEIN        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
     C* * * PAYEE(PROVIDER) MUST BE ON FILE  AND ACTIVE
     C*
B1   C*******    SCFEIN    IFNE XCFIH
     C********   SCMOD     ORNE XCMDH
     C*** BEGIN CHG *** 05/14/97 **************************************
     C********** PDKEY     CHAINPPD01               N74
     C     PDKEY         CHAIN     PPD01                              74
     C*** END CHG *** 05/14/97 ****************************************
     C*
B2   C     *IN74         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
X2   C                   ELSE
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   EXSR      F0534R
B3   C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *IN90                          ADD 11/19/97
     C                   MOVE      *ON           *INKC
     C                   GOTO      ENDEDT
E3   C                   ENDIF
      *
B3   C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
E3   C                   ENDIF
      *
B3   C     QQFEIN        IFNE      SCFEIN
     C     QQMOD         ORNE      SCMOD
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C*** BEGIN CHG *** 05/14/97 **************************************
     C********** PDKEY     CHAINPPD01               N74
     C     PDKEY         CHAIN     PPD01                              74
     C*** END CHG *** 05/14/97 ****************************************
E3   C                   ENDIF
B3   C     PDSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN74
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
E3   C                   END
B3   C     PDTYPE        IFNE      '1'
     C     PDTYPE        ANDNE     '4'
     C                   MOVE      '1'           *IN48
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
E3   C                   END                                                    END PDTYPE NE 1 OR 4
     C                   MOVE      SCFEIN        XCFIH             9 0
     C                   MOVE      SCMOD         XCMDH             3 0
E2   C                   END
E1   C*****************    ENDIF                           SCFEIN:SCFIH
     C*
     C*  A VALID PROVIDER/MODIFIER HAS BEEN SELECTED
     C*
     C*** BEGIN DEL ** 04/10/97 ************************************************
     C********             MOVE PDAMOD    SCAMOD
     C*** END DEL ** 04/10/97 **************************************************
     C*** BEGIN ADD ** 04/10/97 ************************************************
     C*
     C*          Assume BWC ID number if blank
     C*          ------------------------------------------------------
     C*** BEG DEL ** 11/06/97 **************************************************
     C****       SCBWC#    IFEQ *BLANKS
     C****                 MOVE PDBWC#    SCBWC#
     C****                 ENDIF                           BWC# = blank
     C****
     C****       Edit BWC ID number
     C****       ------------------------------------------------------
     C****       PDBWC#    IFNE SCBWC#
     C****       SCBWC#    OREQ *BLANKS
     C****                 SETON                     728290
     C****                 GOTO ENDEDT
     C****                 ENDIF                           BWC# <> BWC#
     C*** BEG DEL ** 11/06/97 **************************************************
     C*** END ADD ** 04/10/97 **************************************************
     C*
     C     'PHN'         CHECK     SCSYS                                  26
     C     *IN26         IFEQ      *ON
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF
      * THIS NEXT TEST CONTRADICTS THE P,H,M,N,S,U,V DOC
     C*********  SCSYS     IFNE 'P'
     C*********  SCSYS     ANDNE'H'
     C*********  SCSYS     ANDNE'N'
     C***********          MOVE '1'       *IN26
     C**************       MOVE *ON       *IN90
     C***************      GOTO ENDEDT
     C****************     END
      *
     C                   MOVE(P)   SCSYS         QQKEY
     C                   CALL      'F0541R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY            10
     C                   PARM      'V'           QQRTCD
      *
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN38
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
      *
      * CHANGED TO CHECK IF VALID SYSTEM ID FOR ALL SYSTEMS NOT ONLY
      * NETWORK
      *
     C                   Z-ADD     1             X                 5 0
     C     SCSYS         LOOKUP    FSY(X)                                 60
     C     *IN60         IFEQ      '0'
     C     SCSYS         IFEQ      'N'
     C                   MOVE      '1'           *IN28
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      '1'           *IN38
     C                   MOVE      *ON           *IN90
     C                   END
     C                   GOTO      ENDEDT
     C                   END
      *
     C     SCSYS         IFEQ      'N'
     C     PDPARN        IFNE      'P'
     C                   MOVE      '1'           *IN28
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
     C                   END
      *
      * SCSYS IS NOW VALIDATED
      *
      * SCPRTY=BILL TYPE
      *
     C     SCPRTY        IFEQ      'X'                                          PHARMACY?
     C     FBFL10        IFNE      'Y'                                          INSTALLED?
     C                   MOVE      '1'           *IN15                          MCR0704
     C                   MOVE      *ON           *IN90                          BILL TYPE
     C                   GOTO      ENDEDT
     C                   END
     C                   END
      *
     C     'NXDH'        CHECK     SCPRTY                                 32
     C     *IN32         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
      *
     C     SCTYPE        IFNE      *BLANKS
      *
     C     SCPRTY        IFNE      'H'
     C                   MOVE      '1'           *IN45
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END                                                    END SCPRTY NE 'H'
      *
     C     SCTYPE        IFNE      'IN '
     C     SCTYPE        ANDNE     'ER1'
     C     SCTYPE        ANDNE     'ER+'
     C     SCTYPE        ANDNE     'OUT'
     C     SCTYPE        ANDNE     'ASC'
     C     SCTYPE        ANDNE     'CAT'
     C     SCTYPE        ANDNE     'MRI'
     C                   MOVE      '1'           *IN46
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END                                                    END SCTYPE NE 'IN'..
     C*
     C                   END                                                    END SCTYPE NE *BLANK
     C*
     C* PROVIDER TYPE MUST MATCH SYSTEM TYPE
     C*
     C     SCSYS         IFEQ      'P'
     C     SCPRTY        IFNE      'N'
     C     SCPRTY        ANDNE     'X'
     C     SCPRTY        ANDNE     'D'                                          ADD 08/11/93
     C                   MOVE      '1'           *IN10
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
     C                   ELSE
     C     SCSYS         IFEQ      'H'
     C     SCPRTY        ANDNE     'H'
     C                   MOVE      *ON           *IN10
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END
     C                   END
      *
      * SCPRTY IS NOW VALIDATED
      *
      *
      * VALIDATE ADJUDICATION STATE
      *
B1   C     SCAST         IFEQ      *BLANKS
     C                   MOVEL(P)  CMAST         XXSTKY
     C     STKY          CHAIN     PST01                              60
B2   C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN39
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
X2   C                   ELSE
B3   C     STINST        IFEQ      'Y'
     C                   MOVEL     CMAST         SCAST
E3   C                   ENDIF                                                  STINST=Y
E2   C                   ENDIF                                                  PST01=60=1
E1   C                   ENDIF                                                  SCAST=BLANK
     C*** GET ADJUDICATION ST AND LICENSE # FOR NEW PROVIDER
B1   C     SCAST         IFEQ      *BLANKS
     C     PDKEY         CHAIN     PP101                              60
B2   C     *IN60         IFEQ      '0'
     C*
     C** CHECK THAT STATE IS A VALID ADJUDICATION STATE IN FS BASE CONTROL FILE
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C                   MOVEL     P1ST          QQSTT
     C                   CALL      'CHKSTR'                             60
     C                   PARM                    QQSTT
     C                   PARM                    QQSTRT
     C                   PARM                    QQFLRT
B3   C     *IN60         IFEQ      '0'
B4   C     QQSTRT        IFEQ      '0'
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDEDT
X4   C                   ELSE                                                   QQSTRT<>0
     C                   MOVEL     QQFLRT        XXSTAT
      *** BEG ADD *** 9/14/98 *****************************************
      * Set flags for Pharmacy and Dental
     C                   MOVE      XXPHAR        XXDFLG            1
     C                   MOVE      XXDENT        XXPFLG            1
      *** END ADD *** 9/14/98 *****************************************
B5   C     XXTAPE        IFEQ      'Y'
     C                   MOVE      '1'           *IN81
X5   C                   ELSE                                                   XXTAPE<>Y
     C                   MOVE      '0'           *IN81
E5   C                   ENDIF                                                  XXTAPE=Y    KUP ST
E4   C                   ENDIF                                                  QQSTRT=0    KUP ST
X3   C                   ELSE                                                   CHKSTR=60=1
     C                   Z-ADD     1             X
     C     P1ST          LOOKUP    ST(X)                                  60
B4   C     *IN60         IFEQ      '1'
     C** CHECK FOR TAPE SUBMISSION IN FS BASE CONTROL FILE
     C                   Z-ADD     1             X
     C     P1ST          LOOKUP    TAPE(X)                                60
B5   C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN81
X5   C                   ELSE                                                   ELSE *IN60 LOKUP TP
     C                   MOVE      '0'           *IN81
E5   C                   END                                                    END *IN60 LOKUP TP
X4   C                   ELSE                                                   ELSE *IN60 LKUP ST
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDEDT
E4   C                   END                                                    *IN60 LOKUP     ST
E3   C                   ENDIF                                                  CHKSTR=60
E2   C                   ENDIF                                                  PP101=60=0
E1   C                   ENDIF                                                  SCAST=BLANK
     C*
     C* IF ADJUDICATION STATE IS NOT EQUAL TO BLANKS
     C*    A VALID STATE CODE
      * NOT EQUAL TO BLANKS: USER WISHES TO OVERRIDE
B1   C     SCAST         IFNE      *BLANKS
     C                   MOVEL(P)  SCAST         XXSTKY
     C     STKY          CHAIN     PST01                              60
B2   C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN39
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
E2   C                   END
E1   C                   END
     C*
     C* IF THIS IS A VALID STATE CODE IT MUST ALSO BE A VLAID INSTALLEDCH
     C* ADJUDICATION STATE
     C*
     C*
     C     SCAST         IFNE      *BLANKS
     C                   MOVEL     SCAST         QQSTT
     C                   CALL      'CHKSTR'                             60
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
B1   C     *IN60         IFEQ      '0'
B2   C     QQSTRT        IFEQ      '0'
     C                   MOVE      '1'           *IN40
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
E2   C                   ENDIF                                                  END LOKUP ST
     C                   MOVEL     QQFLRT        XXSTAT
      *** BEG ADD *** 9/14/98 *****************************************
      * Set flags for Pharmacy and Dental
     C                   MOVE      XXPHAR        XXDFLG            1
     C                   MOVE      XXDENT        XXPFLG            1
      *** END ADD *** 9/14/98 *****************************************
B2   C     XXTAPE        IFEQ      'Y'
     C                   MOVE      '1'           *IN81                          81 DEAD?????
     C                   ELSE
     C                   MOVE      '0'           *IN81
E2   C                   ENDIF                                                  END LOKUP ST
     C     SCPRTY        IFEQ      'X'
     C     XXPHAR        IFNE      'Y'
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  END LOKUP ST
     C                   ENDIF                                                  SCPRTY=X
     C     SCPRTY        IFEQ      'D'
     C     QQSTRT        IFEQ      '1'
     C     XXDENT        IFNE      'Y'
     C                   WRITE     SCR01
     C                   MOVE      '1'           *IN44
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  END LOKUP ST
     C                   ENDIF                                                  END QQSTRT
     C                   ENDIF
      * ELSE CALL TO CHKSTR FAILED
     C                   ELSE
     C                   Z-ADD     1             X
     C     SCAST         LOOKUP    ST(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN40
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   ELSE
     C** CHECK FOR TAPE SUBMISSION IN FS BASE CONTROL FILE
     C                   Z-ADD     1             X
     C     SCAST         LOOKUP    TAPE(X)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN81
     C                   ELSE                                                   ELSE *IN60 LOKUP TP
     C                   MOVE      '0'           *IN81
     C                   END                                                    END *IN60 LOKUP TP
     C     1             SUBST     FBAL02:X      XXPFLG            1    65
     C     XXPFLG        IFNE      'Y'
     C                   MOVE      '1'           *IN41
     C                   GOTO      ENDEDT
     C                   END
     C     1             SUBST     FBAL03:X      XXDFLG            1    65
     C     XXDFLG        IFNE      'Y'
     C                   MOVE      '1'           *IN44
     C                   GOTO      ENDEDT
     C                   ENDIF                                                  *IN60 ADD 12/02/96
     C                   END                                                    END *IN60 LOKUP ST
     C*
     C                   END                                                    CHKSTR=60=0
     C                   ENDIF                                                  ANOTHER AST
      **
     C*
     C* IF AUDIT/RECON ENTERED, IT MUST BE VALID
     C*
     C     SCAR          IFNE      'A'
     C     SCAR          ANDNE     'R'
     C     SCAR          ANDNE     *BLANKS
     C                   MOVE      '1'           *IN52
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END                                                    END SCAR NE 'A'
     C*
     C     *INKX         IFNE      '1'
     C*
     C* IF AUDIT ENTERED, AND ENTER IS PRESSED EOMB MUST BE ZEROS
     C*
     C     SCAR          IFEQ      'A'
     C     SCEOB#        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN53
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END                                                    END SCAR NE 'A'
     C*
     C* IF RECON ENTERED, AND ENTER IS PRESSED GIVE ERROR
     C*
     C     SCAR          IFEQ      'R'
     C                   MOVE      '1'           *IN55
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEDT
     C                   END                                                    END SCAR NE 'A'
     C                   END                                                    END *INKX EQ '1'
      *
     C*  END OF BASE ROUTINE
     C*** EXSR CAPSR   IF NETWORK *****
     C*********  FBFL06    IFEQ 'Y'
     C** KEEP IN 70R ***   EXSR CAPSR
     C*******************  ENDIF
     C*
     C     ENDEDT        ENDSR
     C*
      *
      /EJECT
      **************************************************************
      ** F0070R : DETAIL LINE ITEMS                              ***
      **************************************************************
     C     F0070R        BEGSR
     C                   ADD       1             QQENTR            4 0
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8ACDT            8 0
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   MOVE      SCDT07        XXDT07            6
     C                   MOVE      SCDT08        XXDT08            6
     C                   MOVE      SCDT09        XXDT09            6
     C*** END ADD *** 07/22/97 ****************************************
     C*** BEGIN ADD *** 05/08/98 **************************************
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMMI
     C*** END ADD *** 05/08/98 ****************************************
     C                   CALL      'F0070R'                                     SCPRTY=H
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    CMCO#
     C                   PARM                    CMSUB#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMSS#
     C                   PARM                    X8ACDT
     C                   PARM                    CMCASE
     C                   PARM                    CDDTOB
     C                   PARM                    CDSEX
     C                   PARM                    @CCNAM
     C                   PARM                    PDFEIN
     C                   PARM                    PDMOD
     C*** BEGIN CHG *** 02/04/98 **************************************
     C**********           PARM           PDZIP
     C                   PARM                    P1ZIP
     C*** END CHG *** 02/04/98 ****************************************
     C                   PARM                    P1CNTY
     C*** BEGIN CHG ** 04/10/97 ************************************************
     C********             PARM           SCAMOD
     C****                 PARM           SCBWC#           DEL 11/06/97
     C*** END CHG ** 04/10/97 **************************************************
     C                   PARM                    SCSYS
     C                   PARM                    SCPRTY
     C                   PARM                    SCTYPE
     C                   PARM                    SCAST
     C                   PARM                    SCZIPA
     C                   PARM                    XXDFLG
     C                   PARM                    XXPFLG
     C                   PARM                    SCTITL
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   PARM                    XXDT07
     C                   PARM                    XXDT08
     C                   PARM                    XXDT09
     C*** END ADD *** 07/22/97 ****************************************
     C                   PARM                    QQRT70            1
     C                   PARM                    QQENTR
     C                   PARM                    QQNWCD                         ADD 11/19/97
     C                   PARM                    SCR91R
     C                   PARM                    SCR92R
     C*** BEGIN ADD *** 05/08/98 **************************************
     C                   PARM                    XXMMI
     C*** END ADD *** 05/08/98 ****************************************
     C                   ENDSR
      /EJECT
      **************************************************************
      ** F0091R : CAPTURE BASIC HEADER INFO                      ***
      **************************************************************
     C     F0091R        BEGSR
     C                   CALL      'F0091R'
     C                   PARM                    SCAST
     C                   PARM                    SCZIPA
     C                   PARM                    CMCO#
     C                   PARM                    CMSUB#
     C                   PARM                    SCSBNM
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    CMPREX
     C                   PARM                    CMPREC
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    CMSS#
     C                   PARM                    SCACDT
     C                   PARM                    XXMMI
     C                   PARM                    @CCNAM
     C                   PARM                    CMSCL#
     C                   PARM                    PDFEIN
     C                   PARM                    PDMOD
     C                   PARM                    PDNAME
     C*** BEGIN CHG *** 02/04/98 **************************************
     C**********           PARM           PDZIP
     C                   PARM                    P1ZIP
     C*** END CHG *** 02/04/98 ****************************************
     C                   PARM                    PDWCCE
     C*** FORIEGN KEY FOR FILE FTCCP IS NOT KEPT IN FMP1P, JUST
     C*** THE COUNTY CODE WITHOUT THE HIGHORDER KEY, WILL ASSUME
     C*** THAT THE ADJ ST IS THE HIGH ORDER KEY
     C*** FORIEGN KEY ??   PARM           P1PST
     C                   PARM                    P1CNTY
     C*** BEGIN ADD *** 04/24/97 **************************************
     C                   PARM                    SCSYS
     C*** END ADD *** 04/24/97 ****************************************
     C                   PARM                    SCR91R                         RETURNED
     C                   PARM                    QQPRDS
     C                   PARM                    QQRT91            1
     C*** BEGIN ADD *** 10/14/97 **************************************
     C                   PARM                    QQPRTC            1
     C*** END ADD *** 10/14/97 ****************************************
     C                   ENDSR
     C*
      /EJECT
      **************************************************************
      ** F0092R : CAPTURE BASIC HEADER INFO                      ***
      **************************************************************
     C     F0092R        BEGSR
     C*
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8ACDT
     C                   Z-ADD     CMMMI         SRDTI                          MJF
     C                   CALL      'SRCVTDAT'    DTMDYY                          *
     C                   MOVEL     SRDTO         XXMMI                           *
     C*** END ADD *** 07/22/97 ****************************************
     C                   CALL      'F0092R'
     C                   PARM                    SCAST
     C                   PARM                    SCZIPA
     C                   PARM                    CMCO#
     C                   PARM                    CMSUB#
     C                   PARM                    SCSBNM
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    CMPREX
     C                   PARM                    CMPREC
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    CMSS#
     C*** BEGIN CHG *** 07/22/97 **************************************
     C**********           PARM           CMACDT
     C                   PARM                    X8ACDT
     C*** END CHG *** 07/22/97 ****************************************
     C                   PARM                    XXMMI
     C                   PARM                    @CCNAM
     C                   PARM                    CMSCL#
     C                   PARM                    PDFEIN
     C                   PARM                    PDMOD
     C                   PARM                    PDNAME
     C*** BEGIN CHG *** 02/04/98 **************************************
     C**********           PARM           PDZIP
     C                   PARM                    P1ZIP
     C*** END CHG *** 02/04/98 ****************************************
     C                   PARM                    PDWCCE
     C*****************    PARM           P1PST
     C                   PARM                    P1CNTY
     C                   PARM                    QQPRDS
     C                   PARM                    SCR92R                         RETURNED
     C                   PARM                    QQRT92            1
     C                   ENDSR
     C*
      *
      /EJECT
      **************************************************************
      ** ALL FUNCTION KEY PROCESSING WILL BE CODED HERE          ***
      ** INVOKED WITH *IN33, VLDCMDKEY                           ***
      **************************************************************
     C     FUNKEY        BEGSR
     C*** BEGIN DEL *** 08/01/97 **************************************
     C**********
     C***********************************
     C*   F6                            *
     C***********************************
     C********** *INKF     IFEQ '1'
     C**********           CALL 'F0573R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL  1
     C**********           PARM           SCSS#
     C**********           PARM           SCACDT
     C**********           PARM ' '       QQRTCD  1
      * USER F3 FROM THE LIST
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           ENDIF
      * USER F12 FROM THE LIST
     C********** QQRTCD    IFEQ 'B'
     C********** SCSS#     OREQ *BLANK
     C**********           END                             ENDEQ 'B'
      * USER SELECTED FROM THE LIST
     C********** QQRTCD    IFEQ '*'
     C**********           SETON                     47
     C**********           END                             ENDEQ 'B'
     C**********
     C**********           ENDIF                           *INKF
     C**********
     C************************************
     C*   F7                             *
     C************************************
B1   C********** *INKG     IFEQ '1'
     C**********           Z-ADD*ZEROS    X1FND
     C**********           CALL 'F0570R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXYR    20
     C**********           PARM           XXCASE  70
     C**********           PARM           XXSS#   9
     C**********           PARM ' '       QQRTCD
     C**********           PARM           X1FND   30
      * USER F3 FROM THE LIST
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           ENDIF
      * USER F12 FROM THE LIST
 B2  C********** QQRTCD    IFEQ 'B'
     C********** XXYR      OREQ *ZERO
     C**********           CLEARSCSS#
     C**********           CLEARSCACDT
 E2  C**********           END
      * USER SELECTED
 B2  C********** QQRTCD    IFEQ '*'
     C**********           Z-ADDX1FND     XXFND
     C********** @IN71     IFEQ *ON
     C**********           EXSR WCFND#
     C**********           ENDIF
     C**********           Z-ADDXXYR      XXFYR
     C**********           EXSR ZCML1                      XXFND,XXFYR,
     C********** *IN60     IFEQ *OFF
     C**********           MOVE '1'       *IN47            POS CURSOR PROV
     C**********           ELSE
     C**********           MOVE '1'       *IN22
     C**********           ENDIF
 E2  C**********           END                             END
 E1  C**********           ENDIF                           END F7
     C***********************************
     C*   F8                            *
     C***********************************
B1   C********** *INKH     IFEQ '1'
     C**********           Z-ADD*ZEROS    X1FND
     C**********           CALL 'F0571R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSS#
     C**********           PARM           XXYR
     C**********           PARM           XXCASE
     C**********           PARM           X1FND
     C**********           PARM ' '       QQRTCD
      * USER F3 FROM LIST
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           ENDIF
      **********
     C********** QQRTCD    IFEQ 'B'
     C**********           END                             ENDEQ 'B'
      **********
     C********** QQRTCD    IFEQ '*'
     C**********           Z-ADDX1FND     XXFND
     C********** @IN71     IFEQ *ON
     C**********           EXSR WCFND#
     C**********           ENDIF
     C**********           MOVE '1'       *IN47            POS CURSOR PROV
     C*****************    EXSR ZCML1
     C********   *IN60     IFEQ '0'
     C****************     ELSE
     C******************   MOVE '1'       *IN22            POS CURSOR PROV
     C******************   END                             END 0 *IN60
     C**********           END                             ENDEQ '*'
     C**********           END                             ENDEQ F8
     C***************************************************
     C*   F9  ALPHA SEARCH AND CHANGE USER PARMS.
     C***************************************************
B1   C********** *INKI     IFEQ '1'
     C**********           CALL 'F0578R1'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSS#
     C**********           PARM           SCACDT
     C**********           PARM           XXSUB#  50
     C**********           PARM           XXFND
     C**********           PARM           XXFYR
     C**********           PARM           XXCASE
     C**********           PARM ' '       QQRTCD
     C**********
B2   C********** QQRTCD    IFEQ '*'
     C********** @IN71     IFEQ *ON
     C**********           EXSR WCFND#
     C**********           ENDIF
     C********** XXSUB#    IFGT 0
     C********** XXFND     ANDGT0
     C**********           MOVE UPCO#     QQCO#   3
     C**********           MOVE XXSUB#    Q1SUB#  5        ADD 06/29/94
     C**********           MOVE XXFND     Q1FND   3
     C**********           CALL 'F0006R2'                  CHG 06/29/94
     C**********           PARM           QQCO#
     C**********           PARM           Q1SUB#           ADD 06/29/94
     C**********           PARM           Q1FND
     C**********           PARM ' '       QQRTCD
     C**********           MOVE '1'       *IN47            POS CURSOR PROV
     C**********           EXSR PGMSTR
     C**********           ELSE
     C**********           MOVE '1'       *IN22
     C**********           ENDIF
E2   C**********           END
      **********
B2   C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
E2   C**********           ENDIF
      **********
B2   C********** QQRTCD    IFEQ 'B'
     C********** SCSS#     OREQ *BLANK
E2   C**********           END                             ENDEQ 'B'
     C**********
E1   C**********           END                             ENDEQ KH
     C*** END DEL *** 08/01/97 ****************************************
     C*
     C*** BEGIN ADD *** 08/01/97 **************************************
     C*
     C*****************************
     C*  F10 = SEARCH             *
     C*****************************
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCSS#'
     C     HFLD          OREQ      'SCACDT'
     C                   MOVE      '2'           QQCALL            1
     C                   Z-ADD     0             QQSUB#            5 0
     C                   Z-ADD     0             QQFND             3 0
     C                   MOVE      *BLANKS       QQYR              3 0
     C                   MOVE      *BLANKS       QQCAS             7 0
     C                   MOVE      *BLANKS       SCSS#
     C                   Z-ADD     0             SCACDT
     C                   MOVE      *BLANKS       QQRTCD            1
     C*
     C                   CALL      'F0269R'
     C                   PARM                    QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQYR
     C                   PARM                    QQCAS
     C                   PARM                    SCSS#
     C                   PARM                    SCACDT
     C                   PARM                    QQRTCD
      * USER F3 FROM THE LIST
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      * USER F12 FROM THE LIST
     C     QQRTCD        IFEQ      'B'
     C     SCSS#         OREQ      *BLANK
     C                   END                                                    ENDEQ 'B'
      * USER SELECTED FROM THE LIST
     C     QQRTCD        IFEQ      '*'
     C     @IN71         IFEQ      *ON
     C                   EXSR      WCFND#
     C                   ENDIF
     C     QQSUB#        IFGT      0
     C     QQFND         ANDGT     0
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           MOVE UPCO#     QQCO#   3
     C                   MOVE      SCCO#         QQCO#             3
     C*** END CHG *** 04/08/99 ****************************************
     C                   MOVE      QQSUB#        Q1SUB#            5
     C                   MOVE      QQFND         Q1FND             3
     C                   CALL      'F0006R2'
     C                   PARM                    QQCO#
     C                   PARM                    Q1SUB#
     C                   PARM                    Q1FND
     C                   PARM      ' '           QQRTCD
     C                   MOVE      '1'           *IN47                          POS CURSOR PROV
     C                   EXSR      PGMSTR
     C                   ELSE
     C                   MOVE      '1'           *IN22
     C                   ENDIF
E2   C                   END
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** BEG ADD *** 11/06/97 ****************************************
     C*****************************
     C*   PROVIDER SEARCH         *
     C*****************************
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   MOVE      SCFEIN        ZZFEIN            9
     C                   MOVE      SCMOD         ZZMOD             3
     C                   MOVE      '2'           QQCALL
     C******************   CALL 'F0031R'                   DEL 03/06/98
     C                   CALL      'F0032R'                                     ADD 03/06/98
     C                   PARM                    QQCALL
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM      ' '           QQRTCD
     C                   PARM      ' '           QQTYPE
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN47                          POS CURSOR PROV
     C                   END                                                    ENDEQ 'B'
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN33                          POS CURSOR PRTY
     C                   MOVEL     ZZFEIN        SCFEIN
     C                   MOVEL     ZZMOD         SCMOD
     C                   END                                                    ENDEQ '*'
     C                   ENDIF                                                  END SCFEIN  1'
     C*****************************
     C*  STATE SEARCH             *
     C*****************************
     C     HFLD          IFEQ      'SCAST'
     C                   CALL      'SY600R'
     C                   PARM                    SCAST
     C                   PARM      ' '           QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     QQRTCD        IFEQ      'B'
     C                   Z-ADD     19            SCLINE
     C                   Z-ADD     41            SCPOS
     C                   END
     C     QQRTCD        IFEQ      '*'
     C     XXAR          IFEQ      'Y'
     C                   Z-ADD     21            SCLINE
     C                   Z-ADD     41            SCPOS
     C                   ELSE
     C                   Z-ADD     19            SCLINE
     C                   Z-ADD     41            SCPOS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  SCAST
     C*** END ADD *** 11/06/97 ****************************************
     C                   ENDIF                                                  *INKJ IFEQ
     C*** END ADD *** 08/01/97 ****************************************
     C*** BEG DEL *** 11/06/97 ****************************************
     C***********************************
     C****F11 = PROVIDER SEARCH         *
     C***********************************
     C****       *INKK     IFEQ '1'
     C****                 MOVE SCFEIN    ZZFEIN  9
     C****                 MOVE SCMOD     ZZMOD   3
     C****                 MOVE '2'       QQCALL
     C****                 CALL 'F0031R'
     C****                 PARM           QQCALL
     C****                 PARM           ZZFEIN
     C****                 PARM           ZZMOD
     C****                 PARM ' '       QQRTCD
     C****       QQRTCD    IFEQ 'A'
     C****                 MOVE *ON       *INKC
     C****                 ENDIF
     C****       QQRTCD    IFEQ 'B'
     C****                 MOVE '1'       *IN47            POS CURSOR PROV
     C****                 END                             ENDEQ 'B'
     C****       QQRTCD    IFEQ '*'
     C****                 MOVE '1'       *IN33            POS CURSOR PRTY
     C****                 MOVELZZFEIN    SCFEIN
     C****                 MOVELZZMOD     SCMOD
     C****BEGIN ADD *** 08/25/97 **************************************
     C****WHEN PROVIDER SEARCH IS COMPLETED, BWC NUMBER NEEDS TO BE
     C****CLEARED, INCASE USER CHOOSES A DIFFERENT PROVIDER THAN
     C****ORIGINALLY WORKING WITH.  ORIGINALLY IF USER CHOSE A DIFF
     C****FEIN NUMBER AFTER A SEARCH, THE ORIGINAL BWC NUMBER WAS STILL
     C****IN THE FIELD, CAUSED INVALID MATCH TO FEIN.
      ****
     C****                 MOVEL*BLANKS   SCBWC#
     C****BEGIN ADD *** 08/25/97 **************************************
     C****                 END                             ENDEQ '*'
     C****                 END                             END F11     1'
     C*** BEG DEL *** 11/06/97 ****************************************
     C***********************************
     C*     F13 = CHG USER PARAMETERS   *
     C***********************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
      * F0006R DOESN'T HAVE RETURN CODES SO WE KNOW NOT
     C                   EXSR      PGMSTR
      * POSITION CURSOR BACK AT SS# , FOR LACK OF A BETTER PLACE
     C                   MOVE      *ON           *IN13
     C                   ENDIF                                                  F13
     C***********************************
     C*  F14 - STATE SEARCH             *
     C***********************************
B1   C     *INKN         IFEQ      '1'
      * HONOR F14 ONLY IF VALID CLAIM
B2   C     SCSS#         IFGT      *BLANKS
     C     SCACDT        ANDGT     *ZEROS
     C                   EXSR      ZCMLA
      * CLAIM WAS FOUND TO EXIST WITHIN USER PARM VALUE
B3   C     CNTCAS        IFGT      0
     C*** BEG CHG *** 04/08/99 ****************************************
     C********** CMCO#     ANDEQUPCO#
     C********** CMSUB#    ANDEQUPSUB#
     C********** CMFND     ANDEQUPFND
     C     CMCO#         ANDEQ     SCCO#
     C     CMSUB#        ANDEQ     SCSUB#
     C     CMFND         ANDEQ     SCFND
     C*** END CHG *** 04/08/99 ****************************************
     C*
     C*** ESTABLISH KEY
B4   C     XWCKEY        IFEQ      'Y'
     C                   MOVE      *BLANKS       QWKEY
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           MOVE UPCO#     QWCO#
     C                   MOVE      SCCO#         QWCO#
     C*** END CHG *** 04/08/99 ****************************************
     C                   MOVE      XXWCF#        QWFND
     C                   Z-ADD     CMFYR         QWFYR
     C                   MOVE      CMCASE        QWCASE
     C                   CALL      'SC010R'                             99
     C                   PARM                    QWKEY
     C*** BEGIN CHG *** 08/17/99 **************************************
     C**********           PARM           Q1PGM
     C                   PARM      'W0070R'      XXPGM            10
     C*** END   CHG *** 08/17/99 **************************************
X4   C                   ELSE                                                   END XWCKEY
     C                   MOVE      *BLANKS       QCKEY
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           MOVE UPCO#     QCCO#
     C**********           MOVE UPSUB#    QCSUB#
     C**********           MOVE UPFND     QCFND
     C                   MOVE      SCCO#         QCCO#
     C                   MOVE      SCSUB#        QCSUB#
     C                   MOVE      SCFND         QCFND
     C*** END ADD *** 04/08/99 ****************************************
     C                   Z-ADD     CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
 E4  C                   END                                                    END XWCKEY
 X3  C                   ELSE                                                   VALID CLAIM
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN90
 E3  C                   END                                                    VALID CLAIM
 X2  C                   ELSE                                                   SCSS#/ACDT
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN90
 E2  C                   END                                                    SCSS#/ACDT
 E1  C                   END                                                    *INKN
     C*** BEG DEL *** 11/06/97 ***
     C***********************************
     C***F15 - STATE SEARCH             *
     C***********************************
     C****
     C****       *INKP     IFEQ '1'
     C****                 CALL 'SY600R'
     C****                 PARM           SCAST
     C****                 PARM ' '       QQRTCD
     C****       QQRTCD    IFEQ 'A'
     C****                 MOVE *ON       *INKC
     C****                 ENDIF
     C****       QQRTCD    IFEQ 'B'
     C****                 Z-ADD19        SCLINE
     C****                 Z-ADD41        SCPOS
     C****                 END
     C****       QQRTCD    IFEQ '*'
     C****       XXAR      IFEQ 'Y'
     C****                 Z-ADD21        SCLINE
     C****                 Z-ADD41        SCPOS
     C****                 ELSE
     C****                 Z-ADD19        SCLINE
     C****                 Z-ADD41        SCPOS
     C****                 ENDIF
     C****                 END
     C****                 ENDIF                           F15
     C*** END DEL *** 11/06/97 ***
     C***********************************
     C*  F16=HISTORY SEARCH             *
     C***********************************
     C     *INKQ         IFEQ      '1'
      * RETURNS 27 ON IF ERROR
      * RETURNS 27 OFF IF OKAY TO CONTINUE HISTORY SEARCH
      * DOES NOT RETURN KC ON IF USER REQUESTED END LIST
     C                   EXSR      ZCMLA
      * THIS IS THE HISTORY SEARCH PART
     C     *IN27         IFEQ      *OFF
     C                   MOVE      SCACDT        QQSDAT            6 0
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE            7 0
     C*
     C                   CALL      'F0575R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCSS#
     C                   PARM                    SCACDT
     C                   PARM      ' '           QQRTCD
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    SCCO#
     C                   PARM                    SCSUB#
     C                   PARM                    SCFND
     C*** END ADD *** 04/08/99 ****************************************
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     QQRTCD        IFEQ      'B'
     C                   END
     C                   ELSE                                                   *IN27
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN27
     C*
     C                   ENDIF                                                  F16
     C******************************
     C*     F18                    *
     C******************************
B    C     *INKS         IFEQ      '1'
     C                   EXSR      ZCMLA
     C*? REALLY NEED TO TEST 60 AS WELL AS 27
 BB  C     *IN27         IFEQ      *OFF
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           MOVE UPCO#     QQCO#   3
     C                   MOVE      SCCO#         QQCO#             3
     C*** END CHG *** 04/08/99 ****************************************
     C                   MOVE      SCSUB#        Q1SUB#            5
     C                   MOVE      SCFND         Q1FND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCFEIN        ZZFEIN
     C                   MOVE      SCMOD         ZZMOD
     C                   CALL      'F0582R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCO#
     C                   PARM                    Q1SUB#
     C                   PARM                    Q1FND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM      ' '           QQRTCD
      *
 BBB C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
 EEE C                   ENDIF
      *
 BBB C     QQRTCD        IFEQ      '*'
     C                   MOVE      ZZFEIN        SCFEIN
     C                   MOVE      ZZMOD         SCMOD
 EEE C                   END
 XX  C                   ELSE
     C                   MOVE      *ON           *IN90
 EE  C                   END                                                    END SCR01
 B   C                   ENDIF                                                  F18
      *
     C*
     C****************************************************************
     C* F21=EOMB DELETE                                              *
     C****************************************************************
     C*
     C     *INKV         IFEQ      '1'
     C                   CALL      'F0150C'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   END                                                    ENDEQ SCR01
      *
     C***********************************
     C*  F24                            *
     C***********************************
     C     *INKY         IFEQ      '1'
     C     CK            ADD       1             CK                5 0
     C*
     C*  W/C not interfaced & don't allow Prov Master Maint on SCR01
     C     @IN71         IFEQ      '0'
     C*** BEGIN CHG *** 08/01/97 **************************************
     C********** CK        IFGT 5
     C     CK            IFGT      3                                            CHG 11/06/97
     C*** END CHG *** 08/01/97 ****************************************
     C                   Z-ADD     1             CK
     C                   END                                                    END CK GT 5
     C                   MOVEL     CK1A(CK)      SCCMD1
     C                   END                                                    END N71/N12
     C*  W/C interfaced
     C     @IN71         IFEQ      '1'
     C*** BEGIN CHG *** 08/01/97 **************************************
     C********** CK        IFGT 5
     C     CK            IFGT      3                                            CHG 11/06/97
     C*** END CHG *** 08/01/97 ****************************************
     C                   Z-ADD     1             CK
     C                   END                                                    END CK GT 5
     C                   MOVEL     CK1C(CK)      SCCMD1
     C                   END                                                    END  71/N12
     C*
     C                   END                                                    END *INKY
     C     TERM          ENDSR
      *
      /EJECT
      **************************************************************
      ** PROGRAM END                                             ***
      **************************************************************
     C     PGMEND        BEGSR
     C     LCKOUT        IFEQ      '1'
     C                   EXSR      RMVLCK
     C                   ENDIF
     C                   ENDSR
      *
      /EJECT
      **************************************************************
      ** PROGRAM INITIALIZATION                                  ***
      **  ONE TIME, AND PERHAPS AFTER F13=CHG USR PARMS          ***
      **************************************************************
     C     PGMSTR        BEGSR
     C                   MOVE      '1'           F3REQD
     C                   CLEAR                   SCPOS
     C                   CLEAR                   SCLINE
     C                   CLEAR                   SCPS91
     C                   CLEAR                   SCPS92
     C                   CLEAR                   SCLI91
     C                   CLEAR                   SCLI92
     C                   EXSR      PGMS02                                       SMUPL GET
     C*** BEG CHG *** 04/08/99 ****************************************
     C********** UPCO#     IFNE *ZEROS
     C     SCCO#         IFNE      *ZEROS
     C*** END CHG *** 04/08/99 ****************************************
     C                   EXSR      PGMS03                                       SMCOL GET
     C                   EXSR      PGMS04                                       PFN01
     C                   EXSR      PGMS05                                       PSM01
     C                   EXSR      PGMS06                                       INSTALLED
      * COMMAND KEY DECISIONS
     C                   Z-ADD     1             CK
     C  N71
     CANN99              MOVEL     CK1A(CK)      SCCMD1
     C   71
     CANN99              MOVEL     CK1C(CK)      SCCMD1
     C  N71
     CAN 99              MOVEL     CK1E(CK)      SCCMD1
     C   71
     CAN 99              MOVEL     CK1G(CK)      SCCMD1
     C                   ENDIF
      * WHY HERE ? FOR KLISTS
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    SCSS#
     C*
     C     FNKEY3        KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C                   KFLD                    SCCO#
     C*** END CHG *** 04/08/99 ****************************************
     C                   KFLD                    XXFND
     C*
     C*  CLAIM LOCK OUT FILE
     C*
     C     LOKEY         KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           UPFND
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*** END CHG *** 04/08/99 ****************************************
     C                   KFLD                    XXFYR
     C                   KFLD                    @CCASE            7 0
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*  KEY LIST FOR STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXSTKY           10
     C*
     C                   ENDSR
     C*
      *
      /EJECT
      **************************************************************
      ** PGMS02 : USER PARM RETRIEVAL. FILE/RECORD=SMUPL/PUP01   ***
      **************************************************************
     C     PGMS02        BEGSR
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           MOVE *ZERO     UPFND
     C**********           ELSE
     C**********           Z-ADDUPFND     XXFND
     C*
     C                   MOVE      *ZERO         SCCO#             3 0
     C                   MOVE      *ZERO         SCSUB#
     C                   MOVE      *ZERO         SCFND
     C                   ELSE
     C                   MOVE      UPCO#         SCCO#
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPFND         XXFND
     C*** END CHG *** 04/08/99 ****************************************
     C                   END
     C*********  UPLVL     IFLT 090
     C** THIS CODE S/B *   MOVE '0'       XXIN69  1
     C** AT CHAIN, IF *    ELSE                       ???
     C** NEEDED AT ALL *   MOVE '1'       XXIN69
     C** MAKE IT .ISS**    ENDIF
     C******************   GOTO TOP
      **
     C                   ENDSR
      *
      /EJECT
      **************************************************************
      ** PGMS03 : COMPANY   RETRIEVAL. FILE/RECORD=SMCOL/PCO01   ***
      **************************************************************
     C     PGMS03        BEGSR
      *
      *  GET THE COMPANY RECORD
      *
     C*** BEG CHG *** 04/08/99 ****************************************
     C********** UPCO#     CHAINPCO01                60
     C     SCCO#         CHAIN     PCO01                              60
     C*** END CHG *** 04/08/99 ****************************************
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   ENDIF
     C*
      **
     C                   ENDSR
      /EJECT
      **************************************************************
      ** PGMS04 : SUBSCRIBER RETRIEVAL. ILE/RECORD=SMCOL/PCO01   ***
      **************************************************************
     C     PGMS04        BEGSR
     C*
     C*  GET THE SUBSCRIBER RECORD
     C*
     C     SMKEY         KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C*** END CHG *** 04/08/99 ****************************************
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C**********           MOVE UPSUB#    SCSUB#           DEL 04/08/99
     C                   MOVEL     SMNAME        SCSBNM
     C*
      *
     C                   ENDSR
      /EJECT
      **************************************************************
      ** PGMS05 : GRP/FUND  RETRIEVAL. FILE/RECORD=SMCOL/PCO01   ***
      **************************************************************
     C     PGMS05        BEGSR
     C*
     C*  GET THE FUND RECORD
     C*
     C*   KEY LIST FOR THE FUND
     C     FNKEY         KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           UPFND
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*** END CHG *** 04/08/99 ****************************************
     C     FNKEY         CHAIN     PFN01                              60
     C**********           MOVE UPFND     SCFND            DEL 04/08/99
     C                   MOVEL     FNNAME        SCGRNM
     C                   Z-ADD     FNWCF#        XXWCF#            3 0
     C*
      *
     C                   ENDSR
      /EJECT
      **************************************************************
      ** DETERMINE WHAT FEATURES ARE INSTALLED ON THIS MACHINE   ***
      **************************************************************
     C     PGMS06        BEGSR
     C*
     C     F1KEY         KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           UPFND
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*** END CHG *** 04/08/99 ****************************************
     C*
      *  FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
      ** CHECK FOR INTERFACE WITH WORKERS' COMP
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      'Y'           XXWCFL            1
      *
     C     F1KEY         CHAIN     PF101                              50
     C     *IN50         IFEQ      '0'
     C     F1OUTA        IFEQ      'N'
     C                   Z-ADD     4             X
     C     1             SUBST     FBAL01:X      XDKEY             1    65
     C     XDKEY         IFEQ      'Y'
     C                   MOVE      'Y'           XWCKEY            1
     C                   ELSE
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END XDKEY
     C                   MOVE      '1'           @IN71             1
      *
     C                   ELSE                                                   ELSE F1OUTA
     C                   MOVE      '0'           @IN71
     C                   MOVE      'N'           XWCKEY                         ADD 02/09/94
     C                   END                                                    END F1OUTA
     C                   END                                                    END *IN50
     C                   ELSE                                                   ELSE FBFL01 = 'Y'
     C                   MOVE      'N'           XWCKEY                         ADD 02/09/94
     C                   MOVE      '0'           @IN71
     C                   MOVE      'N'           XXWCFL                         ADD 03/12/93
     C                   END                                                    END FBFL01 = 'Y'
      *
      * CHECK TO SEE IF AUDIT/RECON ALLOWED
      *
     C                   Z-ADD     10            X                 5 0
     C     1             SUBST     FBAL01:X      XXAR              1    65
     C     XXAR          IFEQ      'Y'
     C                   MOVE      '1'           *IN99
     C                   ELSE                                                   ELSE XXAR EQ 'Y'
     C                   MOVE      '0'           *IN99
     C                   END                                                    END XXAR EQ 'Y'
      *
      ** CHECK FOR NETWORK USE  : FBFL06=Y IS YES
      *
      ** CHECK FOR PRE-CERT INSTALLED  : FBFL07=Y IS YES
      *
      * CHECK IF ALLOW SCRRX FOR PHARMACY BILLS
      *
     C     FBFL10        IFEQ      'Y'
     C                   MOVEL     SMSG(1)       SCSMSG                         1=RX BILL
     C                   ELSE
     C                   MOVE      *BLANKS       SCSMSG
     C                   END
      *
      * CHECK IF ALLOW FOR DENTAL BILLS
      *
     C     FBAL03        IFNE      *BLANKS
     C                   MOVEL     SMSG(2)       SCSMS1                         2=DENTAL
     C                   ELSE
     C                   MOVE      *BLANKS       SCSMS1
     C                   END
      *
      * CHECK IF CODEREVIEW CLAIMS EDIT IS INSTALLED
      * N = NOT INSTALLED
      * I = INSTALLED, CALLED INTERACTIVELY
      * B = INSTALLED, JOB SUBMITTED
      *
     C     FBFL04        IFEQ      'I'
     C                   MOVE      'I'           XXCDRV            1
     C                   ELSE                                                   ELSE FBFL04 EQ 'I'
     C     FBFL04        IFEQ      'B'
     C                   MOVE      'B'           XXCDRV
     C                   ELSE                                                   ELSE FBFL04 EQ 'B'
     C                   MOVE      'N'           XXCDRV
     C                   END                                                    END FBFL04 EQ 'B'
     C                   END                                                    END FBFL04 EQ 'I'
     C*
     C                   END                                                    END *IN60 SFB01
     C                   ENDSR
      *
      /EJECT
      **************************************************************
      ** PROCESSING AFTER ALL EDITS PASSED                       ***
      **************************************************************
     C     PRSCTL        BEGSR
      * PRESENTLY ALLOK IS JUST THE RECORD LOCKOUT
     C                   EXSR      ALLOK
      * ADJUDICATION STATE AND ASSOCIATED FILE OVERRIDES
     C                   EXSR      OVRST
     C                   EXSR      DCINIT
     C* EXSR F0091R
      *
     C* EXSR F0092R
      *
     C* EXSR F0070R
     C     SCPRTY        IFNE      'H'
     C                   MOVE      'B'           QQRT70                         70'S RTCD
     C                   MOVE      'X'           QQRT91                         91'S RTCD
     C     QQRT91        DOWEQ     'X'                                          91 INCOMPLT
     C     QQRT70        OREQ      'B'                                          70'S
     C*** BEGIN ADD *** 05/14/97 **************************************
     C*** $1000 MED ONLY PGM WINDOW FOR MEDICAL BILL
     C****
     C****       SCAST     IFEQ 'OH'
     C****       EIKEY     CHAINPEI01                60
     C****       *IN60     IFEQ '0'
     C****
     C****       EIMOFL    IFEQ 'Y'
     C****                 MOVE EIMOFL    SCMOFL
     C****                 MOVE EIMODT    SCMODT
     C****                 Z-ADDSCMODT    SRDTI
     C****                 CALL 'SRCVTDAT'DTMDYY
     C****                 MOVELSRDTO     SCMODT
     C****                 EXFMTWDW01
     C****       *IN12     IFEQ '1'
     C****       *INKL     ANDEQ'1'
     C****                 MOVE '1'       *IN90
     C****                 EXSR RMVLCK
     C****       *IN90     CABEQ'1'       ENDPRS
     C****                 ENDIF                           *IN33 IFEQ
     C****                 ENDIF                           EIMOFL IFEQ
     C****                 ENDIF                           *IN60 IFEQ
     C****                 ENDIF                           SCAST IFEQ
     C*** END ADD *** 05/14/97 ****************************************
     C                   EXSR      F0091R
     C                   MOVE      ' '           QQRT70                         REINIT
     C*** BEG CHG *** 04/24/97 ************************
     C********** QQRT91    IFNE 'B'
     C     QQRT91        IFEQ      ' '
     C*** BEG CHG *** 04/24/97 ************************
     C                   EXSR      F0070R
     C                   ENDIF                                                  QQRT91<>B
     C                   ENDDO                                                  QQRT91=X
     C                   ENDIF                                                  SCPRTY<>H
      * HOSPITAL
     C     SCPRTY        IFEQ      'H'
     C                   MOVE      'X'           QQRT91
     C                   MOVE      'B'           QQRT70
     C     QQRT91        DOWEQ     'X'
     C     QQRT92        OREQ      'B'
     C*** BEGIN ADD *** 05/14/97 **************************************
     C*** $1000 MED ONLY PGM WINDOW FOR HOSPITAL BILL
     C****
     C****       SCAST     IFEQ 'OH'
     C****       EIKEY     CHAINPEI01                60
     C****       *IN60     IFEQ '0'
     C****
     C****       EIMOFL    IFEQ 'Y'
     C****                 MOVE EIMOFL    SCMOFL
     C****                 MOVE EIMODT    SCMODT
     C****                 Z-ADDSCMODT    SRDTI
     C****                 CALL 'SRCVTDAT'DTMDYY
     C****                 MOVELSRDTO     SCMODT
     C****                 EXFMTWDW01
     C****       *IN12     IFEQ '1'
     C****       *INKL     ANDEQ'1'
     C****                 MOVE '1'       *IN90
     C****                 EXSR RMVLCK
     C****       *IN90     CABEQ'1'       ENDPRS
     C****                 ENDIF                           *IN33 IFEQ
     C****                 ENDIF                           EIMOFL IFEQ
     C****                 ENDIF                           *IN60 IFEQ
     C****                 ENDIF                           SCAST IFEQ
     C*** END ADD *** 05/14/97 ****************************************
     C                   EXSR      F0091R
     C                   MOVE      'X'           QQRT92
     C     QQRT92        DOWEQ     'X'
     C     QQRT91        ANDEQ     ' '
     C     QQRT70        OREQ      'B'
     C     QQRT91        ANDEQ     ' '
     C*** BEGIN ADD *** 04/24/97 **************************************
     C                   MOVEL     SCBTYP        SCBTY2
     C*** END ADD *** 04/24/97 ****************************************
     C                   EXSR      F0092R
     C                   MOVE      ' '           QQRT70                         REINIT
     C     QQRT92        IFEQ      ' '
     C                   EXSR      F0070R
     C                   ENDIF                                                  QQRT92=BLNK
     C                   ENDDO                                                  COMPOUND
     C                   ENDDO                                                  QQRT91=X
     C                   ENDIF                                                  SCPRTY =H
      ***********************************************************
      * TRANSITION BACK TO F0090R/SCR01
      ***********************************************************
     C                   EXSR      RMVLCK
     C     QQRT70        IFEQ      'A'                                          F3 FROM 70R
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*** BEG ADD *** 04/24/97 ************************
     C     QQRT91        IFEQ      'A'                                          ADJ FROM 70R
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*** END ADD *** 04/24/97 ************************
     C     QQRT91        IFEQ      ' '                                          ADJ FROM 70R
     C                   EXSR      REINIT
     C                   ENDIF
      *
     C*** BEGIN ADD *** 05/14/97 **************************************
     C****       ENDPRS    TAG
     C*** END ADD *** 05/14/97 ****************************************
     C                   ENDSR
      /EJECT
      **************************************************************
      ** ANY PRE SCREEN PROCESSING DONE HERE                     ***
      **************************************************************
     C     PREDSP        BEGSR
     C                   ENDSR
      /EJECT
      **************************************************************
      ** REINIT WILL PREPARE FOR NEXT BILL AFTER PREVIOUS PRSCTL ***
      **************************************************************
      *
     C     REINIT        BEGSR
      *
     C                   EXSR      RMVLCK
      *
     C                   CLEAR                   SCSS#
     C                   CLEAR                   SCACDT
     C                   CLEAR                   SCFEIN
     C                   CLEAR                   SCMOD
     C                   CLEAR                   SCSYS
     C                   CLEAR                   SCPRTY
     C                   CLEAR                   SCSMSG
     C                   CLEAR                   SCSMS1
     C                   CLEAR                   SCTYPE
     C                   CLEAR                   SCAST
     C                   CLEAR                   SCAR
     C                   CLEAR                   SCEOB#
     C                   CLEAR                   XCFIH
     C                   CLEAR                   XCMDH
     C                   CLEAR                   SCLINE
     C                   CLEAR                   SCPOS
     C*** BEGIN DEL ** 04/10/97 ************************************************
     C********             CLEARSCAMOD
     C****                 CLEARSCBWC#                     DEL 11/06/97
     C*** END DEL ** 04/10/97 **************************************************
     C*** BEGIN ADD *** 07/22/97 **************************************
     C                   CLEAR                   SCDT09
     C*** END ADD *** 07/22/97 ****************************************
      *
     C                   ENDSR
      /EJECT
      **************************************************************
      ** REMOVE LOCKOUT                                          ***
      **************************************************************
     C     RMVLCK        BEGSR
     C     LOKEY         CHAIN     PLO01                              60
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   MOVE      '0'           LCKOUT
     C                   END
      *
     C                   ENDSR
      /EJECT
      **************************************************************
      ** GET CLAIM BY CO,SUB,FND,FYR,CASE                        ***
      **  PASS IN XXFND, XXFYR, XXCASE
      **************************************************************
     C     ZCML1         BEGSR
     C     CMKEY2        KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C*** END CHG *** 04/08/99 ****************************************
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE
      *
     C     CMKEY2        CHAIN     PCM02                              60
B3   C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   Z-ADD     CMMMI         SRDTI                          MJF
     C                   CALL      'SRCVTDAT'    DTMDYY                          *
     C                   MOVEL     SRDTO         XXMMI             8 0           *
     C                   ELSE
     C                   CLEAR                   SCSS#
     C                   CLEAR                   SCACDT
E3   C                   END                                                    END 0 *IN60
      *
     C                   ENDSR
      /EJECT
      **************************************************************
      ** VALIDATE CLAIM USING FMCMLA: SS#/ACDT VIEW              ***
      **************************************************************
     C     ZCMLA         BEGSR
     C     CMKEY         KLIST
     C                   KFLD                    SCSS#
     C                   KFLD                    XXACDT
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*** Check for multiple cases under the same SS#/ACDT
     C                   Z-ADD     *ZEROS        CNTCAS            3 0
     C     CMKEY         CHAIN     PCM01                              27         SS#/ACDT KEY
     C     *IN27         DOWEQ     '0'
     C                   ADD       1             CNTCAS
     C     CMKEY         READE     PCM01                                  27
     C                   END
     C*** If there is more than 1 case, then display "Multi Case" Search F0579F
B1   C     CNTCAS        IFGT      1
     C                   MOVE      *OFF          *IN27
     C                   MOVE      SCACDT        QQACDT
     C                   MOVE      *BLANKS       Q1FND             3
     C                   MOVE      *BLANKS       QQFYR
     C                   MOVE      *BLANKS       QQCASE
     C                   CALL      'F0579R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL
     C                   PARM                    SCSS#
     C*******************  PARM           QQACDT  6        DEL 03/10/98
     C                   PARM                    QQACDT            8            ADD 03/10/98
     C                   PARM                    Q1FND
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD
     C*
B2   C     QQRTCD        IFEQ      'A'
     C     QQRTCD        OREQ      'B'
     C     Q1FND         OREQ      *BLANKS
     C                   MOVE      '1'           *IN27
E2   C                   END                                                    END EQ A/B/*BLANKS
      *
B2   C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C     @IN71         IFEQ      '1'
     C                   EXSR      WCFND#
     C                   END
      *
     C                   EXSR      ZCML1
      * REALLY NEED *IN60 LOGIC HERE IN CASE NOT FOUND  ????
E2   C                   END                                                    END EQ *
     C                   ELSE
     C     CNTCAS        IFEQ      1
     C                   MOVE      *OFF          *IN27
     C                   ENDIF
E1   C                   END                                                    END GT 1
     C*
     C                   ENDSR
      *
      /EJECT
     C     ALLOK         BEGSR
     C*************
     C***A*VALID*SS# AND ACDT HAVE BEEN ENTERED
     C*************        MOVE CMCASE    SCCASE
     C*************        MOVE CMFYR     @CYR
     C***********CMMMI     MULT 100.0001  SCMMI            ADD 01/16/95
     C***A*VALID*PROVIDER/MODIFIER HAS BEEN SELECTED, MOVE TO HOLD
     C*************        Z-ADDSCFEIN    HFE,1
     C*************        Z-ADDSCMOD     HMO,1
     C*************
     C*************
     C*************        MOVELSCFEIN    XXFEIN
     C*************        MOVELSCMOD     XXMOD
     C     PDKEY         CHAIN     PP101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     P1CNTY        SCCNTY           15            ADD 04/29/93
     C                   ELSE
     C                   MOVE      *BLANKS       SCCNTY                         ADD 04/29/93
     C                   END
     C                   Z-ADD     *ZEROS        SCPZIP            5 0
     C                   Z-ADD     PDZIP         XXPZIP            9 0
     C                   Z-ADD     X1PZIP        SCPZIP
     C                   MOVE      *BLANKS       XXZIPA            3
     C                   MOVE      *BLANKS       SCZIPA            3
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C                   MOVEL     P1ZIP         XXZIPA
     C*
     C**2nd - Get the corresponding Geographic Zip Area record:
     C*  GEOGRAPHIC ZIP AREA FILE
     C     GZKY          KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C                   KFLD                    SCCO#
     C*** END CHG *** 04/08/99 ****************************************
     C                   KFLD                    XXZIPA
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C     *IN60         IFEQ      '0'
     C     XXZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        SCZIPA
     C                   ELSE
     C*** BEGIN DEL *** 03/01/97 **************************************
     C**********           CLEARSCZIPA
     C*** END DEL *** 03/01/97 ****************************************
     C*** BEGIN ADD *** 01/14/98 **************************************
     C     SCAST         IFEQ      'OH'
     C*** END ADD *** 01/14/98 ****************************************
     C*** BEGIN ADD *** 03/01/97 **************************************
     C                   MOVEL     '430'         SCZIPA
     C*** END ADD *** 03/01/97 ****************************************
     C*** BEGIN ADD *** 01/14/98 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       SCZIPA
     C                   ENDIF
     C*** END ADD *** 01/14/98 ****************************************
     C                   END                                                    END 0 *IN60
      * GET COUNTY NAME
     C                   MOVEL     'OH'          X5                5
     C* ??? WILL HAVE TO   MOVE P1CNTY POSITION 6-9
     C*************
     C**CHAIN*TO*SPECIALTY CODE TABLE FILE TO GET DESCRIPTION        *
     C*************        MOVE *BLANKS   SCSPEC
     C*************        MOVE *BLANKS   TABKEY
     C*************        MOVELPDSPCD    TABKEY
     C*************        MOVELPDSPCD    SCSPCD
     C*************        MOVELPDSPCD    HLSPCD           ADD 12/28/92
     C***********SPKY      CHAINPSP01                60
     C************IN60     IFEQ '0'
     C*************        EXCPTRSP01
     C*************        MOVELSPDESC    SCSPEC
     C*************        END
     C*END*VALID*PROVIDER/MODIFIER HAS BEEN SELECTED, MOVE TO HOLD
     C*************
     C**IF*USER*ENTERED EOMB NUMBER AND DID NOT ENTER AN AUDIT RECON FLAG
     C**ZERO*OUT*SCEOB# AND IGNORE.  THIS WAY WE CAN USE SCEOB# TO DETERMINE
     C**WHAT*KIND*OF CHARGES WE ARE PROCESSING(I.E. EOB# > ZERO AUDIT/RECON
     C**EOB#*=*ZEROS CUREENT).
     C*************
     C***********SCAR      IFEQ *BLANKS
     C*************        Z-ADD*ZEROS    SCEOB#
     C*************        END
     C**FINAL*MOD*SELECT :
     C***********          EXSR SRPRMO
     C**FILE*OVERRIDES
     C**********           EXSR OVRST
     C**????*FOLLOWING CODE IS SUSPICIOUS ????????
     C*****THIS*CODE WILL HAVE TO RESIDE IN 70R
B1   C     SCAST         IFEQ      'FL'
     C     SCAST         OREQ      'LA'                                         ADD 06/30/93
     C                   MOVEL     HD5(1)        SCTITL
X1   C                   ELSE
B2   C     SCAST         IFEQ      'OK'
     C                   MOVEL     HD5(2)        SCTITL
X2   C                   ELSE
     C                   MOVEL     HD5(3)        SCTITL
E1   C                   END
E2   C                   END
     C************IN09     IFEQ '0'
     C***********ST1A2     IFEQ 'Y'
     C*************        MOVE 'Y'       XX1A2
     C*************        ELSE                            ELSE ST1A2 EQ 'Y'
     C*************        MOVE *BLANKS   XX1A2
     C*************        END                             END ST1A2 EQ 'Y'
     C*************
     C***********SCTYPE    IFNE *BLANKS
     C*************        Z-ADD1         T       20
     C***********SCTYPE    LOKUPBS,T                     70
     C************IN70     IFEQ '1'
     C*************        MOVELSCR,T     Q2RECF
     C*************        MOVE *BLANKS   H1RECF
     C*************        MOVELQ2RECF    H1RECF
     C*************        ELSE                            ELSE *IN70 EQ '1'
     C*************        MOVEL'SCR04'   Q2RECF
     C*************        MOVEL'SCR04'   H1RECF
     C*************        END                             END *IN70 EQ '1'
     C*************        ELSE                            ELSE SCTYPE EQ *BLANK
     C*************        MOVEL'SCR04'   Q2RECF
     C*************        MOVEL'SCR04'   H1RECF
     C*************        END                             END SCTYPE NE *BLANKS
     C*************
     C****MEDICAL*BILL - GOTO SCR02
     C*************
     C***********SCPRTY    IFEQ 'N'
     C***********SCPRTY    OREQ 'D'
     C*************
     C*************        MOVEL'SCR02'   H1RECF
     C*************        Z-ADD1         KY2,1
     C*************        Z-ADD10        KY2,2
     C*************        EXSR SC2LD
     C*************        EXSR DCSR                       DIAG CODE SUBR
     C*************        MOVE '1'       *IN24            POS CURSOR CLM STS
     C*************        MOVE '0'       *IN95
     C*************        EXSR PNDSR
     C*???*THIS*SEEMS TO BE AN ENTER KEY, ALL EDITS PASSED PATH???
     C*************        GOTO STRT
     C*************        ELSE
     C***********SCPRTY    IFEQ 'X'
     C***********XXRXFL    IFNE 'Y'
     C*************        WRITESCR01
     C*************        MOVE *BLANKS   Q2RECF
     C*************        MOVEL'SCR01'   Q2RECF
     C*************        MOVE '1'       *IN15
     C*************        GOTO STRT
     C*************        ELSE
     C*************        MOVEL'SCRRX'   Q2RECF
     C*************        MOVEL'SCRRX'   H1RECF           ADD 04/15/94
     C*************        Z-ADD1         KY2,1
     C*************        Z-ADD10        KY2,2
     C*************        EXSR SCXLD
     C*************        EXSR DCSR                       DIAG CODE SUBR
     C*************        MOVE '1'       *IN24            POS CURSOR CLM STS
     C*************        MOVE '0'       *IN95
     C*************        EXSR PNDSR
     C*************
     C*************        GOTO STRT
     C*************        END
     C*************
     C*************        ELSE                            ELSE EQ 'N'
     C*************
     C****HOSPITAL*BILL - GOTO SCR04
     C***********SCPRTY    IFEQ 'H'
     C***********H1RECF    IFEQ 'SCR04'
     C*************        MOVE *BLANKS   @CCMD4 70
     C***********XX1A2     IFNE 'Y'
     C*************        MOVELCK4,CK    @CCMD4           04/28/92 ADD
     C*************        ELSE                            ELSE XX1A2 NE 'Y'
     C*************        MOVELCK4A,CK   @CCMD4
     C*************        END                             END XX1A2 NE 'Y'
     C*************        Z-ADD1         KY2,1
     C*************        Z-ADD10        KY2,2
     C*************        EXSR INIT04
     C*************        EXSR DCSR                       DIAG CODE SUBR
     C*************        MOVE '1'       *IN24            POS CURSOR CLM STS
     C*************        MOVE '0'       *IN95
     C*************        EXSR PNDSR
     C*************        ELSE                            ELSE H1RECF EQ SCR04
     C***********H1RECF    IFEQ 'SCR02'
     C*************        Z-ADD1         KY2,1
     C*************        Z-ADD10        KY2,2
     C*************        EXSR SC2LD
     C*************        EXSR DCSR                       DIAG CODE SUBR
     C*************        MOVE '1'       *IN24            POS CURSOR CLM STS
     C*************        MOVE '0'       *IN95
     C*************        EXSR PNDSR
     C*************        END                             END H1RECF EQ 'SCR02'
     C*************        END                             END H1RECF EQ 'SCR04'
     C*************
     C*************        GOTO STRT
     C*************        END                             END EQ 'H'
     C*************        END                             END EQ 'X'
     C*************        END                             END EQ 'N'
     C*************
     C*************        ELSE                            ELSE *IN09
     C*************
     C****INQUIRY*MODE
     C*************        MOVEL'SCR03'   Q2RECF
     C*************        END                             END *IN09
     C*************
     C* LOCKOUT STUFF
     C                   MOVE      '1'           LCKOUT            1
     C                   MOVE      Q1USER        LOUSER
     C                   MOVE      Q1PGM         LOPGM
     C*** BEGIN CHG *** 08/08/97 **************************************
     C********             TIME           DSDTTM
     C********   DSDATE    MULT 10000.01  LODT
     C********             Z-ADDDSTIME    LOTIME
     C                   TIME                    DFDTTM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LODT
     C                   Z-ADD     DFTIME        LOTIME
     C*** END CHG *** 08/08/97 ****************************************
     C                   WRITE     PLO01
      *
     C                   MOVE      CMFYR         @CYR              3 0
     C                   MOVE      CMCASE        @CCASE
     C                   MOVE      *BLANKS       @QLAST                         ADD 08/19/94
     C     CDKEY         CHAIN     PCD01                              60
     C****???******        EXCPTRCD01
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         @QLAST           56
     C                   MOVEL     CDFSN         @QFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    @QLAST
     C                   PARM                    @QFRST
     C                   PARM                    QQTERM
     C                   MOVEL     @QLAST        @CCNAM           56
     C                   ELSE
     C                   MOVEL     *BLANKS       @CCNAM           56
     C                   END
     C                   ENDSR
      /EJECT
      ***********************************************************
     C* OVRST                                                         *
     C* THIS SUBROUTINE PERFORMS OVERRIDES TO PROCEDURE CODE FILE     *
     C*      AND THE TABLE FILES AND THE SERVICE CODE FILE            *
     C* AND BASES SAID OVERRIDES ON THE STATE FOUND IN THE PROV FILE. *
      ***********************************************************
     C     OVRST         BEGSR
     C*
     C     SCAST         IFEQ      *BLANKS
     C                   MOVE      P1ST          SCAST
     C                   MOVE      P1ST          SCOVR             2
     C                   ENDIF
     C*********
     C*** CLOSE FILE IF OPENED
     C*********  OPNFL1    IFEQ '1'
     C*********            CLOSEFMPRL
     C*********            MOVE ' '       OPNFL1
     C*********            END
     C*********  OPNFL2    IFEQ '1'
     C*********            CLOSEFTABL
     C*********            MOVE ' '       OPNFL2
     C*********            END
     C*********  OPNFL3    IFEQ '1'
     C*********            CLOSEFMSVL1
     C*********            MOVE ' '       OPNFL3
     C*********            END
     C*********  OPNFL4    IFEQ '1'
     C*********            CLOSEFTPSL
     C*********            MOVE ' '       OPNFL4
     C*********            END
     C*********
     C****DELETE OVERRIDE
     C****Delete Override to FMPRL
     C*********  OVRFL1    IFEQ '1'
     C*********            Z-ADD56        LENGTH
     C*********            CALL 'QCMDEXC'                60
     C*********            PARM           QCA,3
     C*********            PARM           LENGTH 155
     C*********            MOVE ' '       OVRFL1
     C*********            END
     C****BEGIN ADD *** 04/14/92 **************************************
     C****Delete Override to FTABL
     C*********  OVRFL2    IFEQ '1'
     C*********            Z-ADD56        LENGTH
     C*********            CALL 'QCMDEXC'                60
     C*********            PARM           QCA,5
     C*********            PARM           LENGTH
     C*********            MOVE ' '       OVRFL2
     C*********            END
     C****Delete Override to FMSVL1
     C*********  OVRFL3    IFEQ '1'
     C*********            Z-ADD56        LENGTH
     C*********            CALL 'QCMDEXC'                60
     C*********            PARM           QCA,7
     C*********            PARM           LENGTH 155
     C*********            MOVE ' '       OVRFL3
     C*********            END
     C****Delete Override to FMSVL1
     C*********  OVRFL4    IFEQ '1'
     C*********            Z-ADD56        LENGTH
     C*********            CALL 'QCMDEXC'                60
     C*********            PARM           QCA,11
     C*********            PARM           LENGTH 155
     C*********            MOVE ' '       OVRFL4
     C*********            END
     C*********
     C****Override to FMPRL
     C*********  OVRFL1    IFEQ ' '
     C*********            Z-ADD56        LENGTH
     C*********            MOVELQCA,4     @@CMD
     C*********            MOVELP1ST      @@ST             02/21/91 ADD
     C*********            CALL 'QCMDEXC'
     C*********            PARM           @@CMD
     C*********            PARM           LENGTH 155
     C*********            MOVE '1'       OVRFL1
     C*********            END
     C****Override to FTABL
     C*********  OVRFL2    IFEQ ' '
     C*********            Z-ADD56        LENGTH
     C*********            MOVELQCA,6     @@CMD
     C*********            MOVELP1ST      @@ST
     C*********            CALL 'QCMDEXC'
     C*********            PARM           @@CMD
     C*********            PARM           LENGTH 155
     C*********            MOVE '1'       OVRFL2
     C*********            END
     C****Override to FMSVL1
     C*********  OVRFL3    IFEQ ' '
     C*********            Z-ADD56        LENGTH
     C*********            MOVELQCA,8     @XCMD
     C*********            MOVELP1ST      @XST
     C*********            CALL 'QCMDEXC'
     C*********            PARM           @XCMD
     C*********            PARM           LENGTH 155
     C*********            MOVE '1'       OVRFL3
     C*********            END
     C****Override to FTPSL
     C*********  OVRFL4    IFEQ ' '
     C*********            Z-ADD56        LENGTH
     C*********            MOVELQCA,12    @@CMD
     C*********            MOVELP1ST      @@ST
     C*********            CALL 'QCMDEXC'
     C*********            PARM           @@CMD
     C*********            PARM           LENGTH 155
     C*********            MOVE '1'       OVRFL4
     C*********            END
     C*********
     C*********            MOVELP1ST      STOVR   2          SAVE NEW STATE
     C*********            MOVELP1ST      SCAST
     C*********
     C****OPEN*FILE FOR PROCESSING
     C*********  OPNFL1    IFNE '1'                          NOT CURRENTLY OPEN
     C*********            OPEN FMPRL
     C*********            MOVE '1'       OPNFL1
     C*********            END
     C*********  OPNFL2    IFNE '1'                          NOT CURRENTLY OPEN
     C*********            OPEN FTABL
     C*********            MOVE '1'       OPNFL2
     C*********            END
     C*********  OPNFL3    IFNE '1'                          NOT CURRENTLY OPEN
     C*********            OPEN FMSVL1
     C*********            MOVE '1'       OPNFL3
     C*********            END
     C*********  OPNFL4    IFNE '1'                          NOT CURRENTLY OPEN
     C*********            OPEN FTPSL
     C*********            MOVE '1'       OPNFL4
     C*********            END
     C*********
     C*********            WRITESCR99
     C*********            MOVEL*BLANKS   Q2RECF
     C*********            MOVELHLRECF    Q2RECF
     C*********
     C*********            ELSE
     C*********            MOVEL*BLANKS   Q2RECF
     C*********            MOVELHLRECF    Q2RECF
     C*********  Q2RECF    IFEQ 'SCR01'
     C*********            WRITESCR01
     C*********            END
     C*********  Q2RECF    IFEQ 'SCR02'
     C*********            WRITESCR02
     C*********            END
     C*********            MOVE '1'       *IN37
     C*********            GOTO STRT
     C*********            END                             END 0 *IN60
     C*********
     C                   ENDSR
      /EJECT
     C     WCFND#        BEGSR
      ***********************************************************
      **  TRANSLATE THE WORKERS COMP FUND TO MANAGED CARE FUND **
      ***********************************************************
     C     FNKEY3        CHAIN     PFN02                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNFND         XXFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXFND
     C                   END
     C                   ENDSR
      /EJECT
     C     F0534R        BEGSR
     C*****************************************************************
     C* WAS SRPRMO IN F0070R    *
     C* THIS SUBROUTINE VALIDATES THE PROVIDER/MODIFIER. IF THE       *
     C* PROVIDER HAS MULTIPLE MODIFIERS A SEARCH IS CALLED FOR THE    *
     C* USER TO SELECT A MODIFIER.                                    *
     C*****************************************************************
     C*
     C* ALLOW MODIFIER SELECTION
     C*
     C     *LIKE         DEFINE    PDFEIN        QQFEIN
     C     *LIKE         DEFINE    PDMOD         QQMOD
     C     *LIKE         DEFINE    PDDT          XXACDT
     C*
     C                   MOVE      *BLANK        QQTYPE            1
     C                   CALL      'F0534R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQTYPE
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQEFDT            6
     C*** BEGIN DEL *** 02/19/98 **************************************
     C**********           PARM           CMFND
     C**********           PARM           CMFYR
     C**********           PARM           CMCASE
     C*** END   DEL *** 02/19/98 **************************************
     C                   PARM                    QQRTCD
     C*
     C                   ENDSR
**  HD5 - SCR04 COLUMN HEADINGS FOR SVC CODE VARIES BY STATE
HCPCSRVCDUNTS
 DRG RVCDDAYS
CODE CODEUNTS
**  CK1A - COMMAND KEYS for SCR01  (N71/   )
F3=Exit    F10=Search    F13=Chg User Parms    F24=More keys
F16=History Search   F18=Prov Curr Chrgs       F24=More keys
F21=EOMB Delete      F24=More keys
**  CK1C - COMMAND KEYS for SCR01  ( 71/   )
F3=Exit    F10=Search    F13=Chg User Parms    F24=More keys
F16=History Search   F18=Prov Curr Chrgs       F24=More keys
F21=EOMB Delete      F24=More keys
**  CK1E - COMMAND KEYS for SCR01  (N71/   )
F3=Exit    F10=Search    F13=Chg User Parms    F24=More keys
F16=History Search   F18=Prov Curr Chrgs   F24=More keys
F21=EOMB Delete     F23=Post Audit/Recon   F24=More keys
**  CK1G - COMMAND KEYS for SCR01  ( 71/   )
F3=Exit    F10=Search    F13=Chg User Parms    F24=More keys
F16=History Search   F18=Prov Curr Chrgs   F24=More keys
F21=EOMB Delete     F23=Post Audit/Recon   F24=More keys
**  SMSG - PRTY TYPE AVAILABLE
(X = Rx Bill)
(D = Dental Bill)
