     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM SUBROUTINE CR001R - BUILD CLAIMANT NAME PROGRAM
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF PROGRAM AND
     H*  DARTS KEY; RETRIEVE CLAIMANT NAME FROM WORK COMP FILE AND
     H*  RETURN CLAIMANT NAME TO SRKCVT PGM.
     H*****************************************************************
     H*  PROGRAMMER:  LORI J. QUINN
     H*  DATE:        JULY 25, 1991
     H*  CLIENT:      003
     H*  PROJECT:     SC030
     H*****************************************************************
     H*  REVISIONS:                                                   *
      *                                                                *********
      *  04/16/15  C:33  Project: 144WCJ
      *                  Only chain to files, if open                          *
      *                                                                *********
      *  06/09/11  C:061 Project: 133IKU
      *                  Add parms to return "Claims Adjuster" and     yer     *
      *                  "Claim Status".                               *********
      *                                                                *********
     H*  10/06/10  C:255 Project: 133WIZ
     H*                  Create a string algorithm to concatenate employer     *
     H*                  name,accident date and jurisdiction state.            *
     H*                                                                        *
     H*  12/10/07  C:33  Seq#: DR144/61418                                     *
     H*                  Change WTABL & WMEML to be User Controlled.           *
     H*                   Only open the file if WCMS is installed.             *
     H*                                                                        *
      * ** C:002                                                              **
      * ** DR214                                                              **
      * ** 10/24/06                                                           **
      *    Changes for idarts compatibility                                   **
      *** RETURN ADDITIONAL INFORMATION TO THE CALLER (2 79 CHR FLDS)     ******
     F*
     H*                                                               *
     H*   2/19/06  C:002  VDARTS                                      *
     H*                   CHANGE PROGRAM TO USE RETRN INSTEAD OF *INLR*
     H*                    ALSO, CHANGE SRNAME TO USE RETRN SRTERM='1'*
     H*                      ALSO, ONLY OPEN FILES ONCE )
     H*                                                               *
     H*                                                               *
     H*  11/08/04  C:062  Support: DR129/44505                        *
     H*                   Validate key                                *
     H*                                                               *
     H*  11/21/01  C:128  Support: OP109/29973                        *
     H*                   Inz all data structures that contain        *
     H*                   numeric data to prevent data decimal error. *
     H*                                                               *
     H*                                                               *
     H*  09/20/00  C:128  Support: MC091/24414                        *
     H*                   Added code to prevent opening files for     *
     H*                   applications that are no longer installed.  *
     H*                                                               *
     H*  03/20/00  C:15   Support: DR048/22271                        *
     H*                   Added code to prevent data-decimal error.   *
     H*                                                               *
     H*  10/22/99  C:53   PROJ: MC145
     H*                   QUALIFY WCMS FILE W/NON-PDS WCMS FLAG.        *
     H*
     H*  10/30/92  C:20   PROJ:                                       *
     H*                   1. CHANGES FILES TO BE USER CONTROLLED      *
     H*                   2. ADDED FS FILES                           *
     H*                   3. MOVE WC AND FS PROCESSING INTO SEPERATE  *
     H*                      SUBROUTINES.                             *
     H*                   4. CONDITIONED CALL OF SUBROUTINE ON FIRST  *
     H*                      LETTER OF ENTRY CALLING PROGRAM          *
     H*  05/11/93  C:12   PROJ:                                       *
     H*                   1.  INITIALIZE XZKEY,X1KEY WITH ZEROS NOT   *
     H*                       BLANKS, TO ALLEVIATE DATA DECIMAL ERRORS*
     H*****************************************************************
     H*
     F*
     F* WORK COMP CLAIM MASTER FILE
     FWMCML1    IF   E           K DISK    USROPN
     F*
     F* WORK COMP CLAIMANT DETAIL FILE
     FWMCDL     IF   E           K DISK    USROPN
     F*
     F*** BEGIN ADD *** 10/30/92 **************************************
     F* FS CLAIM MASTER FILE
     FFMCML1    IF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)
     F*
     F* FS CLAIMANT DETAIL FILE
     FFMCDL     IF   E           K DISK    USROPN
     F                                     RENAME(PCD01:PCD02)
     F*** END ADD *** 10/30/92 ****************************************
     F*** BEGIN ADD *** 02/10/94 **************************************
     FSSYLF     IF   E           K DISK
     FFMFNL1    IF   E           K DISK    USROPN
     F*** END ADD *** 02/10/94 ****************************************
      ********* beg add ******** 06/09/11 ***********************
      *
      * MC - CLAIM HISTORY
     FFMCHL3    IF   E           K DISK    USROPN
      *
      * SYSTEM USER PARAMETER
     FSMUPL1    IF   E           K DISK
      *
      ********* end add ******** 06/09/11 ***********************
     F** ADD 3/6/07 ***************************************************
     F***  Begin Chg  ***  12/10/07  **************************************
     F**********WMEML   IF  E           K        DISK
     FWMEML     IF   E           K DISK    USROPN
     F***  End   Chg  ***  12/10/07  **************************************
      *  Main Table Files
     F***  Begin Chg  ***  12/10/07  **************************************
     F**********WTABL   IF  E           K        DISK
     FWTABL     IF   E           K DISK    USROPN
     F***  End   Chg  ***  12/10/07  **************************************
      *________________________________________________________________
      *
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
      *           PNC01                             KIGNORE
      **********  PNA01                             KIGNORE
      *         NA01   -- Nature Codes
      *
      **********  PPA01                             KIGNORE
      *         PA01   -- Part Codes
      *
      **********  PCA01                             KIGNORE
      *         CA01   -- Cause Codes
      *
     F                                     IGNORE(PRC01)
      *
     F                                     IGNORE(PRT01)
      *         RT01   -- Reserve Type
      *
     F                                     IGNORE(PPC01)
      *         PC01   -- Payment Code
      *
     F                                     IGNORE(PB201)
      *         B201   -- Payment Value 1
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
      *________________________________________________________________

     D X#JS            C                   CONST('Jurst State:')
     D* DATA STRUCTURE FOR KEY FOR WORK COMP
     D                 DS                  INZ
     D  XZKEY                  1     79
     D*** BEGIN ADD *** 11/08/04 **************************************
     D  XZCKEY                 1     16
     D*** END   ADD *** 11/08/04 **************************************
     D  XZCO#                  1      3  0
     D  XZFND                  4      6  0
     D  XZFYR                  7      9  0
     D  XZCASE                10     16  0
     D*** BEGIN ADD *** 10/30/92 **************************************
     D*
     D* DATA STRUCTURE FOR KEY FOR FEE SCHEDULE
     D                 DS                  INZ
     D  X1KEY                  1     79
     D*** BEGIN ADD *** 11/08/04 **************************************
     D  X1CKEY                 1     21
     D*** END   ADD *** 11/08/04 **************************************
     D  X1CO#                  1      3  0
     D  X1SUB#                 4      8  0
     D  X1FND                  9     11  0
     D  X1FYR                 12     14  0
     D  X1CASE                15     21  0
     D*** END ADD *** 10/30/92 ****************************************
     D                 DS                  INZ
     D  X3KEY                  1     79
     D*** BEGIN ADD *** 11/08/04 **************************************
     D  X3CKEY                 1     16
     D*** END   ADD *** 11/08/04 **************************************
     D  X3CO#                  1      3  0
     D  X3FND                  4      6  0
     D  X3FYR                  7      9  0
     D  X3CASE                10     16  0
     D*** BEGIN ADD *** 02/10/94 **************************************
     D*
     D* DATA STRUCTURE FOR KEY FOR FEE SCHEDULE
     D*** END ADD *** 02/10/94 ****************************************
     D***  ADD 10/24/06 ****************************************************
      *
     D DATEDS          DS
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
     D*
    *D**** TEMP HERE ***************************
     D STUFF           C                   CONST('TESTING THE ABILTY O-
     D                                     F IDARTS TO DEAL WIT-
     D                                     H TEXT ENTRIES 2 AND-
     D                                     THREE. TEST TEST TES')
     D*
    *D**** END TEMP HERE ***************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I** END ADD 3/6/07 **********************************************
     I*
     IPCM02
     I              CMEMP#                      ZZEMP#
     I              CMDIV                       ZZDIV
     I              CMACDT                      ZZACDT
     I              CMDTRP                      ZZDTRP
     I              CMDTLW                      ZZDTLW
     I              CMDTRE                      ZZDTRE
     I              CMDT                        ZZDT
     I              CMENTD                      ZZENTD
     I              CMRCVD                      ZZRCVD
     I              CMMMI                       ZZMMI
     I              CMIDT                       ZZIDT
     I*** BEGIN ADD *** 03/09/98 **************************************
     I              CMOSCL                      ZZOSCL
     I*** END   ADD *** 03/09/98 **************************************
     IPCD02
     I              CDDTOB                      ZZDTOB
     I              CDDT                        ZZDT
     ****  END ADD 10/24/06 *******************************************
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C*** BEGIN ADD *** 10/30/92 **************************************
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C*** END ADD *** 10/30/29 ****************************************
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
      ********* beg add ******** 06/09/11 ***********************
      *
      *
      **********************************************************
      * KLISTS
      **********************************************************
      *
      * MC - Claim History File
      *
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
      ********* end add ******** 06/09/11 ***********************
     C*
     C* KEY LIST FOR THE WORK COMP CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** BEGIN ADD *** 10/30/92 **************************************
     C*
     C* KEY LIST FOR THE FEE SCHEDULE CLAIM MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** END ADD *** 10/30/92 ****************************************
     C*
     C* KEY LIST FOR THE CLAIMANT DETAIL FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*** BEGIN ADD *** 02/10/94 ***************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C* Xref to WCMS Fund # to get WCMC Sub#/Fund
     C*
     C     FNKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END ADD *** 02/10/94 *****************************************
     C**** ADD 3/6/07 ************************************
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *
     C*** END ADD *** 3/6/07*   ************************88
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C*** CHANGE 10/24/06 **************************************************
     C******* DEL ******   PARM           Q1PGM  10        PROGRAM NUMBER   BFR
     C                   PARM                    QQPGM            10            PROGRAM NUMBER   AFT
     C*** END CHANGE 10/24/06 **********************************************
     C                   PARM                    Q1KEY            79            DARTS' KEY
     C                   PARM                    Q1NAME           79            NAME
     I*** BEGIN ADD *** 10/24/06 **************************************
     C                   PARM                    Q1DTA1           79            DTA 1
     C                   PARM                    Q1DTA2           79            DTA 2
      *** END   ADD *** 10/24/06 **************************************
      ********* beg add ******** 06/09/11 *************************
     C                   PARM                    Q1ADJ            10            Claims Adjuster
     C                   PARM                    Q1OCST            1            Claim Status
      ********* end add ******** 06/09/11 *************************
     C*
     C* SET-UP AND INITIALIZE
     C*
      ********* beg add ******** 06/09/11 ***********************
      *
     C     Q1PRMS        IFGE      6
     C                   CLEAR                   Q1ADJ
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      7
     C                   CLEAR                   Q1OCST
     C                   ENDIF
      *
      ********* end add ******** 06/09/11 ***********************
     C                   MOVE      *BLANKS       XXPGM             1
     C***     DEL ***** 05/11/93 **************************C:12*********
     C*******              MOVE *BLANKS   XZKEY
     C                   MOVE      *BLANKS       Q1NAME
     C*** BEGIN ADD *** 10/30/92 **************************************
     C***     DEL ***** 05/11/93 **************************C:12*********
     C*******              MOVE *BLANKS   X1KEY
     C*** BEG ADD ***** 05/11/93 **************************C:12*********
     C                   MOVE      *ZEROS        X1KEY
     C                   MOVE      *ZEROS        XZKEY
     C                   MOVE      *ZEROS        X3KEY                          ADD 02/10/94
     C*** END ADD ***** 05/11/93 **************************C:12*********
      *
     C*** CHANGE 10/24/06 **************************************************
     C***** DEL ******     MOVELQ1PGM     XXPGM            BFR
     C                   MOVEL     QQPGM         XXPGM                          AFTER
     C*** END CHANGE 10/24/06 **********************************************
     C*
     C*** BEGIN ADD *** 03/20/00 **************************************
     C     Q1KEY         IFNE      *BLANKS
     C*** END ADD ***** 03/20/00 **************************************
     C*
     C*** BEG ADD *** 10/22/99 *************
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 10/22/99 *************
     C*
     C* CHECK THE CALLING PROGRAM
     C* IF 1ST CHARACTER OF CALLING PROGRAM IS NOT A 'W' OPEN FS FILES
     C* ELSE OPEN WC FILES AND EXECUTE APPLICABLE SUBROUTINE
     C*
B01  C     XXPGM         IFNE      'W'
     C*
     C*** BEGIN ADD *** 09/20/00 ***************************************
     C*
     C*  Installed applications API
     C*
     C                   MOVE      'FS'          X#APP             2
     C                   MOVE      *BLANK        X#RTN             1
     C                   CALL      'DA400R'
     C                   PARM                    X#APP
     C                   PARM                    X#RTN
     C     X#RTN         IFEQ      '0'                                          No App Inst
     C                   GOTO      SKIP
     C                   ENDIF
     C*** END ADD *** 09/20/00 *****************************************
     C*** ADD 2/19/06 *************************************************
     C     OPENF         IFEQ      *BLANKS
     C*** END ADD 2/19/06 *************************************************
     C                   OPEN      FMCML1
     C                   OPEN      FMCDL
     C                   OPEN      FMFNL1
      ********* beg add ******** 06/09/11 ***********************
     C                   OPEN      FMCHL3
      ********* end add ******** 06/09/11 ***********************
     C*** ADD 2/19/06 *************************************************
     C                   MOVE      '1'           OPENF             1
     C                   ENDIF
     C*** END ADD 2/19/06 *************************************************
     C                   EXSR      SRFS
     C*
X01  C                   ELSE
     C     ##WCIF        IFEQ      'Y'                                          ADD 10/22/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 10/22/99
     C*** ADD 2/19/06 *************************************************
     C     OPENW         IFEQ      *BLANKS
     C*** END ADD 2/19/06 *************************************************
     C                   OPEN      WMCML1
     C                   OPEN      WMCDL
     C*** ADD 2/19/06 *************************************************
     C***  Begin Add  ***  12/10/07  **************************************
     C                   OPEN      WMEML
     C                   OPEN      WTABL
     C***  End   Add  ***  12/10/07  **************************************
     C                   MOVE      '1'           OPENW             1
     C                   ENDIF
     C*** END ADD 2/19/06 *************************************************
     C                   EXSR      SRWC
     C                   ENDIF                                                  ADD 10/22/99
E01  C                   END
     C*** END ADD *** 10/30/92 ****************************************
     C*** BEGIN ADD *** 09/20/00 ***************************************
     C     SKIP          TAG
     C*** END   ADD *** 09/20/00 ***************************************
     C*** BEGIN ADD *** 03/20/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 03/20/00 **************************************
     C*
     C* END OF PROGRAM
     C****** ADD 02/19/06 *********************************************
     C*
     C                   RETURN
     C***END ADD 02/19/06 *********************************************
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 10/30/92 **************************************
     C*******************************************************************
     C* SRFS SUBROUTINE - PROCESS FEE SCHEDULE FILES                    *
     C*******************************************************************
     C     SRFS          BEGSR
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   DO
     C*** END   ADD *** 11/08/04 **************************************
     C*** BEGIN CHG *** 02/10/94 ****************************************
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
B01  C     *IN60         IFEQ      '0'
     C*
B02  C     FBFL01        IFEQ      'Y'
     C                   Z-ADD     4             X                 5 0
     C     1             SUBST     FBAL01:X      XDKEY             1    65
B03  C     XDKEY         IFEQ      'Y'
     C                   Z-ADD     17            X
     C     5             SUBST     Q1KEY:X       XL5POS            5    65
B04  C     XL5POS        IFEQ      *BLANKS
     C                   MOVEL     Q1KEY         X3KEY
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   TESTN                   X3CKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 11/08/04 **************************************
     C*
     C* CHAIN TO CLAIM MASTER FILE TO GET SS#
     C*
     C                   Z-ADD     X3CO#         XXCO#
     C                   Z-ADD     X3FND         XXFND
     C                   Z-ADD     X3FYR         XXFYR
     C                   Z-ADD     X3CASE        XXCASE
     C     FNKEY1        CHAIN     PFN01                              60
B05  C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
X05  C                   ELSE
     C                   Z-ADD     *ZEROS        XXSUB#
E05  C                   END                                                    END *IN60
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   LEAVE
     C                   ENDIF
     C*** END   ADD *** 11/08/04 **************************************
X04  C                   ELSE                                                   ELSE XL5POS
     C                   MOVEL     Q1KEY         X1KEY
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   TESTN                   X1CKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 11/08/04 **************************************
     C*
     C* CHAIN TO CLAIM MASTER FILE TO GET SS#
     C*
     C                   Z-ADD     X1CO#         XXCO#
     C                   Z-ADD     X1SUB#        XXSUB#
     C                   Z-ADD     X1FND         XXFND
     C                   Z-ADD     X1FYR         XXFYR
     C                   Z-ADD     X1CASE        XXCASE
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   LEAVE
     C                   ENDIF
     C*** END   ADD *** 11/08/04 **************************************
E04  C                   END                                                    END XL5POS
X03  C                   ELSE                                                   ELSE XDKEY
     C                   MOVEL     Q1KEY         X1KEY
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   TESTN                   X1CKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 11/08/04 **************************************
     C*
     C* CHAIN TO CLAIM MASTER FILE TO GET SS#
     C*
     C                   Z-ADD     X1CO#         XXCO#
     C                   Z-ADD     X1SUB#        XXSUB#
     C                   Z-ADD     X1FND         XXFND
     C                   Z-ADD     X1FYR         XXFYR
     C                   Z-ADD     X1CASE        XXCASE
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   LEAVE
     C                   ENDIF
     C*** END   ADD *** 11/08/04 **************************************
     C*
E03  C                   END                                                    END XDKEY
X02  C                   ELSE                                                   ELSE FBFL01
     C                   MOVEL     Q1KEY         X1KEY
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   TESTN                   X1CKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 11/08/04 **************************************
     C*
     C* CHAIN TO CLAIM MASTER FILE TO GET SS#
     C*
     C                   Z-ADD     X1CO#         XXCO#
     C                   Z-ADD     X1SUB#        XXSUB#
     C                   Z-ADD     X1FND         XXFND
     C                   Z-ADD     X1FYR         XXFYR
     C                   Z-ADD     X1CASE        XXCASE
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   LEAVE
     C                   ENDIF
     C*** END   ADD *** 11/08/04 **************************************
E02  C                   END                                                    END FBFL01
E01  C                   END                                                    END *IN60
     C*
     C*
     C     CMKY1         CHAIN     PCM02                              60
B01  C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVEL     CMSS#         XXSS#
      ********* beg add ******** 06/09/11 ***********************
      *
     C     Q1PRMS        IFGE      6
     C     CMCLAD        CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSR         Q1ADJ
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      7
     C     CHKY          CHAIN     PCH01                              60
     C  N60              MOVE      CHOCST        Q1OCST
     C                   ENDIF
      *
      ********* end add ******** 06/09/11 ***********************
     C*
     C* GET CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD02                              61
B02  C     *IN61         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C****** CHG 02/19/06 *********************************************
     C***** RMV *******    MOVEL' '       SRTERM  1
     C                   MOVEL     '1'           SRTERM            1
     C** END CHG 02/19/06 *********************************************
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C*
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVEL     SRLNAM        Q1NAME
     C*
E02  C                   END                                                    *IN61 END
E01  C                   END                                                    *IN60 END
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   ENDDO
     C*** END   ADD *** 11/08/04 **************************************
     C*** BEGIN ADD *** 10/24/06 **************************************
     C     Q1PRMS        IFGT      3
     C***  Begin Add  ***  04/16/15  **************************************
     C     ##WCIF        ANDEQ     'Y'                                          ADD 10/22/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 10/22/99
     C***  End   Add  ***  04/16/15  **************************************
     **
     C***  THE ADDITIONAL FIELDS FOR DISPLAY WERE REQUESTED (DTA1/DTA2)
     C***    THESE CAN BE FILLED WITH TEXT AND DATA FROM CLAIMS FILES INCLUDING
      ***    SS#, DOI:, EMP#, EMP NAME, ETC. ETC.  DEPENDING  ON NEED.
      ***    SIMPLY CONCAT THE APPROPRIATE TEXT AND DATA INTO DTA1 AND/OR
      ***    DTA2 AND IT WILL BE DISPLAYED AS PART OF THE HEADING FOR IDARTS
      ***
      ***
     C*** USE NATURE/PART/CAUSE DESCRIPTIONS FROM CLAIMS MASTER FOR LINE2
      *
      *  Accident description codes    DESC = 30A KEY=10    CODE=2
      *  NATURE
     C                   MOVEL     *BLANKS       Q1DTA1
     C                   MOVEL     *BLANKS       Q1DTA2
     **
     C                   MOVE      *BLANK        SCNATR            2
     C                   MOVEL(P)  CMNATR        SCNATR
     C                   MOVE      *BLANK        SCNADS           30
     C                   MOVEL(P)  CMNATR        XXKEY            10
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        SCNADS
     C                   END
      *  BODY PART
     C                   MOVE      *BLANK        SCPART            2
     C                   MOVEL(P)  CMPART        SCPART
     C                   MOVE      *BLANK        SCPADS           30
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        SCPADS
     C                   END
      *  CAUSE
     C                   MOVE      *BLANK        SCCAUS            2
     C                   MOVEL(P)  CMCAUS        SCCAUS
     C                   MOVE      *BLANK        SCCADS           30
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        SCCADS
     C                   END
     C***
     C***
     C     SCCADS        IFNE      *BLANKS
     C                   CAT(P)    SCCADS:1      Q1DTA2
     C                   CAT(P)    ' -':1        Q1DTA2
     C                   CAT(P)    ' '           Q1DTA2
     C                   ENDIF
     C***
     C     SCNADS        IFNE      *BLANKS
     C     Q1DTA2        ANDNE     *BLANKS
     C                   CAT(P)    SCNADS:1      Q1DTA2
     C                   CAT(P)    ' -':1        Q1DTA2
     C                   CAT(P)    ' '           Q1DTA2
     C                   ENDIF
     C***
     C     SCPADS        IFNE      *BLANKS
     C     Q1DTA2        ANDNE     *BLANKS
     C                   CAT(P)    SCPADS:1      Q1DTA2
     C                   ENDIF
     ****
     ****
     ******* JURISDICTION STATE = CMJRST
     ***
     *******
     ****
     C*** USE EMP#/NAME DOI: JURISDICTION STATE FOR LINE 3
      ** **
      ** EMPLOYER, DIV, EMP NAME
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C                   PARM                    XXEDN            80
      *
     C                   ENDIF
      *
      *  ACCIDENT DATE
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CMACDT        CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCACDT           10
     ****
     C*** BEGIN DEL *** 10/06/10 **************************************
     C*****      XXEDN     IFNE *BLANKS
     C*****                CAT  'Emp:':1  Q1DTA1    P
     C*****                CAT  XXEDN:1   Q1DTA1    P
     C*****                CAT  ' -':1    Q1DTA1    P
     C*****                CAT  ' '       Q1DTA1    P
     C*****                ENDIF
     C*****      SCACDT    IFNE *BLANKS
     C*****                CAT  'DOI:':3  Q1DTA1    P
     C*****                CAT  SCACDT:1  Q1DTA1    P
     C*****                CAT  ' -':1    Q1DTA1    P
     C*****                CAT  ' '       Q1DTA1    P
     C*****                ENDIF
     C*****      CMJRST    IFNE *BLANKS
     C*****                CAT  X#JS:3    Q1DTA1    P
     C*****                CAT  CMJRST:1  Q1DTA1    P
     C*****                ENDIF
     C*** END   DEL *** 10/06/10 **************************************
     C*
     C*** BEGIN ADD *** 10/06/10 **************************************
     C                   EXSR      BLDTA1
     C*** END   ADD *** 10/06/10 **************************************
      *
     C                   ENDIF
     C*** END   ADD *** 10/24/06 **************************************
     C                   ENDSR
     C*******************************************************************
     C* SRWC SUBROUTINE - PROCESS WORK COMP FILES                       *
     C*******************************************************************
     C     SRWC          BEGSR
     C*
     C                   MOVEL     Q1KEY         XZKEY
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   TESTN                   XZCKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 11/08/04 **************************************
     C*
     C* CHAIN TO CLAIM MASTER FILE TO GET SS#
     C*
     C                   Z-ADD     XZCO#         XXCO#
     C                   Z-ADD     XZFND         XXFND
     C                   Z-ADD     XZFYR         XXFYR
     C                   Z-ADD     XZCASE        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              60
B01  C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVEL     CMSS#         XXSS#
      ********* beg add ******** 06/09/11 ***********************
      *
     C     Q1PRMS        IFGE      6
     C     CMCLEX        CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSR         Q1ADJ
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      7
     C                   MOVEL     CMSTAT        Q1OCST
     C                   ENDIF
      *
      ********* end add ******** 06/09/11 ***********************
     C*
     C* GET CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              61
B02  C     *IN61         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C****** CHG 02/19/06 *********************************************
     C***** RMV *******    MOVEL' '       SRTERM
     C                   MOVEL     '1'           SRTERM            1
     C** END CHG 02/19/06 *********************************************
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C*
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVEL     SRLNAM        Q1NAME
     C*
E02  C                   END                                                    *IN61 END
E01  C                   END                                                    *IN60 END
     C*** BEGIN ADD *** 11/08/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   ENDIF
     C*** END   ADD *** 11/08/04 **************************************
     C*** BEGIN ADD *** 10/24/06 **************************************
     C     Q1PRMS        IFGT      3
     **
     C***  THE ADDITIONAL FIELDS FOR DISPLAY WERE REQUESTED (DTA1/DTA2)
     C***    THESE CAN BE FILLED WITH TEXT AND DATA FROM CLAIMS FILES INCLUDING
      ***    SS#, DOI:, EMP#, EMP NAME, ETC. ETC.  DEPENDING  ON NEED.
      ***    SIMPLY CONCAT THE APPROPRIATE TEXT AND DATA INTO DTA1 AND/OR
      ***    DTA2 AND IT WILL BE DISPLAYED AS PART OF THE HEADING FOR IDARTS
      ***
     C*** USE NATURE/PART/CAUSE DESCRIPTIONS FROM CLAIMS MASTER FOR LINE2
      *
      *  Accident description codes    DESC = 30A KEY=10    CODE=2
      *  NATURE
     C                   MOVEL     *BLANKS       Q1DTA1
     C                   MOVEL     *BLANKS       Q1DTA2
     **
     C                   MOVE      *BLANK        SCNATR            2
     C                   MOVEL(P)  CMNATR        SCNATR
     C                   MOVE      *BLANK        SCNADS           30
     C                   MOVEL(P)  CMNATR        XXKEY            10
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        SCNADS
     C                   END
      *  BODY PART
     C                   MOVE      *BLANK        SCPART            2
     C                   MOVEL(P)  CMPART        SCPART
     C                   MOVE      *BLANK        SCPADS           30
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        SCPADS
     C                   END
      *  CAUSE
     C                   MOVE      *BLANK        SCCAUS            2
     C                   MOVEL(P)  CMCAUS        SCCAUS
     C                   MOVE      *BLANK        SCCADS           30
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        SCCADS
     C                   END
     C***
     C***
     C     SCCADS        IFNE      *BLANKS
     C                   CAT(P)    SCCADS:1      Q1DTA2
     C                   CAT(P)    ' -':1        Q1DTA2
     C                   CAT(P)    ' '           Q1DTA2
     C                   ENDIF
     C***
     C     SCNADS        IFNE      *BLANKS
     C     Q1DTA2        ANDNE     *BLANKS
     C                   CAT(P)    SCNADS:1      Q1DTA2
     C                   CAT(P)    ' -':1        Q1DTA2
     C                   CAT(P)    ' '           Q1DTA2
     C                   ENDIF
     C***
     C     SCPADS        IFNE      *BLANKS
     C     Q1DTA2        ANDNE     *BLANKS
     C                   CAT(P)    SCPADS:1      Q1DTA2
     C                   ENDIF
     ****
     ****
     ******* JURISDICTION STATE = CMJRST
     ***
     *******
     ****
     C*** USE EMP#/NAME DOI: JURISDICTION STATE FOR LINE 3
      ** **
      ** EMPLOYER, DIV, EMP NAME
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C                   PARM                    XXEDN            80
      *
     C                   ENDIF
      *
      *  ACCIDENT DATE
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CMACDT        CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCACDT           10
     ****
     C*** BEGIN DEL *** 10/06/10 **************************************
     C*****      XXEDN     IFNE *BLANKS
     C*****                CAT  'Emp:':1  Q1DTA1    P
     C*****                CAT  XXEDN:1   Q1DTA1    P
     C*****                CAT  ' -':1    Q1DTA1    P
     C*****                CAT  ' '       Q1DTA1    P
     C*****                ENDIF
     C*****      SCACDT    IFNE *BLANKS
     C*****                CAT  'DOI:':3  Q1DTA1    P
     C*****                CAT  SCACDT:1  Q1DTA1    P
     C*****                CAT  ' -':1    Q1DTA1    P
     C*****                CAT  ' '       Q1DTA1    P
     C*****                ENDIF
     C*****      CMJRST    IFNE *BLANKS
     C*****                CAT  X#JS:3    Q1DTA1    P
     C*****                CAT  CMJRST:1  Q1DTA1    P
     C*****                ENDIF
     C*** END   DEL *** 10/06/10 **************************************
     C*
     C*** BEGIN ADD *** 10/06/10 **************************************
     C                   EXSR      BLDTA1
     C*** END   ADD *** 10/06/10 **************************************
      *
     C                   ENDIF
     C*** END   ADD *** 10/24/06 **************************************
     C                   ENDSR
     C*** END ADD *** 10/30/92 ****************************************
     C***  ADD *** 10/24/06 **************************************
      *
      * ===============================================================
      *   CONVERT 7,0 DATE TO 8,0 ;   0961213 --> 19961213
      *   THEN, CYMD TO MDCY      ;  19961213 --> 12/13/1996
      * ===============================================================
     C     SRCDAT        BEGSR
      *
     C     YMD           IFNE      *ZERO
     C     CC1           IFEQ      0
     C                   MOVE      '19'          CC1
     C                   ELSE
     C                   MOVE      '20'          CC1
     C                   END
      *
     C                   MOVE(P)   CY1           CY2
     C     MM            CAT(P)    '/':0         XXDT10           10
     C     XXDT10        CAT       DD:0          XXDT10
     C     XXDT10        CAT       '/':0         XXDT10
     C     XXDT10        CAT       CY1:0         XXDT10
     C                   END
     C                   ENDSR
     C***   END ADD *** 10/24/06 **************************************
     C*** BEGIN ADD *** 10/06/10 **************************************
      * ===============================================================
      * concatenate employer name, accident date and jurisdiction state
      * ===============================================================
     C     BLDTA1        BEGSR
     C                   MOVE      *BLANKS       Q1DTA1
     C                   MOVE      *ZEROS        USED              3 0
     C                   MOVE      *ZEROS        #FLDS             3 0
     C                   MOVE      *ZEROS        XXLEN             3 0
     C     XXEDN         IFNE      *BLANKS
     C     'Emp:'        CAT(P)    XXEDN:1       XXEDN
     C*FIND LENGTH
     C     ' '           CHECKR    XXEDN         XXLEN
     C*
     C                   ADD       1             #FLDS
     C                   ENDIF
     C     SCACDT        IFNE      *BLANKS
     C     'DOI:'        CAT(P)    SCACDT:1      DOI              15
     C                   ADD       15            USED
     C                   ADD       1             #FLDS
     C                   ENDIF
     C     CMJRST        IFNE      *BLANKS
     C     X#JS          CAT(P)    CMJRST:1      JRST             15
     C                   ADD       15            USED
     C                   ADD       1             #FLDS
     C                   ENDIF
     C*
     C     #FLDS         SUB       1             #FLDS
     C     #FLDS         MULT      5             @TMP              3 0
     C     @TMP          ADD       USED          USED
     C*TRUNCATE EMPLOYER IF NEEDED
     C                   MOVE      *ZEROS        @TMP
     C     XXLEN         ADD       USED          @TMP
     C     @TMP          IFGE      79
     C                   MOVE      *ZEROS        @TMP
     C     79            SUB       USED          @TMP
     C     @TMP          SUBST     XXEDN:1       @@EDN            80
     C                   MOVE      @@EDN         XXEDN
     C                   ENDIF
     C*CREATE DATA STRING
     C                   MOVE      *ZEROS        #FLDS
     C     XXEDN         IFNE      *BLANKS
     C     XXEDN         CAT(P)    Q1DTA1        Q1DTA1
     C                   Z-ADD     1             #FLDS
     C                   ENDIF
     C*
     C     SCACDT        IFNE      *BLANKS
     C     #FLDS         IFGE      1
     C                   CAT(P)    ' -':1        Q1DTA1
     C                   CAT(P)    ' ':1         Q1DTA1
     C                   CAT(P)    DOI:2         Q1DTA1
     C                   ENDIF
     C                   ENDIF
     C     CMJRST        IFNE      *BLANKS
     C     #FLDS         IFGE      1
     C                   CAT(P)    ' -':1        Q1DTA1
     C                   CAT(P)    ' ':1         Q1DTA1
     C                   CAT(P)    JRST:2        Q1DTA1
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*** END   ADD *** 10/06/10 **************************************
